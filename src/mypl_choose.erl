%% @version 0.2
%% @copyright 2007 HUDORA GmbH
%% @author Maximillian Dornseif <md@hudora.de>
%% @doc myPL/kernel-E Retrieval/Pick Functionality
%%
%% This module implements the actual way of selectiong goods for shippments. 
%% The only function meant to be called directly by the user is {@link find_provisioning_candidates/2}.
%%
%% The actual selection process is documented in {@link find_pick_candidates/3}
%% and {@link find_retrieval_candidates/2}.
%% @end

-module(mypl_choose).

-include_lib("stdlib/include/qlc.hrl").
-include("mypl.hrl").

-export([find_provisioning_candidates_multi/2,
         init_provisionings_multi/3]).


%% solange ein retrieval under RETRIEVALTOPICKVOLUME Liter Volumen hat, wird es in einen pick umgewandelt.
%% Siehe reorder_provisioning_candidates_multi() für Details.
-define(RETRIEVALTOPICKVOLUME, 200).


%%%%
%%%% main myPL API - retrieval & picks
%%%%

%% @doc
%% To be called within a transaction.
-spec find_pick_candidates_helper2(mypl_db:quantity(),[#unit{}]) -> 
    {'error','no_fit'} | {'fit',[{mypl_db:quantity(),mypl_db:muID()}]}.
find_pick_candidates_helper2(Quantity, Units) when is_integer(Quantity), is_list(Units), Quantity >= 0 ->            
    % check if we have enough stuff available to satisfy the request
    case lists:sum([X#unit.quantity - X#unit.pick_quantity || X <- Units]) of
        Q when Q < Quantity ->
            % No way to satisfy the request - to few goods
            % TODO: ask do get something down floorlevel: add_movementsuggestion({Quantity, Product}),
            {error, no_fit};
        Q when Q >= Quantity ->
            % we have enough available so just choose a nice combination
            % Preferences are: From the smallest unit to the largest unit (regardless of open picks)
            % this hopfully cleans out the warehouse as fast as possible
            Quantities = mypl_util:combine_until_fit(Quantity, 
                            lists:sort([X#unit.quantity - X#unit.pick_quantity || X <- Units,
                                        X#unit.quantity - X#unit.pick_quantity > 0])),
            % Quantities is now something along the lines of [{5,5},{6,6},{4,7}]
            % convert it to [{5,Mui1},{6,Mui2},{4,Mui3}]
            Muis = [X#unit.mui || X <- find_oldest_units_of([Y || {_, Y} <- Quantities], Units)],
            {fit, lists:zip([A || {A, _} <- Quantities], Muis)}
    end.

%% @private
%% @spec find_pick_candidates_helper1(Quantity::integer(), [mypl_db:unitRecord()]) -> 
%%     {ok, [{Quantity, mypl_db:unitRecord()}]}
%% @doc Finds the Units best suitable to pick Quantity
%%
%% Returns `{error, no_fit}' if nothing is found.
%% To be called within a transaction.
-spec find_pick_candidates_helper1(mypl_db:quantity(),[#unit{}]) ->
    {'error','no_fit'} | {'fit',[]|[{mypl_db:quantity(),mypl_db:muID()}]}.
find_pick_candidates_helper1(Quantity, Units) when is_integer(Quantity), is_list(Units), Quantity >= 0 ->
    % sort candidates - oldest and nearest to the front are picked first
    Sorted = mypl_db_util:sort_units_by_age_and_distance(?BESTLOCATION, Units),
    % prefer candidates already having open picks
    Candidates = [X || X <- Sorted, X#unit.pick_quantity > 0] ++ [X || X <- Sorted, X#unit.pick_quantity =< 0],
    % check for a direct fit
    case [X || X <- Candidates, X#unit.quantity - X#unit.pick_quantity >= Quantity] of
        [Unit|_] ->
            % TODO: instead of blindly taking the first one, check if we find a 100% fit 
            % so the MUI can be disbanded after picking - or is this ensured by the caller?
            {fit, [{Quantity, Unit#unit.mui}]};
        [] ->
            % nothing found to satisfy the quantity from a single unit we now go for combinations
            find_pick_candidates_helper2(Quantity, Candidates)
    end.
    


%% @spec find_pick_candidates(integer(), string(), [muiID()]) -> 
%%     {ok, [{Quantity, mypl_db:unitRecord()}]}
%% @doc Finds the Units best suitable to pick Quantity excluding certain MUIs
%%
%% We return only units which are at floorlevel and not moving. We prefer Units which already
%% have open picks for them. If we find a direct match resulting
%% in the disbandment of an unit we prefer to pick from that unit. Else we pick from the oldest Unit.
%% Returns `{error, no_fit}' if nothing is found.
-spec find_pick_candidates(Quantity::non_neg_integer(),Product::string(),[#unit{}]) ->
    {'error','no_fit'} | {'fit',[]|[{Quantity::non_neg_integer(),mypl_db:muID()}]}.
find_pick_candidates(Quantity, Product, Exclude) when is_integer(Quantity), Quantity >= 0 ->
    Fun = fun() ->
        % TODO: sind die nächsten Zeilen nciht doppelt gemoppelt?
        Candidates = (find_pickable_units(Product) -- Exclude),
        FilteredCandidates = [X || X <- Candidates, not(lists:member(X#unit.mui, Exclude))],
        case [X || X <- FilteredCandidates, X#unit.quantity - X#unit.pick_quantity =:= Quantity] of
            [H|_] ->
                % we found a 100% fit with a single unit ... done
                {fit, [{Quantity, H#unit.mui}]};
            _ -> 
                % go on searching for combinations
                find_pick_candidates_helper1(Quantity, FilteredCandidates)
        end
    end,
    mypl_db_util:transaction(Fun).
    

% @private
%% returns a list of all units which can be picked (no no_picks attribute on location
%% and not already fully allocated for other picks)
%% todo? - move to mypl_db_query?
-spec find_pickable_units(Product::nonempty_string()) -> []|[#unit{}].
find_pickable_units(Product) ->
    [X || X <- mnesia:match_object(#unit{product=Product, _='_'}),
          X#unit.quantity - X#unit.pick_quantity > 0,
          unit_pickable_helper(X)].
    

%% @private
%% TODO: shouldn't this return yes or no?
%% @doc Checks is a unit is pickable
%% expects to be called within a transaction
-spec unit_pickable_helper(#unit{}) -> bool().
unit_pickable_helper(Unit) ->
     Loc = mypl_db_util:get_mui_location(Unit#unit.mui),
     (not(lists:member({no_picks}, Loc#location.attributes))) 
      and (Loc#location.floorlevel =:= true)
      and (mypl_db_util:unit_moving(Unit) =:= no).
     

%% @doc finds the best retrieval candidates to exactly match Quantity.
%%
%% Returns `{error, no_fit}' if no suitable match is found.
%% WARNING: this function can take several seconds of computing to finish. It is definitively CPU-heavy.
-spec find_retrieval_candidates_helper(mypl_db:quantity(),[#unit{}]) ->
    {'error','no_fit'} | {'ok',[#unit{}]}.
find_retrieval_candidates_helper(Quantity, Units) when is_integer(Quantity), is_list(Units), Quantity >= 0 ->
    CandidateQuantites = mypl_util:nearest([X#unit.quantity || X <- Units], Quantity),
    if 
        CandidateQuantites /= [] ->
            % we are only interested in the first result
            {ok, find_oldest_units_of(CandidateQuantites, Units)};
        true ->
            % TODO: check if we have enough available goods in stock
            {error, no_fit}
    end.
    

%% @doc finds the best retrieval candidates to exactly match Quantity.
-spec find_retrieval_candidates(Quantity::integer(),Product::string(),{list()},[#unit{}]) -> 
    {'error','not_enough'} | {'ok',integer(),[#unit{}]}.
find_retrieval_candidates(Quantity, Product, {Props}, Units) when is_integer(Quantity), Quantity >= 0, is_list(Units) ->
    {{FullQuantity, AvailableQuantity, _, _}, _} = mypl_db_query:count_product(Product),
    if
        AvailableQuantity < Quantity ->
            % impossible to fullfill the request
            if
                FullQuantity < Quantity ->
                    % this really shouldn't happen. Something is deeply broken - or isn't it?
                    mypl_zwitscherserver:zwitscher("~s: Nicht genug Ware fuer Retrieval ~w mal ~s am Lager #error",
                                                   [proplists:get_value(kommiauftragnr, Props, "???????"),
                                                   Quantity, Product, FullQuantity]),
                    {error, not_enough};
                true ->
                    {error, not_enough}
            end;
        true ->
            case find_retrieval_candidates_helper(Quantity, Units) of
                {ok, NewUnits} ->
                    % find how much is left to be gathered by picks
                    Remainder = Quantity - lists:sum([X#unit.quantity || X <- NewUnits]),
                    {ok, Remainder, [X#unit.mui || X <- NewUnits]};
                {error, no_fit} ->
                    {ok, Quantity, []}
            end
    end.

%% @doc finds the best retrieval candidates to exactly match Quantity.
%%
%% If there is a 100% fit the remainder is 0. Else the quantity given in Remainder need to found
%% by other mens than retrieval, e.g. by Picks.
-spec find_retrieval_candidates(Quantity::non_neg_integer(),Product::string(),{list()}) -> 
    {'error','not_enough'} | {'ok',integer(),[#unit{}]}.
find_retrieval_candidates(Quantity, Product, {Props}) when is_integer(Quantity), Quantity >= 0 ->
    find_retrieval_candidates(Quantity, Product, {Props}, find_retrievable_units(Product)).


%% @private
%% @spec find_retrievable_units(string()) -> [mypl_db:unitRecord()]
%% @doc returns a list of all units for a product which can be retrieved.
%%
%% (no no_picks attribute on location and no open movements)
-spec find_retrievable_units(_) -> any().
find_retrievable_units(Product) ->
    Fun = fun() ->
        [X || X <- mypl_db_util:find_movable_units(Product), unit_pickable_helper(X)]
    end,
    mypl_db_util:transaction(Fun).
    

%% @private
-spec find_oldest_unit_of(Quantity::non_neg_integer(),[#unit{}],[#unit{}]) -> #unit{}.
find_oldest_unit_of(Quantity, Units, Ignore) when is_integer(Quantity), is_list(Units), is_list(Ignore) ->
    L = [X || X <- Units, (X#unit.quantity - X#unit.pick_quantity) =:= Quantity] -- Ignore,
    case lists:keysort(#unit.created_at, L) of
        % [] -> []; this should not happen
        [H|_] -> H
    end.
    

%% @private

-spec find_oldest_units_of([non_neg_integer()],[#unit{}],[#unit{}]) -> [#unit{}].
find_oldest_units_of([], _Units, _Ignore) -> [];
find_oldest_units_of(Quantities, Units, Ignore) when is_list(Quantities), is_list(Units), is_list(Ignore) ->
    [H|T] = Quantities,
    Unit = find_oldest_unit_of(H, Units, Ignore),
    [Unit|find_oldest_units_of(T, Units, [Unit|Ignore])].
    

%% @private
%% @doc select the oldest units matching certain quantities.
%%
%% for each Quantity in Quanitits the oldest Unit matching that Quantity in Units is returned.
-spec find_oldest_units_of([non_neg_integer(),...], [#unit{},...]) -> [#unit{}, ...].
find_oldest_units_of(Quantities, Units) when is_list(Quantities), is_list(Units) ->
    find_oldest_units_of(Quantities, Units, []).


%% @see find_retrieval_candidates/2
%% @see find_pick_candidates/3
%% @doc find a combination of retrievals and picks to fullfill a order
%%
%% By using find_retrieval_candidates/2 and find_pick_candidates/3 the best combination
%% to get a certain amound of goods out of the warehouse is analysed.
%% Returns {error, no_fit} or {ok, retrievals, picks}
-spec find_provisioning_candidates(non_neg_integer(),nonempty_string(),{list()})
    -> {ok, [mypl_db:muiID()], [{Quantiy::integer(), mypl_db:muiID()}]}.
find_provisioning_candidates(Quantity, Product, {Props}) ->
    Priority = proplists:get_value(priority, Props, "X"),
    % check for full MUIs which can be retrieved
    case find_retrieval_candidates(Quantity, Product, {Props}) of
        {ok, 0, Candidates} ->
            % we found a direct fit
            {ok, Candidates, []};
        {ok, Remainder, Candidates} ->
            % no direct match. So we need to come up with a mix of retrievals and picks
            % Rest is how much we have to get by picking
            % Candidates are excluded from consideration for picks
            case find_pick_candidates(Remainder, Product, Candidates) of
                {fit, Pickcandidates} ->
                    {ok, Candidates, Pickcandidates};
                {error, no_fit} ->
                    Fun = fun() ->
                        % we try just another thing: retrieval only from the upper levels:
                        NonFloorUnits = lists:filter(fun(X) -> Loc = mypl_db_util:get_mui_location(X#unit.mui), 
                                         Loc#location.floorlevel =:= false
                                       end, find_retrievable_units(Product)),
                        case find_retrieval_candidates(Quantity, Product, {Props}, NonFloorUnits) of
                            {ok, NRemainder, NCandidates} ->
                                case find_pick_candidates(NRemainder, Product, NCandidates) of
                                    {fit, NPickcandidates} ->
                                        {ok, NCandidates, NPickcandidates};
                                    {error, no_fit} ->
                                        mypl_requesttracker:in(Quantity, Product, Priority),
                                        {error, no_fit}
                                end;
                            {error, not_enough} ->
                                % Das sollte nicht vorkommen - der Unterbestand sollte schon beim ersten
                                % Aufruf von find_retrieval_candidates bemerkt werden.
                                mypl_log:log("Unterbestand bei ~w mal ~s (sollte echt nicht vorkommen)",
                                             [Quantity, Product], {[{level, debug}]}), 
                               {error, not_enough}
                        end
                    end,
                    mypl_db_util:transaction(Fun)
            end;
        {error, not_enough} ->
            {error, not_enough}
    end.
    

deduper_dictbuilder([], Dict) -> Dict;
deduper_dictbuilder([H|T], Dict) ->
    % disambiguate strange JSON
    case H of
        {Quantity, Product} -> ok;
        [Quantity, Product] -> ok
    end,
    if
        Quantity > 0 ->
            deduper_dictbuilder(T, dict:update_counter(Product, Quantity, Dict));
        true ->
            deduper_dictbuilder(T, Dict)
    end.
    
% @doc converts [{4,"10195"}, {0,"14695"}, {24,"66702"}, {180,"66702"}] to [{204,"66702"},{4,"10195"}]
% also converts list of lists into list of tuples
-spec deduper([{integer(),string()}]) -> [{integer(),string()}].
deduper(L) ->
    lists:map(fun({A, B}) -> {B, A};
                 ([A, B]) -> {B, A} end,
              dict:to_list(deduper_dictbuilder(L, dict:new()))).

%% @see find_provisioning_candidates/2
%% @doc calls {@link find_provisioning_candidates/2} for more than a single product.
%% Possibly Takes advantage of multi-processor system. Returns `{ok, [retrievals], [picks]}'
%% or `{error, no_fit}'.
%%
%% Example:
%% [{ok, [{6, Mui1a0010}], [{4, Mui2a0009}]}, ] = find_provisioning_candidates_multi([{4, "a0009"}, {6, "a0010"}])
%% @TODO: scheinbar wird diese funktion immer zweimal aufgerufen

-spec find_provisioning_candidates_multi([{Quantiy::integer(),Product::string()}], {list()}) -> 
    {'error','no_fit'} | {'ok',[mypl_db:muiID()],[{integer(), mypl_db:muiID()}]}.
find_provisioning_candidates_multi(L1, {Props}) ->
    % multipleoccurances of the same Article in L1 are aggregated into a single occurance
    L = deduper(L1),
    CandList = plists:map(fun({Quantity, Product}) ->
                              find_provisioning_candidates(Quantity, Product, {Props})
                          end, L),
    case lists:all(fun(Reply) -> element(1, Reply) =:= ok end, CandList) of
        false ->
            {error, no_fit};
        true ->
            Retrievalcandidates = lists:foldl(fun(X, Acc) -> 
                                                  lists:append(Acc, X) 
                                              end,
                                              [],
                                              [element(2, X) || X <- CandList]),
            Pickcandidates = lists:flatten([element(3, X) || X <- CandList]),
            reorder_provisioning_candidates_multi(Retrievalcandidates, Pickcandidates)
    end.
    
%% @doc this function allows policy decisions on the output of find_provisioning_candidates_multi
-spec reorder_provisioning_candidates_multi([mypl_db:muID()],_) -> {'ok',_,[any()]}.
reorder_provisioning_candidates_multi(Retrievalcandidates, Pickcandidates) ->
    case {Retrievalcandidates, Pickcandidates} of
        {_, []} ->
            % alle, die nur retrievals, sind unveraendert lassen
            {ok, Retrievalcandidates, Pickcandidates};
        {[], _} ->
            % alle, die nur picks sind, unveraendert lassen
            {ok, Retrievalcandidates, Pickcandidates};
        _ ->
            % check if all retrievals could also be reached by picks
            Pickable = mypl_db_util:transaction(fun() ->
                           [unit_pickable_helper(mypl_db_util:mui_to_unit(X)) || X <- Retrievalcandidates]
                                                end),
            case lists:all(fun(X) -> X end, Pickable) of
                false ->
                    % no, we can't reach them all on the floorlevel
                    {ok, Retrievalcandidates, Pickcandidates};
                true ->
                    % calculate volumes for each retrieval
                    Volumes = mypl_db_util:transaction(fun() ->
                                                           [get_mui_volume(X) || X <- Retrievalcandidates]
                                                       end),
                    case lists:all(fun(X) -> ((X < ?RETRIEVALTOPICKVOLUME) and (X > 0)) end, Volumes) of
                        false ->
                            % at least one retrieval has a volume bigger than ?RETRIEVALTOPICKVOLUME
                            {ok, Retrievalcandidates, Pickcandidates};
                        true ->
                            % all retrievals have a volume bleow ?RETRIEVALTOPICKVOLUME,
                            % change them to picks.
                            {ok, [], lists:merge(Pickcandidates,
                                                 mypl_db_util:transaction(fun() ->
                                                                 [get_mui_quantity(X) || X <- Retrievalcandidates]
                                                              end))}
                    end
            end
    end.
    

-spec get_mui_volume(mypl_db:muID()) -> number().
get_mui_volume(Mui) ->
    Unit = mypl_db_util:mui_to_unit(Mui),
    mypl_volumes:volume({Unit#unit.quantity, Unit#unit.product}).

%% @doc returns {quantity, mui} for a mui
-spec get_mui_quantity(mypl_db:muID()) -> {Quantity::non_neg_integer(), mypl_db:muID()}.
get_mui_quantity(Mui) ->
    Unit = mypl_db_util:mui_to_unit(Mui),
    {Unit#unit.quantity, Mui}.


%% @see find_provisioning_candidates/2
%% @doc calls {@link find_provisioning_candidates/2} for more than a single product.
%% Takes advantage of multi-processor system.
%%
%% Example:
%% {ok, [MovementID1, MovementID2], [PickId3]} = init_provisionings_multi([{9, "a0009"}, {6, "a0010"}])

-spec init_provisionings_multi([{Quantiy::integer(),Product::string()}],{list()},{list()}) -> 
    {error, no_fit} | {'ok',[mypl_db:movementID()], [mypl_db:pickID()]}.

%% Attributs sind die Attribute, die in die neuen Bewegungen gepackt werden sollen.
%% Props sind Informationen zur Vorgangsbearbeitung
init_provisionings_multi(L, {Attributes}, {Props}) ->
    case find_provisioning_candidates_multi(L, {Props}) of
        {error, no_fit} ->
            {error, no_fit};
        {ok, Retrievals, Picks} ->
            % generate provisionings and picks - we use a transaction to ensure either all succedd or all fail
            Fun = fun() ->
                {ok, lists:map(fun(Mui) -> 
                                   {ok, MovementId} = mypl_db:init_movement(Mui, "AUSLAG", 
                                                          [{kernel_type, retrieval}] ++ Attributes),
                                   MovementId
                               end, Retrievals),
                     lists:map(fun({Quantity, Mui}) -> 
                                  {ok, PickId} = mypl_db:init_pick(Quantity, Mui, Attributes),
                                  PickId
                               end, Picks)}
            end,
            {atomic, Ret} = mnesia:transaction(Fun),
            Ret
    end.
    

% ~~ Unit tests
-ifdef(EUNIT).
-compile(export_all).

%%% @hidden
test_init() ->
    mypl_db:run_me_once(),
    % flush database
    mnesia:start(),
    mnesia:clear_table(unit),
    mnesia:clear_table(location),   
    mnesia:clear_table(movement),   
    mnesia:clear_table(pick),       
    mnesia:clear_table(picklist),   
    mnesia:clear_table(articleaudit),
    mnesia:clear_table(unitaudit),  
    % regenerate locations
    % init_location(Name, Height, Floorlevel, Preference, Attributes)
    mypl_db:init_location("EINLAG", 6000, true,  0, [{no_picks}]),
    mypl_db:init_location("AUSLAG", 6000, true,  0, [{no_picks}]),
    mypl_db:init_location("010101", 2000, true,  6, []),
    mypl_db:init_location("010102", 1950, false, 6, []),
    mypl_db:init_location("010103", 1200, false, 5, []),
    mypl_db:init_location("010201", 2000, true,  7, []),
    mypl_db:init_location("010202", 2000, false,  7, []),
    mypl_db:init_location("010203", 2000, false,  7, []),
    mypl_db:init_location("010301", 2000, true,   7, []),
    mypl_db:init_location("010302", 2000, false,  7, []),
    mypl_db:init_location("010303", 2000, false,  7, []),
    mypl_db:init_location("010401", 2000, true,   7, []),
    mypl_db:init_location("010402", 2000, false,  7, []),
    mypl_db:init_location("010403", 2000, false,  7, []),
    ok.

%%% @hidden
mypl_simple_picking1_test() ->
    test_init(),
    Mui1 = "MUI1-" ++ mypl_util:generate_mui(),
    Mui2 = "MUI2-" ++ mypl_util:generate_mui(),
    Mui3 = "MUI3-" ++ mypl_util:generate_mui(),
    Mui4 = "MUI4-" ++ mypl_util:generate_mui(),
    Mui6 = "MUI6-" ++ mypl_util:generate_mui(),
    {ok, _} = mypl_db:store_at_location("EINLAG", Mui1,  5, "a0003", 1200),
    {ok, _} = mypl_db:store_at_location("010101", Mui2,  5, "a0003", 1200),
    {ok, _} = mypl_db:store_at_location("EINLAG", Mui3, 17, "a0004", 1200),
    {ok, _} = mypl_db:store_at_location("010102", Mui4, 19, "a0004", 1200),
    {ok, _} = mypl_db:store_at_location("EINLAG", mypl_util:generate_mui(), 23, "a0005", 1200),
    {ok, _} = mypl_db:store_at_location("010103", Mui6, 71, "a0005", 1200),
    {error,no_fit} = find_pick_candidates(6, "a0004", []), % nothing available at floor level
    
    {fit, [{4, Mui2}]} = find_pick_candidates(4, "a0003", []), % Mui1 on EINLAG is older but no_picks
    {ok, Movement1} = mypl_db:init_movement(Mui1, "010201"),
    mypl_db:commit_movement(Movement1),
    {fit, [{4, Mui1}]} = find_pick_candidates(4, "a0003", []), % Now Mui1 is not on EINLAG anymore
    
    {ok, Movement2} = mypl_db:init_movement(Mui1, "010301"),
    {fit, [{4, Mui2}]} = find_pick_candidates(4, "a0003", []), % Mui1 is moving so it can't be picked
    mypl_db:commit_movement(Movement2),
    
    {fit, [{4, Mui1}]} = find_pick_candidates(4, "a0003", []), % Now Mui1 can be picked again
    
    {ok, Movement3} = mypl_db:init_movement(Mui1, "010302"),
    mypl_db:commit_movement(Movement3),
    {fit, [{4, Mui2}]} = find_pick_candidates(4, "a0003", []), % Mui1 is not floor level anymore so it can't be picked
    ok.

%%% @hidden
mypl_simple_picking2_test() ->
    test_init(),
    Mui1 = "Mui1-" ++ mypl_util:generate_mui(),
    Mui2 = "Mui2-" ++ mypl_util:generate_mui(),
    Mui3 = "Mui3-" ++ mypl_util:generate_mui(),
    {ok, _} = mypl_db:store_at_location("010101", Mui1,  5, "a0006", 1200),
    {ok, _} = mypl_db:store_at_location("010101", Mui2,  7, "a0006", 1200),
    {ok, _} = mypl_db:store_at_location("010101", Mui3, 11, "a0006", 1200),
    {fit, [{5, Mui1}]} = find_pick_candidates(5, "a0006", []),
    {fit, [{6, Mui2}]} = find_pick_candidates(6, "a0006", []),
    {fit, [{7, Mui2}]} = find_pick_candidates(7, "a0006", []),
    {fit, [{8, Mui3}]} = find_pick_candidates(8, "a0006", []),
    {fit, [{11, Mui3}]} = find_pick_candidates(11, "a0006", []),
    
    % exclusion of MUIs works
    {fit, [{1, Mui1}]} = find_pick_candidates(1, "a0006", []),
    {fit, [{1, Mui2}]} = find_pick_candidates(1, "a0006", [Mui1]),
    
    % the next should be fulfilled by "picking empty" two units
    {fit, [{5, Mui1}, {7, Mui2}]} = find_pick_candidates(12, "a0006", []),
    % we can't fullfill this pick, but we can get close - do we actualy want this?
    {error, no_fit} = find_pick_candidates(25, "a0006", []),
    
    %% now let's see if units with open picks are preferred
    {ok, Pick1} = mypl_db:init_pick(7, Mui3),
    {fit, [{4, Mui3}]} = find_pick_candidates(4, "a0006", []),
    %% not enough on Mui3, so wee need to pick from the others
    {fit, [{5, Mui1}]} = find_pick_candidates(5, "a0006", []),
    {fit, [{6, Mui2}]} = find_pick_candidates(6, "a0006", []),
    {fit,[{4, Mui3}, {4, Mui1}]} = find_pick_candidates(8, "a0006", []),
    mypl_db:rollback_pick(Pick1),
    ok.
    

%%% @hidden
mypl_simple_picking3_test() ->
    test_init(),
    Mui1 = "Mui1-" ++ mypl_util:generate_mui(),
    Mui2 = "Mui2-" ++ mypl_util:generate_mui(),
    Mui3 = "Mui3-" ++ mypl_util:generate_mui(),
    {ok, _} = mypl_db:store_at_location("010101", Mui1, 11, "a0007", 1200),
    {ok, _} = mypl_db:store_at_location("010101", Mui2,  7, "a0007", 1200),
    {ok, _} = mypl_db:store_at_location("010101", Mui3,  5, "a0007", 1200),
    % If we can use the pick to exactly empty an unit we prefer to do that instead of picking
    % from the oldest unit
    {fit, [{5, Mui3}]} = find_pick_candidates(5, "a0007", []),
    % and what about two picks
    {fit, [{5, Mui3}, {7, Mui2}]} = find_pick_candidates(12, "a0007", []),
    ok.
    

%%% @hidden
mypl_simple_retrieval_test() ->
    test_init(),
    Mui1 = "Mui1-" ++ mypl_util:generate_mui(),
    Mui2 = "Mui2-" ++ mypl_util:generate_mui(),
    Mui3 = "Mui3-" ++ mypl_util:generate_mui(),
    {ok, _} = mypl_db:store_at_location("010101", Mui1, 11, "a0008", 1200),
    {ok, _} = mypl_db:store_at_location("010101", Mui2,  7, "a0008", 1200),
    {ok, _} = mypl_db:store_at_location("010101", Mui3,  5, "a0008", 1200),
    {ok, 0, [Mui1]} = find_retrieval_candidates(11, "a0008"),
    {ok, 0, [Mui2]} = find_retrieval_candidates(7, "a0008"),
    {ok, 0, [Mui2, Mui1]} = find_retrieval_candidates(18, "a0008"),
    {ok, 0, [Mui3, Mui2, Mui1]} = find_retrieval_candidates(23, "a0008"),
    {error, not_enough} = find_retrieval_candidates(25, "a0008"),
    {ok, 1, [Mui3, Mui1]} = find_retrieval_candidates(17, "a0008"),
    {ok, 3, [Mui3, Mui2]} = find_retrieval_candidates(15, "a0008"),
    {ok, 2, [Mui3, Mui2]} = find_retrieval_candidates(14, "a0008"),
    {ok, 4, []} = find_retrieval_candidates(4, "a0008"),
    ok.
    

%%% @hidden
mypl_simple_provisioning1_test() ->
    % all MUIs floorlevel
    test_init(),
    Mui1 = "Mui1-" ++ mypl_util:generate_mui(),
    Mui2 = "Mui2-" ++ mypl_util:generate_mui(),
    Mui3 = "Mui3-" ++ mypl_util:generate_mui(),
    {ok, _} = mypl_db:store_at_location("010101", Mui1, 11, "a0009", 1200),
    {ok, _} = mypl_db:store_at_location("010101", Mui2,  7, "a0009", 1200),
    {ok, _} = mypl_db:store_at_location("010101", Mui3,  5, "a0009", 1200),
    % check some random combinations
    {ok, [Mui3], []} = find_provisioning_candidates(5, "a0009"),
    {ok, [], [{4, Mui1}]} = find_provisioning_candidates(4, "a0009"),
    {ok, [Mui3], [{1, Mui1}]} = find_provisioning_candidates(6, "a0009"),
    {ok, [Mui2], [{3, Mui1}]} = find_provisioning_candidates(10, "a0009"),
    {ok, [Mui1], []} = find_provisioning_candidates(11, "a0009"),
    {ok, [Mui3, Mui2], []} = find_provisioning_candidates(12, "a0009"),
    {ok, [Mui3, Mui2], [{1, Mui1}]} = find_provisioning_candidates(13, "a0009"),
    
    % The following tests check that picks will not be initiated on units also marked for retrieval
    % without this functionality the result would be {ok, [Mui2, Mui1], [{4, Mui1}]}
    % but we want {ok, [Mui2, Mui1], [{4, Mui3}]}
    {ok, [Mui2, Mui1], [{4, Mui3}]} = find_provisioning_candidates(22, "a0009"),
    {ok, [Mui3, Mui2, Mui1], []} = find_provisioning_candidates(23, "a0009"),
    {error, not_enough} = find_provisioning_candidates(24, "a0009"),
    ok.
    

%%% @hidden
mypl_simple_provisioning2_test() ->
    % NOT all MUIs floorlevel
    test_init(),
    Mui1 = "Mui1-" ++ mypl_util:generate_mui(),
    Mui2 = "Mui2-" ++ mypl_util:generate_mui(),
    Mui3 = "Mui3-" ++ mypl_util:generate_mui(),
    Mui4 = "Mui4-" ++ mypl_util:generate_mui(),
    {ok, _} = mypl_db:store_at_location("010101", Mui1, 11, "a0009", 1200),
    {ok, _} = mypl_db:store_at_location("010102", Mui2,  7, "a0009", 1200),
    {ok, _} = mypl_db:store_at_location("010103", Mui3,  5, "a0009", 1200),
    {ok, _} = mypl_db:store_at_location("010201", Mui4,  6, "a0010", 1200),
    
    % check random combinations
    {ok, [Mui3], []} = find_provisioning_candidates(5, "a0009", "X"),
    {ok, [], [{4, Mui1}]} = find_provisioning_candidates(4, "a0009", "X"),
    {ok, [Mui3], [{1, Mui1}]} = find_provisioning_candidates(6, "a0009", "X"),
    {ok, [Mui2], [{3, Mui1}]} = find_provisioning_candidates(10, "a0009", "X"),
    {ok, [Mui1], []} = find_provisioning_candidates(11, "a0009", "X"),
    {ok, [Mui3, Mui2], []} = find_provisioning_candidates(12, "a0009", "X"),
    {ok, [Mui3, Mui2], [{1, Mui1}]} = find_provisioning_candidates(13, "a0009", "X"),
    
    % TODO: there are actual fits for that pick, our code is just not smart enough to find them
    % on the first run - so there is some extremely obscure code at work to find the picks
    {ok, [Mui3, Mui2], [{10, Mui1}]} = find_provisioning_candidates(22, "a0009", "X"),
    %                
    {ok, [Mui3, Mui2, Mui1], []} = find_provisioning_candidates(23, "a0009", "X"),
    {error, not_enough} = find_provisioning_candidates(24, "a0009", "X"),
    {ok, [Mui4], [{4, Mui1}]} = find_provisioning_candidates_multi([{4, "a0009"}, {6, "a0010"}], "X"),
    {ok, [Mui4, Mui2], [{2, Mui1}]} = find_provisioning_candidates_multi([{9, "a0009"}, {6, "a0010"}], "X"),
    ok.
    
real_world1_test() ->
    test_init(),
    ?assertMatch({ok, _}, mypl_db:store_at_location("010103", "mui1", 48, "42236", 1950)), 
    ?assertMatch({ok, _}, mypl_db:store_at_location("010102", "mui2", 48, "42236", 1950)), 
    ?assertMatch({ok, _}, mypl_db:store_at_location("010202", "mui3", 48, "42236", 1950)), 
    ?assertMatch({ok, _}, mypl_db:store_at_location("010301", "mui4", 48, "42236", 1950)), 
    % {ok, _} = mypl_db:store_at_location("010203", mui5, 48, "42236", 1950), 
    % {ok, _} = mypl_db:store_at_location("010303", mui6, 48, "42236", 1950), 
    % {ok, _} = mypl_db:store_at_location("010401", mui7, 48, "42236", 1950), 
    % {ok, _} = mypl_db:store_at_location("010302", mui8, 48, "42236", 1950), 
    % {ok, _} = mypl_db:store_at_location("010403", mui9, 48, "42236", 1950), 
    % {ok, _} = mypl_db:store_at_location("010402", mui0, 48, "42236", 1950), 
    ?assertMatch({ok, _}, mypl_db:store_at_location("010101", "muia",  2, "42236", 1950)), 
    ?assertMatch({ok, _}, mypl_db:store_at_location("010201", "muib", 48, "42236", 1950)), 
    % reihenfolge: kleinste, aelteste
    ?assertMatch({ok, 2, ["muia", "mui1", "mui2"]}, find_retrieval_candidates(100, "42236")),
    ?assertMatch({ok, ["muia", "mui1", "mui2"], [{2, "mui4"}]}, find_provisioning_candidates(100, "42236")),
    ?assertMatch({fit,[{2,"muia"},{48,"mui4"}]}, find_pick_candidates(50, "42236", [muia, mui1])),
    ?assertMatch({ok, ["muia", "mui1", "mui2"],[{2, "mui4"}]}, find_provisioning_candidates(100, "42236")),
    ?assertMatch({ok, ["muia", "mui1", "mui2"],[{2, "mui4"}]}, find_provisioning_candidates_multi([[100, "42236"]])),
    ok.

real_world2_test() ->
   test_init(),
   % Beispiel, wo kein ergebnis gefunden wird/wurde, obwohl dies im grunde moeglich waere
   {ok, _} = mypl_db:store_at_location("010102", "mui1", 56, "14890/01", 1950),
   {ok, _} = mypl_db:store_at_location("010103", "mui2", 56, "14890/01", 1950),
   %{ok, _} = mypl_db:store_at_location("010202", mui3, 56, "14890/01", 1950),
   %{ok, _} = mypl_db:store_at_location("010203", mui4, 56, "14890/01", 1950),
   %{ok, _} = mypl_db:store_at_location("010302", mui5, 56, "14890/01", 1950),
   %{ok, _} = mypl_db:store_at_location("010303", mui6, 56, "14890/01", 1950),
   %{ok, _} = mypl_db:store_at_location("010402", mui7, 56, "14890/01", 1950),
   {ok, _} = mypl_db:store_at_location("010403", "mui8", 56, "14890/01", 1950),
   {ok, _} = mypl_db:store_at_location("010101", "mui9", 62, "14890/01", 1950),
   ?assertMatch({ok, ["mui1"], [{44, "mui9"}]}, mypl_choose:find_provisioning_candidates(100,"14890/01")),
   ok.

real_world3_test() ->
    test_init(),
    % dieser code fuehrte zu problemen, weil 66702 zwei mal im Auftrag vorkam.
    % obendrein wird init_provisionings_multi getestet
    {ok, _} = mypl_db:store_at_location("010203", "mui1", 32, "10195", 1950),
    {ok, _} = mypl_db:store_at_location("010301", "mui2", 32, "10195", 1950),
    {ok, _} = mypl_db:store_at_location("010202", "mui3", 32, "10195", 1950),
    {ok, _} = mypl_db:store_at_location("010201", "mui4", 31, "10195", 1950),
    {ok, _} = mypl_db:store_at_location("010202", "mui5",  7, "14695", 1950),
    {ok, _} = mypl_db:store_at_location("010203", "mui6", 36, "14695", 1950),
    {ok, _} = mypl_db:store_at_location("010302", "mui7", 36, "14695", 1950),
    {ok, _} = mypl_db:store_at_location("010303", "mui8", 36, "14695", 1950),
    {ok, _} = mypl_db:store_at_location("010402", "mui9", 36, "14695", 1950),
    {ok, _} = mypl_db:store_at_location("010403", "muia", 36, "14695", 1950),
    {ok, _} = mypl_db:store_at_location("010102", "muib", 36, "14695", 1950),
    {ok, _} = mypl_db:store_at_location("010103", "muic", 36, "14695", 1950),
    %{ok, _} = mypl_db:store_at_location("151603", muid, 36, "14695", 1950),
    %{ok, _} = mypl_db:store_at_location("152803", muie, 36, "14695", 1950),
    ?assertMatch({ok, _}, mypl_db:store_at_location("010101", "muif",250, "66702", 1950)),
    ?assertMatch({ok,[],[{4, "mui2"}]}, mypl_choose:find_provisioning_candidates(4, "10195")),
    ?assertMatch({error, no_fit}, mypl_choose:find_provisioning_candidates(18, "14695")),
    ?assertMatch({ok,[],[{24, "muif"}]}, mypl_choose:find_provisioning_candidates(24, "66702")),
    ?assertMatch({ok,[],[{100, "muif"}]}, mypl_choose:find_provisioning_candidates(100,"66702")),
    % find_provisioning_candidates_multi takes list-of-lists and list-of-tuples as an argument to satisfy JSON
    ?assertMatch({error, no_fit}, find_provisioning_candidates_multi([[4,"10195"], [18,"14695"], [24,"66702"], [180,"66702"]])),
    ?assertMatch({error, no_fit}, find_provisioning_candidates_multi([{4,"10195"}, {18,"14695"}, {24,"66702"}, {180,"66702"}])),
    % duplicate articles (66702) are aggregated into a single pick
    ?assertMatch({ok,[], [{204, "muif"}, {4, "mui2"}]}, find_provisioning_candidates_multi([{4,"10195"}, {24,"66702"}, {180,"66702"}])),
    
    % quantity 0 products are ignored
    ?assertMatch({ok,[],[{204, "muif"},{4, "mui2"}]}, find_provisioning_candidates_multi([{4,"10195"}, {0,"14695"}, {24,"66702"}, {180,"66702"}])),
    ?assertMatch({error,no_fit}, mypl_choose:init_provisionings_multi([[4,"10195"], [18,"14695"], [24,"66702"], [180,"66702"]])),
    {ok,[],[Pick1, Pick2]} = mypl_choose:init_provisionings_multi([[4,"10195"], [24,"66702"], [180,"66702"]]),
    ?assertMatch({ok, {204, "66702"}}, mypl_db:commit_pick(Pick1)),
    ?assertMatch({ok, {4, "10195"}}, mypl_db:commit_pick(Pick2)),
    ok.

%%% @hidden
testrunner() ->
    mypl_simple_picking1_test(),
    mypl_simple_picking2_test(),
    mypl_simple_picking3_test(),
    mypl_simple_retrieval_test(),
    mypl_simple_provisioning1_test(),
    mypl_simple_provisioning2_test(),
    real_world1_test(),
    real_world2_test(),
    real_world3_test(),
    ok.
    

-endif.
