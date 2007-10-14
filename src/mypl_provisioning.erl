%% @version 0.1
%% @copyright 2007 HUDORA GmbH
%% @author Maximillian Dornseif <md@hudora.de>
%% @doc myPL/kernel-E Retrieval/Pick Functionality
%%
%% This module implements the actual way of selectiong goods for shippments. 
%% The only function meant to be called directly by the user is {@link find_provisioning_candidates/2}.
%%
%% The actual selection process is documented in {@link find_pick_candidates/2}
%% and {@link find_retrieval_candidates/2}.
%% @end

-module(mypl_provisioning).

-include_lib("stdlib/include/qlc.hrl").
-include("include/mypl.hrl").

-export([find_pick_candidates/2, find_provisioning_candidates/2]).


%%%%
%%%% main myPL API - retrieval & picks
%%%%

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
                                                     lists:sort([X#unit.quantity 
                                                                 - X#unit.pick_quantity || X <- Units])),
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
find_pick_candidates_helper1(Quantity, Units) when is_integer(Quantity), is_list(Units), Quantity >= 0 ->
    % sort candidates - oldes are picked first
    Sorted = lists:keysort(#unit.created_at, Units),
    % prefer candidates already having open picks
    Candidates = [X || X <- Sorted, X#unit.pick_quantity > 0] ++ [X || X <- Sorted, X#unit.pick_quantity =< 0],
    % check for a direct fit
    case [X || X <- Candidates, X#unit.quantity - X#unit.pick_quantity >= Quantity] of
        [Unit|_] ->
            % TODO: instead of blindly taking the first one, check if we find a 100% fit so the MUI can be disbanded after picking
            {fit, [{Quantity, Unit#unit.mui}]};
        [] ->
            % nothing found to satisfy the quantity from a single unit we now go for combinations
            find_pick_candidates_helper2(Quantity, Candidates)
    end.
    


%% @spec find_pick_candidates(integer(), string(), [muiID()]) -> 
%%     {ok, [{Quantity, mypl_db:unitRecord()}]}
%% @doc Finds the Units best suitable to pick Quantity excluding certain MUIs
%% @see find_pick_candidates/2
find_pick_candidates(Quantity, Product, Exclude) when is_integer(Quantity), Quantity >= 0 ->
    % Candidates = mypl_db_util:do(qlc:q([X || X <- find_pickable_units(Product),
    %                            X#unit.quantity - X#unit.pick_quantity >= Quantity])),
    Candidates = find_pickable_units(Product),
    
    FilteredCandidates = [X || X <- Candidates, not(lists:member(X#unit.mui, Exclude))],
    % we only pick from floorlevel
    FloorCandidates = lists:filter(fun(X) -> Loc = mypl_db_util:get_mui_location(X#unit.mui), 
                                             Loc#location.floorlevel =:= true
                                   end, FilteredCandidates),
    case [X || X <- Candidates, X#unit.quantity - X#unit.pick_quantity =:= Quantity] of
        [H|_] ->
            % we found a 100% fit with a single unit ... done
            {fit, [{Quantity, H#unit.mui}]};
        _ -> 
            % go on searching for combinations
            find_pick_candidates_helper1(Quantity, FloorCandidates)
    end.
    

%% @spec find_pick_candidates(integer(), string()) -> 
%%     {ok, [{Quantity, mypl_db:unitRecord()}]}
%% @doc Finds the Units best suitable to pick Quantity
%%
%% We return only units which are at floorlevel and not moving. We prefer Units which already
%% have open picks for them. If we find a direct match resulting
%% in the disbandment of an unit we prefer to pick from that unit. Else we pick from the oldest Unit.
%% Returns `{error, no_fit}' if nothing is found.
find_pick_candidates(Quantity, Product) when is_integer(Quantity), Quantity >= 0 ->
    find_pick_candidates(Quantity, Product, []).
    

% @private
%% returns a list of all units which can be picked (no no_picks attribute on location)
%% todo? - move to mypl_db_query?
find_pickable_units(Product) ->
    [X || X <- mypl_db_util:do(qlc:q([X || X <- mnesia:table(unit), X#unit.product =:= Product])), unit_pickable_helper(X)].
    
%% @private
%% TODO: shouldn't this return yes or no?
unit_pickable_helper(Unit) ->
     Loc = mypl_db_util:get_mui_location(Unit#unit.mui),
     (not(lists:member(no_picks, Loc#location.attributes))) and (mypl_db_util:unit_moving(Unit) =:= no).
     

%% @spec find_retrieval_candidates_helper(Quantity::integer(), [mypl_db:muID()]) -> {ok, [mypl_db:unitRecord()]}
%% @doc finds the best retrieval candidates to exactly match Quantity.
%%
%% Returns `{error, no_fit}' if no suitable match is found.
%% WARNING: this function can take several seconds of computing to finish. It is definitively CPU-heavy.
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
    

%% @spec find_retrieval_candidates(Quantity::integer(), string) -> {ok, Remainder, [mypl_db:unitRecord()]}
%% @doc finds the best retrieval candidates to exactly match Quantity.
%%
%% If there is a 100% fit the remainder is 0. Else the quantity given in Remainder need to found
%% by other mens than retrieval, e.g. by Picks.
find_retrieval_candidates(Quantity, Product) when is_integer(Quantity), Quantity >= 0 ->
    {{_, AvailableQuantity, _, _}, _} = mypl_db_query:count_product(Product),
    if
        AvailableQuantity < Quantity ->
            % impossible to fullfill the request
            {error, not_enough};
        true ->
            case find_retrieval_candidates_helper(Quantity, find_retrievable_units(Product)) of
                {ok, Units} ->
                    % find how much is left to be gathered by picks
                    Remainder = Quantity - lists:sum([X#unit.quantity || X <- Units]),
                    {ok, Remainder, [X#unit.mui || X <- Units]};
                {error, no_fit} ->
                    {ok, Quantity, []}
            end
    end.


%% @private
%% find_movable_units(string()) -> [mypl_db:unitRecord()]
%% @doc returns a list of all movable units for a product
find_movable_units(Product) -> 
    Candidates = mypl_db_util:do(qlc:q([X || X <- mnesia:table(unit), X#unit.product =:= Product,
                                                         X#unit.pick_quantity =< 0])),
    lists:filter(fun(X) -> mypl_db_util:unit_movable(X) =:= yes end, Candidates).
    

%% @private
%% @spec find_retrievable_units(string()) -> [mypl_db:unitRecord()]
%% @doc returns a list of all units for a product which can be retrived.
%%
%% (no no_picks attribute on location and no open movements)
find_retrievable_units(Product) ->
    [X || X <- find_movable_units(Product), unit_pickable_helper(X)].


%% @private
find_oldest_unit_of(Quantity, Units, Ignore) when is_integer(Quantity), is_list(Units), is_list(Ignore) ->
    L = [X || X <- Units, (X#unit.quantity - X#unit.pick_quantity) =:= Quantity],
    case lists:keysort(#unit.created_at, L -- Ignore) of
        [] -> [];
        [H|_] -> H
    end.
    
%% @private
find_oldest_unit_of(Quantity, Units) when is_integer(Quantity), is_list(Units) ->
    find_oldest_unit_of(Quantity, Units, []).

%% @private
find_oldest_units_of([], _Units, _Ignore) -> [];
find_oldest_units_of(Quantities, Units, Ignore) when is_list(Quantities), is_list(Units), is_list(Ignore) ->
    [H|T] = Quantities,
    Mui = find_oldest_unit_of(H, Units, []),
    [Mui|find_oldest_units_of(T, Units, [Mui|Ignore])].
    
%% @private
%% @spec find_oldest_units_of([integer()], [mypl_db:unitRecord()]) -> term()
%% @doc select the oldes units matching certain quantities.
%%
%% for each Quantity in Quanitits the oldest Unit matching that Quantity in Units is returned.
find_oldest_units_of(Quantities, Units) when is_list(Quantities), is_list(Units) ->
    find_oldest_units_of(Quantities, Units, []).


%% @spec find_provisioning_candidates(integer(), string()) -> 
%%       {ok, [mypl_db:muiID()], [{Quantiy::integer(), mypl_db:muiID()}]}
%% @see find_retrieval_candidates/2
%% @see find_pick_candidates/2
%% @doc find a combination of retrievals and picks to fullfill a order
%%
%% By using find_retrieval_candidates/2 and find_pick_candidates/2 the best combination
%% to get a certain amound of goods out of the warehouse is analysed.
%% Returns {error, no_fit} or {ok, retrievals, picks}
find_provisioning_candidates(Quantity, Product) ->
    % check for full MUIs which can be retrived
    case find_retrieval_candidates(Quantity, Product) of
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
                    {error, no_fit}
            end;
        {error, no_fit} ->
            {error, no_fit};
        {error,not_enough} ->
            {error, not_enough}
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
    mypl_db:init_location("EINLAG", 6000, true,  0, [no_picks]),
    mypl_db:init_location("AUSLAG", 6000, true,  0, [no_picks]),
    mypl_db:init_location("010101", 2000, true,  6, []),
    mypl_db:init_location("010102", 1950, false, 6, []),
    mypl_db:init_location("010103", 1200, false, 5, []),
    mypl_db:init_location("010201", 2000, true,  7, []),
    mypl_db:init_location("010301", 2000, true,  7, []),
    mypl_db:init_location("010302", 2000, false,  7, []),
    mypl_db:init_location("010303", 2000, false,  7, []),
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
    {error,no_fit} = find_pick_candidates(6, "a0004"), % nothing available at floor level
    
    {fit, [{4, Mui2}]} = find_pick_candidates(4, "a0003"), % Mui1 on EINLAG is older but no_picks
    {ok, Movement1} = mypl_db:init_movement(Mui1, "010201"),
    mypl_db:commit_movement(Movement1),
    {fit, [{4, Mui1}]} = find_pick_candidates(4, "a0003"), % Now Mui1 is not on EINLAG anymore
    
    {ok, Movement2} = mypl_db:init_movement(Mui1, "010301"),
    {fit, [{4, Mui2}]} = find_pick_candidates(4, "a0003"), % Mui1 is moving so it can't be picked
    mypl_db:commit_movement(Movement2),
    
    {fit, [{4, Mui1}]} = find_pick_candidates(4, "a0003"), % Now Mui1 can be picked again
    
    {ok, Movement3} = mypl_db:init_movement(Mui1, "010302"),
    mypl_db:commit_movement(Movement3),
    {fit, [{4, Mui2}]} = find_pick_candidates(4, "a0003"), % Mui1 is not floor level anymore so it can't be picked
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
    {fit, [{5, Mui1}]} = find_pick_candidates(5, "a0006"),
    {fit, [{6, Mui2}]} = find_pick_candidates(6, "a0006"),
    {fit, [{7, Mui2}]} = find_pick_candidates(7, "a0006"),
    {fit, [{8, Mui3}]} = find_pick_candidates(8, "a0006"),
    {fit, [{11, Mui3}]} = find_pick_candidates(11, "a0006"),
    
    % exclusion of MUIs works
    {fit, [{1, Mui1}]} = find_pick_candidates(1, "a0006"),
    {fit, [{1, Mui2}]} = find_pick_candidates(1, "a0006", [Mui1]),
    
    % the next should be fulfilled by "picking empty" two units
    {fit, [{5, Mui1}, {7, Mui2}]} = find_pick_candidates(12, "a0006"),
    % we can't fullfill this pick, but we can get close - do we actualy want this?
    {error, no_fit} = find_pick_candidates(25, "a0006"),
    
    %% now let's see if units with open picks are preferred
    {ok, Pick1} = mypl_db:init_pick(7, Mui3),
    {fit, [{4, Mui3}]} = find_pick_candidates(4, "a0006"),
    %% not enough on Mui3, so wee need to pick from the others
    {fit, [{5, Mui1}]} = find_pick_candidates(5, "a0006"),
    {fit, [{6, Mui2}]} = find_pick_candidates(6, "a0006"),
    %% TODO: this should find fit along the lines of  {ok, [{5, Mui1}, {3, Mui4}]}
    {fit,[{4, Mui3}, {4, Mui1}]} = find_pick_candidates(8, "a0006"),
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
    {fit, [{5, Mui3}]} = find_pick_candidates(5, "a0007"),
    % and what about two picks
    {fit, [{5, Mui3}, {7, Mui2}]} = find_pick_candidates(12, "a0007"),
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
    {error,not_enough} = find_retrieval_candidates(25, "a0008"),
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
    {ok, _} = mypl_db:store_at_location("010101", Mui1, 11, "a0009", 1200),
    {ok, _} = mypl_db:store_at_location("010102", Mui2,  7, "a0009", 1200),
    {ok, _} = mypl_db:store_at_location("010103", Mui3,  5, "a0009", 1200),
    
    % check random combinations
    {ok, [Mui3], []} = find_provisioning_candidates(5, "a0009"),
    {ok, [], [{4, Mui1}]} = find_provisioning_candidates(4, "a0009"),
    {ok, [Mui3], [{1, Mui1}]} = find_provisioning_candidates(6, "a0009"),
    {ok, [Mui2], [{3, Mui1}]} = find_provisioning_candidates(10, "a0009"),
    {ok, [Mui1], []} = find_provisioning_candidates(11, "a0009"),
    {ok, [Mui3, Mui2], []} = find_provisioning_candidates(12, "a0009"),
    {ok, [Mui3, Mui2], [{1, Mui1}]} = find_provisioning_candidates(13, "a0009"),
    
    % TODO: there are actual fits for that pick, our code is just not smart enough to find them
    {error, no_fit} = find_provisioning_candidates(22, "a0009"),
    
    {ok, [Mui3, Mui2, Mui1], []} = find_provisioning_candidates(23, "a0009"),
    {error, not_enough} = find_provisioning_candidates(24, "a0009"),
    ok.
    

%%% @hidden
testrunner() ->
    mypl_simple_picking1_test(),
    mypl_simple_picking2_test(),
    mypl_simple_picking3_test(),
    mypl_simple_retrieval_test(),
    mypl_simple_provisioning1_test(),
    mypl_simple_provisioning2_test(),
    ok.
    

-endif.
