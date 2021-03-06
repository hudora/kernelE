%% @version 0.2
%% @copyright 2007 HUDORA GmbH
%% @author Maximillian Dornseif <md@hudora.de>
%% @doc myPL/kernel-E movement chooser
%%
%% This module is meant to decide what to move where in the warehouse. All this movements are
%% only optimisations and not directly related to provisioning goods.
%% This tries to decide what to move where and what would be the best next move.
%% 
%% Whenever {@link create_automatic_movements/0} is called the system tries to determine the next "good move"
%% and calls init_movement for one or possible a few movements.
%%
%% Strategies for choosing a move:
%% <ul>
%% <li>Any mofes to clean up something, remove goods from unwanted locations etc.</li>
%% <li>Goods where recently needed but not available. (Available via {@link mypl_requesttracker}) are
%%     moved to floorlevel. The current implementation never moves more than one Unit at a time.
%%     (To floorlevel)</li>
%% <li>Goods which are classified A by {@link mypl_abcserver} and of which no unit is at floorlevel
%%     (To floorlevel).</li>
%% <li>Goods which will be needed in near future for picking
%%     and are not available at floorlevel. (To floorlevel) - CURRENTLY UNIMPLEMENTED.</li>
%% <li>Goods which are NOT needed in the next few weeks and
%%     are using up floorspace (To upper levels) - CURRENTLY UNIMPLEMENTED.</li>
%% <li>Unify products on multiple units onto a single unit - CURRENTLY UNIMPLEMENTED. Possibly only if
%%     are not going to pick from them in near future because this would make unifying useless.</li>
%% </ul>
%%
%% Configuration variables:
%% <ul>
%% <li>FLOORCLEANER_START - if there are <em>less</em> than FLOORCLEANER_START free locations at floor level
%%     start moving units to the upper level. Set to 0 to disable the floorcleaner.</li>
%% <li>ABCMOVEMENT_START - if there are <em>more</em> than ABCMOVEMENT_START free locations start moving down
%%     high demand goods. Set to 99999 to disable.</li>
%% </ul>
%% @end

-module(mypl_movements).

-include_lib("stdlib/include/qlc.hrl").
-include("mypl.hrl").

% There should be at least MINFLOORFREE locations free at floorlevel
-define(FLOORCLEANER_START_DEFAULT, 5).
-define(ABCMOVEMENT_START_DEFAULT, 100).


-export([unwanted_location_units/0,
         get_floor_removal_unit/0, count_empty_floor_locations/0, 
         get_movementsuggestion_from_floorcleaner/0,
         get_movementsuggestion_from_unwanted_locations/0, 
         show_movementsuggestions/0,
         create_automatic_movement/1,
         create_automatic_movements/1, more_than_one_floorunit/0]).
-compile(export_all).


unwanted_location_units_helper([]) -> [];
unwanted_location_units_helper([Location|Tail]) ->
    Ret = [X || X <- Location#location.allocated_by, mypl_db_util:unit_movable(mypl_db_util:mui_to_unit(X)) =:= yes],
    Ret ++ unwanted_location_units_helper(Tail).

unwanted_location_units() ->
    Fun = fun() ->
        UnwantedLocations = mypl_db_util:do(qlc:q([X || X <- mnesia:table(location),
                                                        X#location.preference =:= 0,
                                                        X#location.allocated_by /= []])),
        unwanted_location_units_helper(UnwantedLocations)
    end,
    mypl_db_util:transaction(Fun).


%% @doc suggests unit to be moved away von locations with preference 0
%% returns [{unit, location}]
%% see the configuration option minimum_free_floor
get_movementsuggestion_from_unwanted_locations() ->
    case unwanted_location_units() of
        [] ->
            [];
        [Mui|_Tail] ->
            Location = mypl_db_util:transaction(fun() ->
                                                      mypl_db_util:best_location(mypl_db_util:mui_to_unit(Mui))
                                                  end),
            [{Mui, Location#location.name}]
    end.


floorunits_dictbuilder2([], Dict) -> Dict;
floorunits_dictbuilder2([Mui|Tail], Dict) ->
    Unit = mypl_db_util:mui_to_unit(Mui),
    Product = Unit#unit.product,
    floorunits_dictbuilder2(Tail, dict:update_counter(Product, 1, Dict)).

floorunits_dictbuilder1([], Dict) -> Dict;
floorunits_dictbuilder1([Location|Tail], Dict) ->
    floorunits_dictbuilder1(Tail, floorunits_dictbuilder2(Location#location.allocated_by, Dict)).

    
%% @doc returns for every product how many units are available at floorlevel]
%% [{21,"10202/01"}, {18,"14600/03"}, {17,"10111"}, {15,"71653"}, ...
floorunits() ->
    Fun = fun() ->
        FloorLocations = mypl_db_util:do(qlc:q([X || X <- mnesia:table(location),
                                                     X#location.floorlevel =:= true])),
        lists:map(fun({A, B}) -> {B, A} end,
                  dict:to_list(floorunits_dictbuilder1(FloorLocations, dict:new())))
    end,
    lists:reverse(lists:sort(mypl_db_util:transaction(Fun))).
    

%% @doc returns a list of articles with more than one unit on the floor
more_than_one_floorunit() ->
    [Product || {Quantity, Product} <- floorunits(), Quantity > 1].
    

% @doc gets a list of lists of products of which a single unit shoul be moved from floor level to a higher level.
% 
% the lists are ordered by priority and may contain duplicates.
get_floor_removal_products() ->
    {Araw, Braw, Craw} =  mypl_abcserver:get_abc(),
    [A, B, C] = lists:map(fun(X) -> ordsets:from_list([Product || {_, Product} <- X]) end, [Araw, Braw, Craw]),
    Penner = ordsets:from_list(mypl_abcserver:get_penner()),
    Doppelboden = ordsets:from_list(more_than_one_floorunit()),
    Einzelboden = ordsets:from_list([Product || {Quantity, Product} <- floorunits(), Quantity =:= 1]),
    InPipeline = lists:sort([Y || {_X, Y} <- mypl_prov_query:pipelinearticles()]),
    Alle = ordsets:union([A, B, C]),
    Boden = ordsets:union([Einzelboden, Doppelboden]),
    % @TODO: create a way to find open orders
    %NoOrders = InPipeleine
    
    _Hochlagerartikel = [
    % Es folgen Produkte, für die wir keine Aufträge haben
    % Penner, von denen mehr als eine Palette auf dem Boden Stehen
    %erlang:display({ordsets:to_list(sets:subtract(ordsets:intersection(Penner, Doppelboden), NoOrders))}),
    %% Penner, von denen eine Palette auf dem Bode steht
    %erlang:display({ordsets:to_list(sets:subtract(ordsets:intersection(Penner, Einzelboden), NoOrders))}),
    %% C Artikel von denen mehr als eine Palette am Boden steht
    %erlang:display({ordsets:to_list(sets:subtract(ordsets:intersection(C, Doppelboden), NoOrders))}),
    %% C Artikel von denen eine Palette am Boden steht
    %erlang:display({ordsets:to_list(sets:subtract(ordsets:intersection(C, Einzelboden), NoOrders))}),
    %% B Artikel von denen eine Palette am Boden steht
    %% erlang:display({ordsets:to_list(subtract(ordsets:intersection(B, Einzelboden), NoOrders))}),
    %% B Artikel von denen mehr als eine Palette am Boden steht
    %erlang:display({ordsets:to_list(sets:subtract(ordsets:intersection(B, Doppelboden), NoOrders))}),
    %% A Artikel von denen mehr als eine Palette am Boden steht
    %% erlang:display({ordsets:to_list(subtract(ordsets:intersection(A, Doppelboden), NoOrders))}),
    
    % Es folgen Produkte, für die wir keine Komissionierbelege haben
    % Penner, von denen mehr als eine Palette auf dem Boden Stehen
    ordsets:subtract(ordsets:intersection(Penner, Doppelboden), InPipeline),
    % Penner, von denen eine Palette auf dem Bode steht
    ordsets:subtract(ordsets:intersection(Penner, Einzelboden), InPipeline),
    % C Artikel von denen mehr als eine Palette am Boden steht
    ordsets:subtract(ordsets:intersection(C, Doppelboden), InPipeline),
    % C Artikel von denen eine Palette am Boden steht
    ordsets:subtract(ordsets:intersection(C, Einzelboden), InPipeline),
    % B Artikel von denen mehr als eine Palette am Boden steht
    ordsets:subtract(ordsets:intersection(B, Doppelboden), InPipeline),
    
    % Es folgen Artikel, für die wir Aufträge haben
    % Penner, von denen mehr als eine Palette auf dem Boden Stehen
    ordsets:intersection(Penner, Doppelboden),
    % C Artikel von denen mehr als eine Palette am Boden steht
    ordsets:intersection(C, Doppelboden),
    % B Artikel von denen mehr als eine Palette am Boden steht
    ordsets:intersection(B, Doppelboden),
    % A Artikel von denen mehr als eine Palette am Boden steht
    ordsets:intersection(A, Doppelboden),
    
    % zum Ende alles:
    ordsets:intersection(Alle, Boden)
    ].
    

% @doc get a mui which should be removed from floorlevel and should be put somewhere higher
get_floor_removal_unit() ->
    Fun = fun() ->
        get_floor_removal_unit1(get_floor_removal_products())
    end,
    mypl_db_util:transaction(Fun).

get_floor_removal_unit1([]) ->
    [];
get_floor_removal_unit1([Head|Tail]) ->
    case get_floor_removal_unit2(Head) of
        [] ->
            get_floor_removal_unit1(Tail);
        Ret ->
            Ret
    end.
    

get_floor_removal_unit2([]) -> [];
get_floor_removal_unit2([Product|CandidateProducts]) ->
    Units = mypl_db_query:find_floor_units_for_product(Product),
    % We prefer to move smallest units upwards first, because they have a higher chance to be choosen
    % for retrievals instead of picks.
    SortedUnits = lists:keysort(#unit.quantity, Units),
    case [X || X <- SortedUnits, mypl_db_util:unit_movable(X) =:= yes] of
        [] ->
            get_floor_removal_unit2(CandidateProducts);
        [MovableUnit|_Tail] ->
            MovableUnit
    end.
    

%% @doc Cont the number of Empty, non Priority 1 Locations at floorlevel.
count_empty_floor_locations() ->
    Fun = fun() ->
        length([X || X <- mypl_db_util:find_empty_location(1950), X#location.floorlevel =:= true,
                                                                  X#location.preference > 1 ])
    end,
    mypl_db_util:transaction(Fun).
    

%% @doc suggests unit to be moved to higher levels to keep enough space at floor level
%% returns [{unit, location}]
%% see the configuration option minimum_free_floor
get_movementsuggestion_from_floorcleaner() ->
    Empty = count_empty_floor_locations(),
    % A minimum of X locations must be free at floorlevel
    MinEmpty = mypl_util:get_config(floorcleaner_start, ?FLOORCLEANER_START_DEFAULT), 
    if
        Empty < MinEmpty ->
            case get_floor_removal_unit() of
                [] ->
                    [];
                Unit ->
                    case mypl_db_util:transaction(fun() -> 
                                                      mypl_db_util:best_locations(higherlevel, [Unit])
                                                  end) of
                        [[]] -> 
                            []; % TODO: why not []?
                        [Location] ->
                            [{Unit#unit.mui, Location#location.name}]
                    end
            end;
        true ->
            []
    end.
    

init_next_movement() ->
    get_movementsuggestion_from_requesttracker().
    

collect_requesed_units(Quantity, _, Acc) when Quantity =< 0 -> lists:reverse(Acc);
collect_requesed_units(_, [], Acc) -> Acc;
collect_requesed_units(Quantity, Candidates, Acc) ->
   [H|T] = Candidates,
   collect_requesed_units(Quantity - H#unit.quantity, T, [H|Acc]).
    

%% @doc generates list of movement suggestions by looking at requirements from the {@link requesttracker}.
%%
%% a movement suggestion is defined as {unit, location}
get_movementsuggestion_from_requesttracker(0) -> [];
get_movementsuggestion_from_requesttracker(Tries) ->
    % might return {empty}}
    case mypl_requesttracker:out() of
        {empty} ->
            [];
        {ok, {Quantity, Product}} ->
            % find movable pallets not on the floor.
            Fun = fun() ->
                Candidates = lists:keysort(#unit.created_at,
                                           lists:filter(fun(X) -> Loc = mypl_db_util:get_mui_location(X#unit.mui), 
                                                                  Loc#location.floorlevel =:= false
                                                        end, mypl_db_util:find_movable_units(Product))),
                Units = collect_requesed_units(Quantity, Candidates, []),
                Locations = mypl_db_util:best_locations(floorlevel, Units),
                % sorting results in lowest unit_ids beeing first
                % TODO: better choose the oldest
                lists:sort(lists:zip([X#unit.mui || X <- Units], [X#location.name || X <- Locations]))
            end,
            case mypl_db_util:transaction(Fun) of
                [] ->
                    % retry
                    get_movementsuggestion_from_requesttracker(Tries-1);
                [H|_] ->
                    [H]
            end
    end.

get_movementsuggestion_from_requesttracker() ->
    % we do 17 tries since get_movementsuggestion_from_requesttracker() might return []
    % if there are currently no movable units of an product.
    get_movementsuggestion_from_requesttracker(17).
    

% [] == no units at floorlevel 
% TODO: instead check "no units at floorlevel not having any open movements or open picks"
get_movementsuggestion_from_abc_helper(Product, []) ->
    % if nothing is currently moving
    case mypl_db_query:open_movements_for_product(Product) of
        [] ->
            % suggest a unit to be moved to the floor
            case mypl_db_util:find_movable_units(Product) of
                [] ->
                    [];
                L ->
                    [H|_] = lists:keysort(#unit.created_at, L),
                    [H]
            end;
        _L ->
            % else ignore that product
            []
    end;
get_movementsuggestion_from_abc_helper(_Product, _Units) ->
    % if there are units at floorlevel we suggest nothing
    [].
    

%% @doc gets a list of units which should be moved to floorlevel based on ABC classification
get_abc_units() ->
    {A, _B, _C} = mypl_abcserver:get_abc(),
    lists:flatten(lists:map(fun({_Picks, Product}) ->
                  get_movementsuggestion_from_abc_helper(Product, 
                                                         mypl_db_query:find_floor_units_for_product(Product))
              end, A)).
    

%% @spec get_movementsuggestion_from_abc() -> [Suggestion]
%%          Suggestion = {Mui, Location}
%%
%% @doc generates movement suggestions by looking at ABC classification.
%% 
%% This is done by consulting {@link mypl_abcserver:get_abc/0} and checking for all products which
%% are classified as "A" but have no unit at floorlevel
get_movementsuggestion_from_abc() ->
    Empty = count_empty_floor_locations(),
    % A minimum of X locations must be free at floorlevel
    MinEmpty = mypl_util:get_config(abcmovement_start, ?ABCMOVEMENT_START_DEFAULT), 
    if
        Empty > MinEmpty ->
            Fun = fun() ->
                case get_abc_units() of
                    [] ->
                        [];
                    [Unit|_] ->
                        [Location] = mypl_db_util:best_locations(floorlevel, [Unit]),
                        [{Unit#unit.mui, Location#location.name}]
                end
            end,
            mypl_db_util:transaction(Fun);
        true ->
            []
    end.


%% @doc displays all movementsuggestions
show_movementsuggestions() ->
    Unwanted       = get_movementsuggestion_from_unwanted_locations(),
    Floorcleaner   = get_movementsuggestion_from_floorcleaner(),
    Requesttracker = get_movementsuggestion_from_requesttracker(),
    Abc            = get_movementsuggestion_from_abc(),
    [{unwanted_locations, Unwanted},
     {floorcleaner, Floorcleaner},
     {requesttracker, Requesttracker},
     {abc, Abc}
    ].
    

init_automovements() ->
    init_automovements([]).
%% @doc generate movements
%% 
%% the movements are generated either based on the results from
%% {@link get_movementsuggestion_from_floorcleaner/0}, {@link get_movementsuggestion_from_requesttracker/0} or if this yields nor results based on
%% {@link get_movementsuggestion_from_abc/0}.
init_automovements(Attributes) ->
    case get_movementsuggestion_from_floorcleaner() of
        [] ->
            case get_movementsuggestion_from_unwanted_locations() of
                [] ->
                    case get_movementsuggestion_from_requesttracker() of
                        [] ->
                            % TODO: reactivate ABC movements
                            %case get_movementsuggestion_from_abc() of
                            %    % TODO: dalyzer says:
                            %    % mypl_movements.erl:137: The variable L2 can never match since previous clauses completely covered the type []
                            %    [] ->
                            %        % No Movementsuggestions
                            %        {ok, []};
                            %    L2 ->
                            %        [H|_] = L2, % we are only interested in the first result
                            %        {ok, init_movements([H], [])}
                            %end;
                            {ok, []};
                        L3 ->
                            mypl_log:log("Movements für Requesttracker initialisiert: ~p", [L3], {Attributes}),
                            {ok, init_movements(L3, [{mypl_notify_requesttracker},
                                                     {reason, requesttracker}|Attributes])}
                    end;
                L2 ->
                    mypl_log:log("Movements für Floorcleaner initialisiert: ~p", [L2], {Attributes}),
                    {ok, init_movements(L2, [{reason, floorcleaner}|Attributes])}
            end;
        L1 ->
            mypl_log:log("Movements für Unwanted Location Cleaner initialisiert: ~p", [L1], {Attributes}),
            {ok, init_movements(L1, [{reason, unwanted_locations}|Attributes])}
    end.
    

%% @see mypl_db:init_movement/2
%% @doc call init_movement/2 for several movements at once
init_movements(L, Attributes) when is_list(L), is_list(Attributes) ->
    %% we use a transaction to ensure all movements fail if a single one fails.
    Fun = fun() ->
        lists:map(fun({Mui, Destination}) -> 
                      {ok, MovementId} = mypl_db:init_movement(Mui, Destination, Attributes),
                      MovementId
                    end, L)
          end,
    mypl_db_util:transaction(Fun).
    

%% @doc create one or more movements which make the warehouse a better place ...
%% ... by cleaning it up and optimizing.
-spec create_automatic_movements(list()) -> {ok, [mypl_db:movementID(), ...]}.
create_automatic_movements(Attributes) ->
    init_automovements(Attributes).


%% @doc create ONLY one movement which make the warehouse a better place ...
create_automatic_movement(Attributes) ->
    case create_automatic_movements(Attributes) of
        {ok, []} ->
            nothing_available;
        {ok, [Movement|Tail]} ->
            % in case thre was generated more than one movement - rollback the unwanted movements
            [mypl_db:rollback_movement(X) || X <- Tail],
            Movement
        end.
    

% ~~ Unit tests
-ifdef(EUNIT).
-compile(export_all).

%% @hidden
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
    mypl_db:init_location("010101", 2000, true,  1, []),
    mypl_db:init_location("010102", 1950, false, 2, []),
    mypl_db:init_location("010103", 1200, false, 3, []),
    mypl_db:init_location("010201", 2000, true,  4, []),
    mypl_db:init_location("010202", 2000, false, 5, []),
    mypl_db:init_location("010203", 2000, false, 6, []),
    mypl_db:init_location("010301", 2000, true,  7, []),
    mypl_db:init_location("010302", 2000, false, 8, []),
    mypl_db:init_location("010303", 2000, false, 9, []),
    ok.
 

%%% @hidden
get_movementsuggestion_test() ->
    mypl_requesttracker:start(),
    test_init(),
    {ok, _} = mypl_db:store_at_location("010102", "mui1",  5, "a0003", 1200),
    {ok, _} = mypl_db:store_at_location("010103", "mui2",  5, "a0003", 1200),
    {ok, _} = mypl_db:store_at_location("010202", "mui3", 17, "a0004", 1200),
    {ok, _} = mypl_db:store_at_location("010203", "mui4", 19, "a0004", 1200),
    {ok, _} = mypl_db:store_at_location("010302", "mui5", 23, "a0005", 1200),
    {ok, _} = mypl_db:store_at_location("010303", "mui6", 71, "a0005", 1200),
    mypl_requesttracker:in(20, "a0005"),
    ?assertMatch([{"mui5","010301"}], get_movementsuggestion_from_requesttracker()),
    mypl_requesttracker:in(30, "a0005"),
    ?assertMatch([{"mui5","010301"}], get_movementsuggestion_from_requesttracker()),
    mypl_requesttracker:in(999, "a0005"),
    mypl_requesttracker:in(30, "a0004"),
    % why is mui6 this time leading the list ???
    ?assertMatch([{"mui6","010301"}], get_movementsuggestion_from_requesttracker()),
    % the second call to mypl_requesttracker will address Product a0004
    ?assertMatch([{"mui3","010301"}], get_movementsuggestion_from_requesttracker()),
    ok.
    

%%% @hidden
testrunner() ->
    get_movementsuggestion_test(),
    ok.
    

-endif.
