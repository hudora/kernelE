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
%% <li>Goods where recently needed but not available. (Available via {@link mypl_requesttracker}) are
%%     moved to floorlevel. The current implementation never moves more than one Unit at a time.
%%     (To floorlevel)</li>
%% <li>Goods which are classified A by {@link mypl_abcserver} and of which no unit is at floorlevel
%%     (To floorlevel).</li>
%% <li>Goods which will be needed in near future for picking (information gained from {@link mypl_oracle})
%%     and are not available at floorlevel. (To floorlevel) - CURRENTLY UNIMPLEMENTED.</li>
%% <li>Goods which are NOT needed in the next few weeks (information gained from {@link mypl_oracle}) and
%%     are using up floorspace (To upper levels) - CURRENTLY UNIMPLEMENTED.</li>
%% <li>Unify products on multiple units onto a single unit - CURRENTLY UNIMPLEMENTED. Possibly only if
%%     are not going to pick from them in near future because this would make unifying useless.</li>
%% </ul>
%% @end

-module(mypl_movements).

-include_lib("stdlib/include/qlc.hrl").
-include("include/mypl.hrl").

-export([create_automatic_movements/0]).
-compile(export_all).

init_next_movement() ->
    get_movementsuggestion_from_requesstracker().

collect_requesed_units(Quantity, _, Acc) when Quantity =< 0 -> lists:reverse(Acc);
collect_requesed_units(_, [], Acc) -> Acc;
collect_requesed_units(Quantity, Candidates, Acc) ->
   [H|T] = Candidates,
   collect_requesed_units(Quantity - H#unit.quantity, T, [H|Acc]).
    
%% @doc generates movement suggestions by looking at requirements from the {@link requesttracker}.
get_movementsuggestion_from_requesstracker() ->
    case mypl_requesttracker:out() of
        {empty} ->
            [];
        {ok, {Quantity, Product}} ->
            % might return {empty}}
            % find movable pallets not on the floor.
            Fun = fun() ->
                Candidates = lists:keysort(#unit.created_at,
                                           lists:filter(fun(X) -> Loc = mypl_db_util:get_mui_location(X#unit.mui), 
                                                                  Loc#location.floorlevel =:= false
                                                        end, mypl_db_util:find_movable_units(Product))),
                Units = collect_requesed_units(Quantity, Candidates, []),
                Locations = mypl_db_util:best_locations(floorlevel, Units),
                lists:zip([X#unit.mui || X <- Units], [X#location.name || X <- Locations])
            end,
            mypl_db_util:transaction(Fun)
    end.
    

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
    Fun = fun() ->
        case get_abc_units() of
            [] ->
                [];
            [Unit|_] ->
                % @TODO: better handle situations where no floorlevel locations are available
                [Location] = mypl_db_util:best_locations(floorlevel, [Unit]),
                [{Unit#unit.mui, Location#location.name}]
        end
    end,
    mypl_db_util:transaction(Fun).
    

%% @doc generate movements
%% 
%% the movements are generated either based on the results from
%% {@link get_movementsuggestion_from_requesstracker/0} or if this yields nor results based on
%% {@link get_movementsuggestion_from_abc/0}.
init_automovements() ->
    case get_movementsuggestion_from_requesstracker() of
        [] ->
            %case get_movementsuggestion_from_abc() of
            %    % TODO: dalyzer says:
            %    % mypl_movements.erl:137: The variable L2 can never match since previous clauses completely covered the type []
            %    [] ->
            %        % No Movementsuggestions
            %        {ok, []};
            %    L2 ->
            %        [H|_] = L2, % we are only interested in the first result
            %        {ok, init_movements([H])}
            %end;
            {ok, []};
        L1 ->
            {ok, init_movements(L1)}
    end.
    

%% @spec init_movements([{Mui, Destination}]) -> [mypl_db:movementID()]
%% @see mypl_db:init_movement/2
%% @doc call init_movement/2 for several movements at once
init_movements(L) ->
    %% we use a transaction to ensure all movements fail if a single one fails.
    Fun = fun() ->
        lists:map(fun({Mui, Destination}) -> 
                       {ok, MovementId} = mypl_db:init_movement(Mui, Destination,
                                                                [{mypl_notify_requestracker}]),
                       MovementId
                    end, L)
          end,
    mypl_db_util:transaction(Fun).
    

%% @doc create one or more movements which make the warehouse a better place ...
%% 
%% ... by cleaning it up and optimizing.
create_automatic_movements() ->
    init_automovements().


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
    ?assertMatch([{"mui5","010301"}], get_movementsuggestion_from_requesstracker()),
    mypl_requesttracker:in(30, "a0005"),
    ?assertMatch([{"mui5","010301"},{"mui6","010201"}], get_movementsuggestion_from_requesstracker()),
    mypl_requesttracker:in(999, "a0005"),
    mypl_requesttracker:in(30, "a0004"),
    % why is mui6 this time leading the list ???
    ?assertMatch([{"mui6","010301"}, {"mui5","010201"}], get_movementsuggestion_from_requesstracker()),
    % the second call to mypl_requesttracker will address Product a0004
    ?assertMatch([{"mui3","010301"},{"mui4","010201"}], get_movementsuggestion_from_requesstracker()),
    ok.
    

%%% @hidden
testrunner() ->
    get_movementsuggestion_test(),
    ok.
    

-endif.
