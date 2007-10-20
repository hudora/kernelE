%% @version 0.1
%% @copyright 2007 HUDORA GmbH
%% @author Maximillian Dornseif <md@hudora.de>
%% @doc myPL/kernel-E movement chooser
%%
%% This tries to decide what to move where and what would be the best next move.
%%
%% Strategies for choosing a move:
%% <ul>
%% <li>Goods where recently needed but not available. (Available via mypl_requesttracker) </li>
%% </ul>
%% @end

-module(mypl_movements).

-include_lib("stdlib/include/qlc.hrl").
-include("include/mypl.hrl").

-export([create_automatic_movements/0]).

init_next_movement() ->
    get_movementsuggestion_from_requesstracker().

collect_requesed_units(Quantity, _, Acc) when Quantity =< 0 -> lists:reverse(Acc);
collect_requesed_units(_, [], Acc) -> Acc;
collect_requesed_units(Quantity, Candidates, Acc) ->
   [H|T] = Candidates,
    collect_requesed_units(Quantity - H#unit.quantity, T, [H|Acc]).
    

get_movementsuggestion_from_requesstracker() ->
    case mypl_requesttracker:out() of
        {empty} ->
            [];
        {ok, {Quantity, Product}} ->
            % might return {empty}}
            % find movable pallets not on the floor.
            Candidates = lists:keysort(#unit.created_at,
                                       lists:filter(fun(X) -> Loc = mypl_db_util:get_mui_location(X#unit.mui), 
                                                              Loc#location.floorlevel =:= false
                                                    end, mypl_db_util:find_movable_units(Product))),
            Units = collect_requesed_units(Quantity, Candidates, []),
            Locations = mypl_db_util:best_locations(floorlevel, Units),
            lists:zip([X#unit.mui || X <- Units], [X#location.name || X <- Locations])
    end.
    

init_requestracker_movements() ->
    case get_movementsuggestion_from_requesstracker() of
        L ->
            {ok, lists:map(fun({Mui, Destination}) -> mypl_db:init_movement(Mui, Destination) end, L)}
    end.
    

create_automatic_movements() ->
    init_requestracker_movements().

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
    {ok, _} = mypl_db:store_at_location("010102", mui1,  5, "a0003", 1200),
    {ok, _} = mypl_db:store_at_location("010103", mui2,  5, "a0003", 1200),
    {ok, _} = mypl_db:store_at_location("010202", mui3, 17, "a0004", 1200),
    {ok, _} = mypl_db:store_at_location("010203", mui4, 19, "a0004", 1200),
    {ok, _} = mypl_db:store_at_location("010302", mui5, 23, "a0005", 1200),
    {ok, _} = mypl_db:store_at_location("010303", mui6, 71, "a0005", 1200),
    mypl_requesttracker:in(20, "a0005"),
    [{mui5,"010301"}] = get_movementsuggestion_from_requesstracker(),
    mypl_requesttracker:in(30, "a0005"),
    [{mui5,"010301"},{mui6,"010201"}] = get_movementsuggestion_from_requesstracker(),
    mypl_requesttracker:in(999, "a0005"),
    mypl_requesttracker:in(30, "a0004"),
    % why is mui6 this time leading the list ???
    [{mui6,"010301"}, {mui5,"010201"}] = get_movementsuggestion_from_requesstracker(),
    % the second call to mypl_requesttracker will address Product a0004
    [{mui3,"010301"},{mui4,"010201"}] = get_movementsuggestion_from_requesstracker(),
    ok.
    

%%% @hidden
testrunner() ->
    get_movementsuggestion_test(),
    ok.
    

-endif.
