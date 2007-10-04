-module(test).
-compile(export_all).

-include("mypl.hrl").

testbefuellung([]) -> [];
testbefuellung(L) ->
    [H|T] = L,
    io:format("A ~w~n", [H]),
    {_Softmlocation, Artnr, Quantity} = H,
    {ok, _} = mypl:store(mypl:oid(), Quantity, Artnr, 1950),
    testbefuellung(T).
    

generate_locations() ->
    Fun = fun() ->
        mnesia:write(#location{name="EINLAG", height=3000, floorlevel=true,  preference=0, allocated_by=[], reserved_for=[]}),
        mnesia:write(#location{name="010101", height=2000, floorlevel=true,  preference=6, allocated_by=[], reserved_for=[]}),
        mnesia:write(#location{name="010102", height=1950, floorlevel=false, preference=6, allocated_by=[], reserved_for=[]}),
        mnesia:write(#location{name="010103", height=1200, floorlevel=false, preference=5, allocated_by=[], reserved_for=[]}),
        mnesia:write(#location{name="010201", height=2000, floorlevel=true,  preference=7, allocated_by=[], reserved_for=[]}),
        mnesia:write(#location{name="010202", height=1950, floorlevel=false, preference=5, allocated_by=[], reserved_for=[]}),
        mnesia:write(#location{name="010203", height=1200, floorlevel=false, preference=5, allocated_by=[], reserved_for=[]})
    end,
    {atomic,ok} = mnesia:transaction(Fun).
    

clear_table_helper(_Table, []) -> [];
clear_table_helper(Table, L) ->
    [H|T] = L,
    io:format("deleting ~w~n", [{Table, H}]),
    mnesia:delete({Table, H}),
    clear_table_helper(Table, T).
    

clear_table(Table) ->
    Fun = fun() ->
        clear_table_helper(Table, mnesia:all_keys(unit))
    end,
    mnesia:transaction(Fun).
    

test_counting() ->
    io:format("counting goods ...~n"),
    [{"14601", 5}, {"14600",18}] = mypl:count_products(),
    5 = mypl:count_product("14601"),
    18 = mypl:count_product("14600"),
    0 = mypl:count_product("14602").
    

test_movements1(Mui1) ->
    io:format("testing imperative movements ...~n"),
    Loc1 = mypl:get_mui_location(Mui1),
    {ok, Movement1} = mypl:init_movement(Mui1, "010101"),
    5 = mypl:count_product("14601"),
    mypl:commit_movement(Movement1),
    5 = mypl:count_product("14601"),
    Loc2 = mypl:get_mui_location(Mui1),
    io:format("~w~n", [Loc2#location.name]),
    "010101" = Loc2#location.name.
    

test_movements2(Mui1) ->
    io:format("testing self-directed movements ...~n"),
    Loc1 = mypl:get_mui_location(Mui1),
    {ok, Movement1} = mypl:init_movement_to_good_location(Mui1),
    5 = mypl:count_product("14601"),
    mypl:commit_movement(Movement1),
    5 = mypl:count_product("14601"),
    Loc2 = mypl:get_mui_location(Mui1).
    

test_finding() ->
    io:format("finding goods ...~n"),
    [_,_] = mypl:find_product("14600"),
    [_] = mypl:find_product("14601"),
    [] = mypl:find_product("14602"),
    
    [_] = mypl:find_product("14600", 7),
    [] = mypl:find_product("14600", 6),
    [] = mypl:find_product("14602", 7),

    io:format("----> ~w~n", [mypl:find_pick_candidates(3, "14600")]),
    [[7, 11]] = mypl:find_retrival_candidates(18, "14600"),
    [[7]] = mypl:find_retrival_candidates(7, "14600"),
    [] = mypl:find_retrival_candidates(17, "14600").
    

test_utils() ->
    [[1,1,1,1,1],[1,1,1,2],[1,1,3],[1,4],[5]] = mypl_util:choose([1,1,1,1,1,2,3,4,5,6,7], 5),
    [[7]] = mypl_util:choose([7, 18], 7).
    

% test2() ->
    % this tests assume they have too prepare everything for themselfs

test() ->
    test_utils(),
    io:format("reinit data store ...~n"),
    clear_table(location),
    clear_table(movement),
    clear_table(unit),
    % mypl_manager:init_mypl(),
    mypl:start(),
    
    io:format("generating initial data ... locations ..."),
    generate_locations(),
    io:format("putting in 3 units~n"),
    Mui1 = mypl:generate_mui(),
    mypl:store_at_location("EINLAG", Mui1, 5,  "14601", 1900),
    mypl:store_at_location("EINLAG", mypl:generate_mui(), 7,  "14600", 1900),
    mypl:store_at_location("EINLAG", mypl:generate_mui(), 11, "14600", 1900),
    
    test_counting(),
    test_movements1(Mui1),
    test_counting(),
    test_movements2(Mui1),
    test_counting(),
    test_finding().
    

timeit() ->
    {M, R} = timer:tc(?MODULE, test, []),
    io:format("~w~n~n", [R]),
    io:format("~p microseconds~n", [M]).
    

