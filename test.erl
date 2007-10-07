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
        mnesia:write(#location{name="EINLAG", height=6000, floorlevel=true,  preference=0, allocated_by=[], reserved_for=[], attributes=[no_picks]}),
        mnesia:write(#location{name="AUSLAG", height=6000, floorlevel=true,  preference=0, allocated_by=[], reserved_for=[], attributes=[no_picks]}),
        mnesia:write(#location{name="010101", height=2000, floorlevel=true,  preference=6, allocated_by=[], reserved_for=[], attributes=[]}),
        mnesia:write(#location{name="010102", height=1950, floorlevel=false, preference=6, allocated_by=[], reserved_for=[], attributes=[]}),
        mnesia:write(#location{name="010103", height=1200, floorlevel=false, preference=5, allocated_by=[], reserved_for=[], attributes=[]}),
        mnesia:write(#location{name="010201", height=2000, floorlevel=true,  preference=7, allocated_by=[], reserved_for=[], attributes=[]}),
        mnesia:write(#location{name="010202", height=1950, floorlevel=false, preference=5, allocated_by=[], reserved_for=[], attributes=[]}),
        mnesia:write(#location{name="010203", height=1200, floorlevel=false, preference=5, allocated_by=[], reserved_for=[], attributes=[]}),
        mnesia:write(#location{name="010301", height=1950, floorlevel=true,  preference=3, allocated_by=[], reserved_for=[], attributes=[]}),
        mnesia:write(#location{name="010302", height=1950, floorlevel=false, preference=3, allocated_by=[], reserved_for=[], attributes=[]}),
        mnesia:write(#location{name="010303", height=1950, floorlevel=false, preference=3, allocated_by=[], reserved_for=[], attributes=[]}),
        mnesia:write(#location{name="010401", height=1950, floorlevel=true,  preference=3, allocated_by=[], reserved_for=[], attributes=[]}),
        mnesia:write(#location{name="010402", height=1950, floorlevel=false, preference=3, allocated_by=[], reserved_for=[], attributes=[]}),
        mnesia:write(#location{name="010403", height=1950, floorlevel=false, preference=3, allocated_by=[], reserved_for=[], attributes=[]}),
        mnesia:write(#location{name="010501", height=1950, floorlevel=true,  preference=3, allocated_by=[], reserved_for=[], attributes=[]}),
        mnesia:write(#location{name="010502", height=1950, floorlevel=false, preference=3, allocated_by=[], reserved_for=[], attributes=[]}),
        mnesia:write(#location{name="010503", height=1950, floorlevel=false, preference=3, allocated_by=[], reserved_for=[], attributes=[]}),
        mnesia:write(#location{name="010601", height=1950, floorlevel=true,  preference=3, allocated_by=[], reserved_for=[], attributes=[]}),
        mnesia:write(#location{name="010602", height=1950, floorlevel=false, preference=3, allocated_by=[], reserved_for=[], attributes=[]}),
        mnesia:write(#location{name="010603", height=1950, floorlevel=false, preference=3, allocated_by=[], reserved_for=[], attributes=[]})
    end,
    {atomic,ok} = mnesia:transaction(Fun).
    

clear_table_helper(_Table, []) -> [];
clear_table_helper(Table, L) ->
    [H|T] = L,
    % io:format("deleting ~w~n", [{Table, H}]),
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
    _Loc1 = mypl:get_mui_location(Mui1),
    {ok, Movement1} = mypl:init_movement(Mui1, "010101"),
    5 = mypl:count_product("14601"),
    mypl:commit_movement(Movement1),
    5 = mypl:count_product("14601"),
    Loc2 = mypl:get_mui_location(Mui1),
    "010101" = Loc2#location.name.
    

test_movements2(Mui1) ->
    io:format("testing self-directed movements ...~n"),
    _Loc1 = mypl:get_mui_location(Mui1),
    {ok, Movement1} = mypl:init_movement_to_good_location(Mui1),
    5 = mypl:count_product("14601"),
    mypl:commit_movement(Movement1),
    5 = mypl:count_product("14601"),
    _Loc2 = mypl:get_mui_location(Mui1),
    {ok, Movement2} = mypl:init_movement(Mui1, "010101"),
    mypl:commit_movement(Movement2).
    

test_finding() ->
    io:format("finding goods "),
    [_,_] = mypl:find_product("14600"),
    io:format("."),
    [_] = mypl:find_product("14601"),
    io:format("."),
    [] = mypl:find_product("14602"),
    
    io:format("."),
    % this should fail since we have eough 14600 but not in pickable locations
    {error, no_fit} = mypl:find_provisioning_candidates(18, "14600"),
    io:format("."),
    {error, no_fit} = mypl:find_provisioning_candidates(17, "14600"),
    io:format("."),
    {error, no_fit} = mypl:find_provisioning_candidates(7, "14600"),
    io:format("."),
    % 14601 is actually accessible
    {ok,[_],[]} = mypl:find_provisioning_candidates(5, "14601"),
    io:format("."),
    {ok,[],[{_, 4}]} = mypl:find_provisioning_candidates(4, "14601"),
    io:format(".~n").
    


test_utils() ->
    % this tests are for functions which have no need for database access
    io:format("testing non-database utils "),
    [[1,1,1,1,1],[1,1,1,2],[1,1,3],[1,4],[5]] = mypl_util:choose([1,1,1,1,1,2,3,4,5,6,7], 5),
    io:format("."),
    [[7]] = mypl_util:choose([7, 18], 7),
    io:format("."),
    {1,[b, c]} = mypl:find_less_than_units_of(5, [#unit{mui=a, created_at=123, quantity=2},
                                             #unit{mui=b, created_at=121, quantity=2},
                                             #unit{mui=c, created_at=122, quantity=2}]),
    io:format("\n").


% test2() ->
    % this tests assume they have too prepare everything for themselfs
    

test() ->
    test_utils(),
    
    io:format("reinit data store ...~n"),
    clear_table(movement),
    clear_table(unit),
    clear_table(location),
    % mypl_manager:init_mypl(),
    mypl:start(),
    
    io:format("generating initial data ... locations ..."),
    generate_locations(),
    io:format("putting in 3 units~n"),
    Mui1 = "14601" ++ mypl:generate_mui(),
    mypl:store_at_location("EINLAG", Mui1, 5,  "14601", 1900),
    mypl:store_at_location("EINLAG", "14600" ++ mypl:generate_mui(), 7,  "14600", 1900),
    mypl:store_at_location("EINLAG", "14600" ++ mypl:generate_mui(), 11, "14600", 1900),
    
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
    

