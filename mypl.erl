-module(mypl).
-compile(export_all).

-include_lib("/opt/local/lib/erlang/lib/stdlib-1.14.5/include/qlc.hrl").
-include("mypl.hrl").

-export([start/0, stop/0, store/4, store_at_location/5]).

start() ->
    mnesia:start().

stop() ->
    mnesia:stop().

oid() -> {node(), erlang:now()}. 


% api:
% store_at_location
% store

store_at_location(Locname, Mui, Quantity, Product, Height) when Quantity > 0 ->
    Fun = fun() ->
        % check that location exists
        [Location] = mnesia:read({location, Locname}),
        % check no unit_load record with this mui exists
        [] = mnesia:read({unit_load, Mui}),
        % generate unit_load record
        Entry = #unit_load{mui=Mui, quantity=Quantity, product=Product, height=Height},
        mnesia:write(Entry),
        Newloc = Location#location{allocated_by=Mui},
        mnesia:write(Newloc),
        {ok, Entry}
    end,
    mnesia:transaction(Fun).

store(Mui, Quantity, Product, Height) ->
    case find_empty_location(Height) of
    [] ->
        {error, no_suitable_location};
    [Loc| _]->
        store_at_location(Loc#location.name, Mui, Quantity, Product, Height)
    end.
    

retrive(Locname, Mui) ->
    Fun = fun() ->
        % check that location exists
        [Location] = mnesia:read({location, Locname}),
        %% check no unit_load record with this mui exists
        [Unit] = mnesia:read({unit_load, Mui}),
        % delete unit_load record
        mnesia:delete(unit_load, Mui),
        % update locatione
        Newloc = Location#location{allocated_by=undefined},
        mnesia:write(Newloc),
        {Newloc}
    end,
    mnesia:transaction(Fun).
    

count_products([], D) -> dict:to_list(D);
count_products(L, D) ->
    [P|T] = L,
    NewDict = dict:update_counter(P#unit_load.product, P#unit_load.quantity, D),
    count_products(T, NewDict).
count_products() ->
    L = do(qlc:q([X || X <- mnesia:table(unit_load)])),
    D = dict:new(),
    count_products(L, D).

% database helper
do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

find_empty_location(Height) ->
    do(qlc:q([X || X <- mnesia:table(location), X#location.height >= Height, X#location.allocated_by =:= undefined, X#location.reserved_for =:= undefined])).

find_empty_floor_location(Height) ->
    do(qlc:q([X || X <- mnesia:table(location), X#location.floorlevel =:= true, X#location.height >= Height, X#location.allocated_by =:= undefined, X#location.reserved_for =:= undefined])).


%test() ->
%    % empty warehouse
%    L = do(qlc:q([X || X <- mnesia:table(unit_load)])),

timeit() ->
    {M, R} = timer:tc(?MODULE, demo, []),
    io:format("~w~n~n", [R]),
    io:format("~p microseconds~n", [M]).

demo() ->
    % mnesia:start(),
    [Loc| _] = find_empty_location(1950),
    find_empty_floor_location(1950),
    store_at_location(Loc#location.name, oid(), 50, "14600", 1950),
    store_at_location(Loc#location.name, oid(), 70, "14850", 1850),
    count_products().
    