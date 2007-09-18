-module(mypl).

-export([store/5]).

-include("mypl.hrl").

oid() -> {node(), erlang:now()}. 

store(Location, Mui, Quantity, Product, Height) ->
    fun() ->
        %% check no unit_load record with this mui exists
        [] = mnesia:read({unit_load, Mui}),
        % check that location exists
        [Storage_location] = mnesia:read({storage_location, Location}),
        % generate unit_load record
        Entry = #unit_load{mui=Mui, quantity=Quantity, product=Product, height=Height},
        mnesia:write(Entry)
    end.
    