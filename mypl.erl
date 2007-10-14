%% @version 0.1
%% @copyright 2007 HUDORA GmbH
%% @author Maximillian Dornseif <md@hudora.de>
%% @doc myPL main engine
%%
%% This implements our basic functionality for storing (and finding) things in our warehouse. 
%% 
%% The system only passes IDs (MUIs, PickIDs, Location-Names) to the outside, no special datastructures
%% or objects except for data retrival funcions, where occasional list of lists are used.
%% 
%% The usual cycle is:
%% 1. A unit enteres the warehouse and is stored on a special logation (e.g. "EINLAG").
%% 2. A movement is generated from EINLA to a suitable location somewhere in the warehouse.
%% 3. The unit is physically moved, the software is informed if the physical process is finished.
%% 4. The movement is marked as "done" the unit is now stored on the new location.
%%
%% @end

-module(mypl).
% -compile(export_all).

-include_lib("/opt/local/lib/erlang/lib/stdlib-1.14.5/include/qlc.hrl").
-include("mypl.hrl").

-export([start/0, stop/0, init_mypl/0, generate_mui/0, store_at_location/5, retrive/1,
count_products/0, count_product/1, 
 
init_movement/2, init_movement_to_good_location/1, commit_movement/1, 
get_mui_location/1, find_product/1,
find_provisioning_candidates/2,

%  for testing
find_less_than_units_of/2,
add_movementsuggestion/1, get_movementsuggestion/0
]).


%% @doc
%%
%% api:
%% generate_mui()         - returns a globally unique MUI
%% store_at_location      - generates a unit and stores product at a location
%% count_product(product) - counts all units of a single product
%% count_products()       - counts all products
%% init_movement_to_good_location() - start a movement to an appropriate location
%% init_movement()        - start a movement to an user choosen location
%% commit_movement()      - finnish movement
%% find_product()         - get all {quantities, MUIs} of a product
%% find_provisioning_candidates
%%
%% Missing:
%% rollback_movement()
%% init_pick()
%% rollback_pick()

%% store - stores product at a location choosen by the system


start() ->
    mnesia:start(),
    movementsuggestions_loop_start().
    

stop() ->
    movementsuggestions_loop_stop(),
    mnesia:stop().
    

generate_mui() ->
    mypl_util:oid().
    

init_mypl() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(unit,         [{disc_copies,[node()]}, {attributes, record_info(fields, unit)}]),
    mnesia:create_table(location,     [{disc_copies,[node()]}, {attributes, record_info(fields, location)}]),
    mnesia:create_table(movement,     [{disc_copies,[node()]}, {attributes, record_info(fields, movement)}]),
    mnesia:create_table(pick,         [{disc_copies,[node()]}, {attributes, record_info(fields, pick)}]),
    mnesia:create_table(picklist,     [{disc_copies,[node()]}, {attributes, record_info(fields, picklist)}]),
    mnesia:create_table(articleaudit, [{disc_copies,[node()]}, {attributes, record_info(fields, articleaudit)}]),
    mnesia:create_table(unitaudit,    [{disc_copies,[node()]}, {attributes, record_info(fields, unitaudit)}]),
    mnesia:stop().
    
    
%% Infrastructure for keeping a Queue of what to be moved

add_movementsuggestion(Suggestion) ->
    movementsuggestions ! {self(), {in, Suggestion}}.
    

get_movementsuggestion() ->
    movementsuggestions ! {self(), {out}},
    receive
        Foo ->
            Foo
    end.
    

movementsuggestions_loop_start() ->
    mypl_util:spawn_and_register(movementsuggestions, fun() -> movementsuggestions_loop() end).

movementsuggestions_loop_stop() ->
    movementsuggestions ! {self(), {finished}},
    receive
        {finished, Queue} ->
            io:format("Left over movementsuggestions: ~w~n", [queue:to_list(Queue)])
    end.
    

movementsuggestions_loop() ->
    movementsuggestions_loop(queue:new()).
movementsuggestions_loop(Queue) ->
    receive
        {From, {finished}} ->
            io:format("exiting~n"),
            From ! {finished, Queue}; % return Queue and exit
        {_From, {in, Foo}} ->
            io:format("adding to queue~w~n", [Foo]),
            NewQueue = queue:in(Foo, Queue), 
            movementsuggestions_loop(NewQueue);
        {From, {out}}  ->
            {Ret, NewQueue} = queue:out(Queue),
            io:format("returning ~w~n", [Ret]),
            From ! Ret,
            movementsuggestions_loop(NewQueue)
    end.
    


%%%%
%%%% main myPL API - counting
%%%%

    
