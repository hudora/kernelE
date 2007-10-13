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

    

%%%%
%%%% main myPL API - retrieval & picks
%%%%

% finds the best retrival candidates - TODO: INTERNAL USE ONLY?
%% returns {ok, [Units]} or {error, reason}
% @private
find_retrival_candidates(Quantity, Units) when is_integer(Quantity), is_list(Units) ->
    CandidateQuantites = mypl_util:choose([X#unit.quantity || X <- Units], Quantity),
    if 
        CandidateQuantites /= [] ->
            [H|_] = CandidateQuantites,
            {ok, find_oldest_units_of(H, Units)};
        true ->
            % TODO: check if we have enough available goods in stock
            {error, no_fit}
    end.
    

% TODO: INTERNAL USE ONLY?
% @private
find_pick_candidate_floorlevel(Quantity, Product) ->
    Candidates = do(qlc:q([X || X <- find_pickable_units(Product),
                                X#unit.quantity - X#unit.pick_quantity >= Quantity])),
    % sort candidates and remove non-floor units
    Sorted = lists:keysort(#unit.created_at,
                           lists:filter(fun(X) -> Loc = get_mui_location(X#unit.mui), 
                                                  Loc#location.floorlevel =:= true
                                        end, 
                           Candidates)),
    % prefer candidates already having open picks
    case [X#unit.mui || X <- Sorted, X#unit.pick_quantity > 0] ++ [X#unit.mui || X <- Sorted, X#unit.pick_quantity =< 0] of
        [] -> 
            add_movementsuggestion({Quantity, Product}),
            {error, no_fit};
        [Mui|_] ->
            {ok, Mui}
    end.
    

% finds unitloads from whom Quantity Produkts can be taken.
% @private
find_pick_candidate(Quantity, Product) ->
    find_pick_candidate_floorlevel(Quantity, Product).
    

%% find a combination of retrivals and picks to fullfill a order
%% returns {ok, retrivals, picks}
find_provisioning_candidates(Quantity, Product) ->
    % We can get goods from 
    RetrivalCandidates = find_retrievable_units(Product),
    case find_retrival_candidates(Quantity, RetrivalCandidates) of
        {ok, Candidates} ->
            % we found a direct fit
            {ok, [X#unit.mui || X <- Candidates], []};
        {error, no_fit} ->
            % no direct match. So we need to come up with a mix of retrievals and picks
            % we use a stupid strategy here: take units starting from the oldest unit and fill the
            % remainder from the "fixplatz".
            % the current interface supports more than one pick but we don't implement that
            {RestQuantity, Candidates} = find_less_than_units_of(Quantity, RetrivalCandidates),
            case find_pick_candidate(RestQuantity, Product) of
                {ok, PickMui} ->
                    {ok, [X#unit.mui || X <- Candidates], [{PickMui, RestQuantity}]};
                {error, no_fit} ->
                    {error, no_fit}
            end
    end.
    

% database helper
% @private
do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
    


%% returns a list of all movable units for a product
% @private
find_movable_units(Product) ->
    Candidates = do(qlc:q([X || X <- mnesia:table(unit), X#unit.product =:= Product,
                                                         X#unit.pick_quantity =< 0])),
    lists:filter(fun(X) -> unit_movable(X) =:= yes end, Candidates).
    

% @private
unit_pickable_helper(Unit) ->
     Loc = get_mui_location(Unit#unit.mui),
     not(lists:member(no_picks, Loc#location.attributes)).

%% returns a list of all units for a product which can be retrived (no no_picks attribute on location and no open movements)
% @private
find_retrievable_units(Product) ->
    [X || X <- find_movable_units(Product), unit_pickable_helper(X)].

%% returns a list of all units which can be picked (no no_picks attribute on location)
% @private
find_pickable_units(Product) ->
    [X || X <- do(qlc:q([X || X <- mnesia:table(unit), X#unit.product =:= Product])), unit_pickable_helper(X)].
    

% used by find_retrival_candidates
% @private
find_oldest_unit_of(Quantity, Units, Ignore) when is_integer(Quantity), is_list(Units), is_list(Ignore) ->
    L = [X || X <- Units, X#unit.quantity =:= Quantity, X#unit.pick_quantity < 1],
    case lists:keysort(#unit.created_at, L -- Ignore) of
        [] -> [];
        [H|_] -> H
    end.
    
% @private
find_oldest_unit_of(Quantity, Units) when is_integer(Quantity), is_list(Units) ->
    find_oldest_unit_of(Quantity, Units, []).

% @private
find_oldest_units_of([], _Units, _Ignore) -> [];
find_oldest_units_of(Quantities, Units, Ignore) when is_list(Quantities), is_list(Units), is_list(Ignore) ->
    [H|T] = Quantities,
    Mui = find_oldest_unit_of(H, Units, []),
    [Mui|find_oldest_units_of(T, Units, [Mui|Ignore])].
    
% @private
find_oldest_units_of(Quantities, Units) when is_list(Quantities), is_list(Units) ->
    find_oldest_units_of(Quantities, Units, []).


% @private
find_less_than_units_of_helper(Quantity, Units, AccIn) when Quantity > 0 ->
    Candidates = [X || X <- Units, X#unit.quantity =< Quantity],
    if
        Candidates =:= [] ->
            % nothing more to find
            {Quantity, AccIn};
        true ->
            [H|_] = Candidates,
            find_less_than_units_of_helper(Quantity-H#unit.quantity, Units--[H], [H|AccIn])
        end.
% used by find_provisioning_candidates, tries to get as near as possible to Quantity
% returns {Rest, Units} where Rest is quantity which couldn't be satisfied by retrieval
% @private
find_less_than_units_of(Quantity, Units)  when is_integer(Quantity), is_list(Units) ->
    SortedUnits = lists:keysort(#unit.created_at, Units),
    {RestQuantity, RetrivalUnits} = find_less_than_units_of_helper(Quantity, SortedUnits, []),
    {RestQuantity, lists:reverse([X#unit.mui || X <- RetrivalUnits])}.


