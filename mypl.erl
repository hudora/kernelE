-module(mypl).
% -compile(export_all).

-include_lib("/opt/local/lib/erlang/lib/stdlib-1.14.5/include/qlc.hrl").
-include("mypl.hrl").

-export([start/0, stop/0, init_mypl/0, generate_mui/0, store_at_location/5,
count_products/0, count_product/1, 
 
init_movement/2, init_movement_to_good_location/1, commit_movement/1, 
get_mui_location/1, find_product/1,
find_provisioning_candidates/2,

%  for testing
find_less_than_units_of/2,
add_movementsuggestion/1, get_movementsuggestion/0
]).


%% @doc
%% This implements our basic functionality for storing (and finding) things in our warehouse. The basic
%% components are:
%% * units     - things to be moved in the warehouse. Sometimes called UnitLoad. Roughly the same as
%%               an pallets. UnitLoads have an UUID called the MUI (Movable unit ID - maybe it is a
%%               SSCC/NVE), a heigtht in mm used to choose a fitting location and possibly a string
%%               encoding the product, a quantity of products.
%% * locations - locations are spaces where unit loads are stored. A location has a name to identify it,
%%               a heigth im mm encoding the maximum size of unit load to be placed there, 
%%               a flag encoding if the location can be accessed without forklift, a preference (locations
%%               with higher preference get filled first), maxunits encoding the maximum number of units
%%               to be placed there and some data on what is actually stored on the location. (TBD).
%%               A location can with preference 0 gets never filled automaticaly.
%% * movements - representing the movement of units between locations. We use the term "retrieval" for
%%               movements out of the Warehouse ("Auslagerung").
%% * picks     - representing products to be removed from an unit. Unit with unfinished picks are
%%               considered unavailable for movements. We use the term "provisionings" to describe
%%               picks and retrievals.
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
    mnesia:create_table(unit,     [{disc_copies,[node()]}, {attributes, record_info(fields, unit)}]),
    mnesia:create_table(location, [{disc_copies,[node()]}, {attributes, record_info(fields, location)}]),
    mnesia:create_table(movement, [{disc_copies,[node()]}, {attributes, record_info(fields, movement)}]),
    mnesia:create_table(pick,     [{disc_copies,[node()]}, {attributes, record_info(fields, pick)}]),
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
%%%% main myPL API - storage
%%%%

store_at_location(Location, Unitload) when Unitload#unit.quantity > 0 ->
    Fun = fun() ->
        % check no unit record with this mui exists
        case mnesia:read({unit, Unitload#unit.mui}) of
            [_Unitload] ->
                {error, duplicate_mui};
            [] ->
                % generate unit record
                mnesia:write(Unitload),
                case Location#location.allocated_by of
                    undefined ->
                        Newloc = Location#location{allocated_by=[Unitload#unit.mui]};
                    _ ->
                        Newloc = Location#location{allocated_by=[Unitload#unit.mui|Location#location.allocated_by]}
                end,
            mnesia:write(Newloc),
            {ok, Unitload#unit.mui}
        end
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

store_at_location(Locname, Mui, Quantity, Product, Height) when Quantity > 0 ->
    Fun = fun() ->
        % check that location exists
        case mnesia:read({location, Locname}) of
            [] ->
                {error, unknown_location};
            [Location] ->
                Unitload = #unit{mui=Mui, quantity=Quantity, product=Product, height=Height, pick_quantity=0,
                                 created_at=calendar:universal_time()},
                store_at_location(Location, Unitload)
        end
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

% NEEDED?
retrive(Locname, Mui) ->
    Fun = fun() ->
        % check that location exists
        [Location] = mnesia:read({location, Locname}),
        %% check no unit record with this mui exists
        [_Unit] = mnesia:read({unit, Mui}),
        % delete unit record
        mnesia:delete(unit, Mui),
        % update locatione
        % TODO: fixme for multi locations
        Newloc = Location#location{allocated_by=undefined},
        mnesia:write(Newloc),
        {Newloc}
    end,
    mnesia:transaction(Fun).
    

%%%%
%%%% main myPL API - movement
%%%%

% start moving a unit from its current location to Destination
init_movement(Mui, Destination) ->
    Fun = fun() ->
        % get unit for Mui & get current location of mui
        [Unit] = mnesia:read({unit, Mui}),
        Currentloc = get_mui_location(Mui),
        % check destination exists
        [ToLocation] = mnesia:read({location, Destination}),
        % check that mui is movable
        is_movable = unit_movable(Unit),
        
        % TODO: set reserved_by
        mnesia:write(ToLocation#location{reserved_for=ToLocation#location.reserved_for ++ [Mui]}),
        % generate movement record
        Movement = #movement{id=mypl_util:oid(), mui=Mui,
                             from_location=Currentloc#location.name,
                             to_location=Destination},
        mnesia:write(Movement),
        Movement#movement.id
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    {ok, Ret}.
    

% move to a location choosen automatically
init_movement_to_good_location(Mui) ->
    % chooses the best location for an MUI and starts moving it there
    Fun = fun() ->
        [Unit] = mnesia:read({unit, Mui}),
        Destination = best_location(Unit),
        init_movement(Mui, Destination#location.name)
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

% finish a movement
commit_movement(MovementId) ->
    Fun = fun() ->
        [Movement] = mnesia:read({movement, MovementId}),
        % get unit for Mui & get current location of mui
        [Unit] = mnesia:read({unit, Movement#movement.mui}),
        Source = get_mui_location(Movement#movement.mui),
        % todo: check Movement#movement.from_location = Source#location.name,
        % check destination exists
        [Destination] = mnesia:read({location, Movement#movement.to_location}),
        % change locationdatat
        teleport(Unit, Source, Destination),
        % delete movement
        mnesia:delete({movement, MovementId})
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    {ok, Ret}.

% move something in the warehouse - internal use only
% this assumed to be called inside a mnesia transaction
teleport(Unit, Source, Destination) ->
    Newsource = Source#location{allocated_by=lists:filter(fun(X) -> X /= Unit#unit.mui end,
                                                          Source#location.allocated_by)},
    mnesia:write(Newsource),
    Newdestination = Destination#location{allocated_by=Destination#location.allocated_by ++ [Unit#unit.mui],
                                          reserved_for=lists:filter(fun(X) -> X /= Unit#unit.mui end,
                                          Source#location.reserved_for)},
    mnesia:write(Newdestination).


%%%%
%%%% main myPL API - counting
%%%%

% internal use only
count_product_helper([]) -> 0;
count_product_helper(L) ->
    [P|T] = L,
    P#unit.quantity + count_product_helper(T).
% get the Quantity of Product
count_product(Product) ->
    L = do(qlc:q([X || X <- mnesia:table(unit), X#unit.product =:= Product])),
    count_product_helper(L).
    

% internal use only
count_products_helper([], D) -> dict:to_list(D);
count_products_helper(L, D) ->
    [P|T] = L,
    NewDict = dict:update_counter(P#unit.product, P#unit.quantity, D),
    count_products_helper(T, NewDict).
% get the Quantity of all Products as a dict
count_products() ->
    L = do(qlc:q([X || X <- mnesia:table(unit)])),
    D = dict:new(),
    count_products_helper(L, D).
    

% get (quantity, Product) of all products
find_product(Product) ->
    do(qlc:q([{X#unit.quantity, X#unit.mui} || X <- mnesia:table(unit), X#unit.product =:= Product])).
    

%%%%
%%%% main myPL API - retrieval & picks
%%%%

% finds the best retrival candidates - TODO: INTERNAL USE ONLY?
%% returns {ok, [Units]} or {error, reason}
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
do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
    

% TODO: this ignores Multi Unit Locations
find_empty_location(Height) ->
    lists:reverse(lists:keysort(#location.preference, 
                                lists:keysort(#location.name,
                                              do(qlc:q([X || X <- mnesia:table(location), 
                                               X#location.height >= Height, 
                                               X#location.allocated_by =:= [], 
                                               X#location.reserved_for =:= [], 
                                               X#location.preference > 0]))))).
    

find_empty_floor_location(Height) ->
    lists:filter(fun(X) -> X#location.floorlevel =:= true end, find_empty_location(Height)).
    

% finds the best location for an Unit - used by initiate_movement_to_good_location
best_location(Unit) ->
    Candidates = find_empty_location(Unit#unit.height),
    % order by heigth, so we prefer lower locations
    [H|_] = lists:keysort(#location.height, Candidates),
    H.
    

% finds the location a unit is currently placed
get_mui_location(Mui) ->
    Locations = do(qlc:q([X || X <- mnesia:table(location), X#location.allocated_by /= []])),
    [H|[]] = lists:filter(fun(X) -> lists:member(Mui, X#location.allocated_by) end, Locations),
    H.

%% checks if a Unit can be moved, returns is_movable if so, else false
unit_movable(Unit) ->
    % Check if there is no other open movements and no open picks.
    if 
        % check for no open picks
        Unit#unit.pick_quantity =< 0 -> 
            % check for no open movements
            L = do(qlc:q([X || X <- mnesia:table(movement), X#movement.mui =:= Unit#unit.mui])),
            if
                L =:= [] -> is_movable;
                true -> false
            end;
        true -> false
    end.
    

%% returns a list of all movable units for a product
find_movable_units(Product) ->
    Candidates = do(qlc:q([X || X <- mnesia:table(unit), X#unit.product =:= Product,
                                                         X#unit.pick_quantity =< 0])),
    lists:filter(fun(X) -> unit_movable(X) =:= is_movable end, Candidates).
    

unit_pickable_helper(Unit) ->
     Loc = get_mui_location(Unit#unit.mui),
     not(lists:member(no_picks, Loc#location.attributes)).

%% returns a list of all units for a product which can be retrived (no no_picks attribute on location and no open movements)
find_retrievable_units(Product) ->
    [X || X <- find_movable_units(Product), unit_pickable_helper(X)].

%% returns a list of all units which can be picked (no no_picks attribute on location)
find_pickable_units(Product) ->
    [X || X <- do(qlc:q([X || X <- mnesia:table(unit), X#unit.product =:= Product])), unit_pickable_helper(X)].
    

% used by find_retrival_candidates
find_oldest_unit_of(Quantity, Units, Ignore) when is_integer(Quantity), is_list(Units), is_list(Ignore) ->
    L = [X || X <- Units, X#unit.quantity =:= Quantity, X#unit.pick_quantity < 1],
    case lists:keysort(#unit.created_at, L -- Ignore) of
        [] -> [];
        [H|_] -> H
    end.
    
find_oldest_unit_of(Quantity, Units) when is_integer(Quantity), is_list(Units) ->
    find_oldest_unit_of(Quantity, Units, []).

find_oldest_units_of([], _Units, _Ignore) -> [];
find_oldest_units_of(Quantities, Units, Ignore) when is_list(Quantities), is_list(Units), is_list(Ignore) ->
    [H|T] = Quantities,
    Mui = find_oldest_unit_of(H, Units, []),
    [Mui|find_oldest_units_of(T, Units, [Mui|Ignore])].
    
find_oldest_units_of(Quantities, Units) when is_list(Quantities), is_list(Units) ->
    find_oldest_units_of(Quantities, Units, []).


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
find_less_than_units_of(Quantity, Units)  when is_integer(Quantity), is_list(Units) ->
    SortedUnits = lists:keysort(#unit.created_at, Units),
    {RestQuantity, RetrivalUnits} = find_less_than_units_of_helper(Quantity, SortedUnits, []),
    {RestQuantity, lists:reverse([X#unit.mui || X <- RetrivalUnits])}.


