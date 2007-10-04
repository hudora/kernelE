-module(mypl).

-include_lib("/opt/local/lib/erlang/lib/stdlib-1.14.5/include/qlc.hrl").
-include("mypl.hrl").

-export([start/0, stop/0, init_mypl/0, generate_mui/0, store_at_location/5,
count_products/0, count_product/1, 
 
init_movement/2, init_movement_to_good_location/1, commit_movement/1, 
get_mui_location/1, find_product/1, find_product/2,
find_pick_candidates/2, find_retrival_candidates/2,
movementsuggestions_loop_start/0, add_movementsuggestion/1, get_movementsuggestion/0]).


%% @doc
%% This implements our basic functionality for storing (and finding) things in our warehouse. The basic
%% components are:
%% * units     - things to be moved in the warehouse. Sometimes called UnitLoad. Roughly the same as
%%               an pallets. UnitLoads have an UUID called the MUI (Movable unit ID), a heigth in mm
%%               used to choose a fitting location and possibly a string encoding the product, a quantity
%%               of products.
%% * locations - locations are spaces where unit loads are stored. A location has a name to identify it,
%%               a heigth im mm encoding the maximum size of unit load to be placed there, 
%%               a flag encoding if the location can be accessed without forklift, a preference (locations
%%               with higher preference get filled first), maxunits encoding the maximum number of units
%%               to be placed there and some data on what is actually stored on the location. (TBD).
%%               A location can with preference 0 gets never filled automaticaly.
%% * movements - representing the movement of units between locations. We use the term "Retrival" for
%%               movements out of the Warehouse ("Auslagerung").
%% * picks     - representing products to be removed from an unit. Unit with unfinished picks are
%%               considered unavailable for movements.
%%
%% The usual cycle is:
%% 1. A unit enteres the warehouse and is stored on a special logation (e.g. "EINLAG").
%% 2. A movement is generated from EINLA to a suitable location somewhere in the warehouse.
%% 3. The unit is physically moved, the software is informed if the physical process is finished.
%% 4. The movement is marked as "done" the unit is now stored on the new location.
%%
%% api:
%% store_at_location - generates a unit and stores product at a location
%% count_product(product)
%% count_products

%% store - stores product at a location choosen by the system


start() ->
    mnesia:start().
    % Pid = spawn(?MODULE, movementsuggestions_loop_start, []),
    % register(movementsuggestions, Pid).
    

stop() ->
    % movementsuggestions_loop_stop(),
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
    

add_movementsuggestion(Foo) ->
    movementsuggestions ! {self(), {in, Foo}}.
    

get_movementsuggestion() ->
    movementsuggestions ! {self(), {out}},
    receive
        Foo ->
            Foo
    end.
    

movementsuggestions_loop_start() ->
    movementsuggestions_loop(queue:new()).
    

movementsuggestions_loop_stop() ->
    movementsuggestions ! {self(), {finished}},
    receive
        {finished, Queue} ->
            io:format("Left over movementsuggestions: ~w~n", [queue:to_list(Queue)])
    end.
    

movementsuggestions_loop(Queue) ->
    receive
        {From, {finished}} ->
            io:format("exiting~n"),
            From ! {finished, Queue}; % return Queue and exit
        {_From, {in, Foo}} ->
            io:format("adding ~w~n", [Foo]),
            NewQueue = queue:in(Foo, Queue), 
            movementsuggestions_loop(NewQueue);
        {From, {out}}  ->
            {Ret, NewQueue} = queue:out(Queue),
            io:format("returning ~w~n", [Ret]),
            From ! Ret,
            movementsuggestions_loop(NewQueue)
    end.
    

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
                        io:format("undef~n"),
                        Newloc = Location#location{allocated_by=[Unitload#unit.mui]};
                    true ->
                        io:format("true~n"),
                        Newloc = Location#location{allocated_by=[Unitload#unit.mui|Location#location.allocated_by]};
                    L ->
                        io:format("L~n"),
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
    

% move something in the warehouse
% this assumed to be called inside a mnesia transaction
teleport(Unit, Source, Destination) ->
    Newsource = Source#location{allocated_by=lists:filter(fun(X) -> X /= Unit#unit.mui end,
                                                          Source#location.allocated_by)},
    mnesia:write(Newsource),
    Newdestination = Destination#location{allocated_by=Destination#location.allocated_by ++ [Unit#unit.mui],
                                          reserved_for=lists:filter(fun(X) -> X /= Unit#unit.mui end,
                                          Source#location.reserved_for)},
    mnesia:write(Newdestination).
    

count_product_helper([]) -> 0;
count_product_helper(L) ->
    [P|T] = L,
    P#unit.quantity + count_product_helper(T).
count_product(Product) ->
    L = do(qlc:q([X || X <- mnesia:table(unit), X#unit.product =:= Product])),
    count_product_helper(L).
    

count_products([], D) -> dict:to_list(D);
count_products(L, D) ->
    [P|T] = L,
    NewDict = dict:update_counter(P#unit.product, P#unit.quantity, D),
    count_products(T, NewDict).
count_products() ->
    L = do(qlc:q([X || X <- mnesia:table(unit)])),
    D = dict:new(),
    count_products(L, D).
    

% finds the location a unit is currently placed
get_mui_location(Mui) ->
    Locations = do(qlc:q([X || X <- mnesia:table(location), X#location.allocated_by /= []])),
    [H|[]] = lists:filter(fun(X) -> lists:member(Mui, X#location.allocated_by) end, Locations),
    H.
    

% checks if a Unit can be moved
unit_movable(Unit) ->
    % TODO: Check if there is no other open movements and no open picks.
    case Unit#unit.pick_quantity of
        0 -> is_movable;
        true -> false
    end.
    

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
    

find_pick_candidates_floorlevel(Quantity, Product) ->
    Candidates = do(qlc:q([X || X <- mnesia:table(unit), X#unit.product =:= Product,
                                                         X#unit.quantity - X#unit.pick_quantity >= Quantity])),
    lists:keysort(#unit.created_at, lists:filter(fun(X) -> Loc = get_mui_location(X#unit.mui), 
                                                          Loc#location.floorlevel =:= true
                                                    end, 
                                                Candidates)).
    
% finds unitloads from whom Quantity Produkts can be taken.
find_pick_candidates(Quantity, Product) ->
    find_pick_candidates_floorlevel(Quantity, Product).
    

find_retrival_candidates(Quantity, Product) ->
    mypl_util:choose(do(qlc:q([X#unit.quantity || X <- mnesia:table(unit), X#unit.product =:= Product,
                               X#unit.quantity =< Quantity, X#unit.pick_quantity < 1])),
                     Quantity).
  


find_product(Product) ->
  do(qlc:q([X#unit.mui || X <- mnesia:table(unit), X#unit.product =:= Product])).
find_product(Product, Quantity) ->
  do(qlc:q([X#unit.mui || X <- mnesia:table(unit), X#unit.product =:= Product,
                                                   X#unit.quantity =:= Quantity])).
    

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
    

% finds the best location for an Unit
best_location(Unit) ->
    Candidates = find_empty_location(Unit#unit.height),
    % order by heigth, so we prefer lower locations
    [H|_] = lists:keysort(#location.height, Candidates),
    H.
    

