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
%%%% main myPL API - storage
%%%%

%% @private
%% this actualy creates new products in the warehouse
store_at_location(Location, Unit) when Unit#unit.quantity > 0 ->
    Fun = fun() ->
        % check no unit record with this mui exists
        case mnesia:read({unit, Unit#unit.mui}) of
            [_ExistingUnitload] ->
                {error, duplicate_mui};
            [] ->
                % generate unit record
                mnesia:write(Unit),
                case Location#location.allocated_by of
                    undefined ->
                        Newloc = Location#location{allocated_by=[Unit#unit.mui]};
                    _ ->
                        Newloc = Location#location{allocated_by=[Unit#unit.mui|Location#location.allocated_by]}
                end,
            mnesia:write(Newloc),
            log_articleaudit(Unit#unit.quantity, Unit#unit.product,
                             "Warenzugang auf " ++ Location#location.name, Unit#unit.mui),
            {ok, Unit#unit.mui}
        end
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

%%% @type locationName() = string().
%%%     Unique, human readable name of an location.

%%% @type muID() = string().
%%%     MovableUnitID, unique id of an Unit. Often an SSCC/NVE.

%%% @type movementID() = string().
%%%     MoventID, unique id of an movement.

%%% @type heigthMM() = integer().
%%%     Heigth of an Unit or Location im mm. If unsure it is suggested that you choose 1950.


%% @doc create a new Unit and insert it into the warehouse
%% @spec store_at_location(locationName(), muID(), integer(), string(), heigthMM()) -> muID()
store_at_location(Locname, Mui, Quantity, Product, Height) when Quantity > 0 ->
    Fun = fun() ->
        % check that location exists
        case mnesia:read({location, Locname}) of
            [] ->
                {error, unknown_location};
            [Location] ->
                Unit = #unit{mui=Mui, quantity=Quantity, product=Product, height=Height, pick_quantity=0,
                             created_at=calendar:universal_time()},
                log_unitaudit(Unit, "Erzeugt auf " ++ Location#location.name),
                store_at_location(Location, Unit)
        end
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

%% @spec retrive(muID()) -> locationName()
%% @doc remove a Unit and the goods on it from the warehouse
%%
%% returns the name of the location from where the Unit was removed
retrive(Mui) ->
    Fun = fun() ->
        Location = get_mui_location(Mui),
        [Unit] = mnesia:read({unit, Mui}),
        % TODO: check no open pichs  exist.
        
        % update location
        NewLocation = Location#location{allocated_by=Location#location.allocated_by--[Unit#unit.mui]},
        mnesia:write(NewLocation),
        % delete unit record
        mnesia:delete(unit, Mui),
        % log
        log_unitaudit(Unit, "Aufgeloeesst auf " ++ Location#location.name),
        log_articleaudit(Unit#unit.quantity, Unit#unit.product,
                         "Warenzugang auf " ++ Location#location.name, Unit#unit.mui),
        NewLocation#location.name
    end,
    mnesia:transaction(Fun).
    

%%%%
%%%% main myPL API - movement
%%%%

%% @spec init_movement(muID(), locationName()) -> movementID()
%% @doc start moving a unit from its current location to a new one
init_movement(Mui, DestinationName) ->
    Fun = fun() ->
        % get unit for Mui & get current location of mui
        [Unit] = mnesia:read({unit, Mui}),        
        Source = get_mui_location(Mui),
        % check destination exists
        [Destination] = mnesia:read({location, DestinationName}),
        % check that mui is movable
        is_movable = unit_movable(Unit),
        
        % now we can write to the database
        mnesia:write(Destination#location{reserved_for=Destination#location.reserved_for ++ [Mui]}),
        % generate movement record
        Movement = #movement{id=mypl_util:oid(), mui=Mui,
                             from_location=Source#location.name,
                             to_location=Destination#location.name},
        mnesia:write(Movement),
        log_unitaudit(Unit, "Umlagerung von " ++ Source#location.name ++ " nach "
                      ++ Destination#location.name ++ " initialisiert", Movement#movement.id),
        Movement#movement.id
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    {ok, Ret}.
    

%% @spec init_movement_to_good_location(muID()) -> movementID()
%% @see best_location/1
%% @doc start moving a Unit to a location choosen automatically
%%
%% The system chooses the (hopfully) best fitting location for the Unit and then uses {@link init_movement/2}
%% to initiate a movement of the Unit tu that Location.
init_movement_to_good_location(Mui) ->
    % chooses the best location for an MUI and starts moving it there
    Fun = fun() ->
        [Unit] = mnesia:read({unit, Mui}),
        Destination = best_location(Unit),
        init_movement(Mui, Destination#location.name)
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

%% @spec commit_movement(movementID()) -> locationName()
%% @see rollback_movement/1
%% @doc finish a movement
%%
%% Commits a movement created previously by {@link init_movement/2}. This is by doing the actual
%% bookkeping of storing the unit on the new location and removing all previous information on the
%% formerly unfinished movement. Returns the name of the Location where the Unit is stored now.
commit_movement(MovementId) ->
    Fun = fun() ->
        [Movement] = mnesia:read({movement, MovementId}),
        % get unit for Mui & get current location of mui
        [Unit] = mnesia:read({unit, Movement#movement.mui}),
        Source = get_mui_location(Movement#movement.mui),
        % TODO: check Movement#movement.from_location = Source#location.name,
        % check destination exists
        [Destination] = mnesia:read({location, Movement#movement.to_location}),
        % change locationdatat
        teleport(Unit, Source, Destination),
        % delete movement
        mnesia:delete({movement, MovementId}),
        log_unitaudit(Unit, "Umlagerung von " ++ Source#location.name ++ " nach "
                      ++ Destination#location.name ++ " comitted", Movement#movement.id),
        Destination#location.name
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    {ok, Ret}.
    

%% @spec rollback_movement(movementID()) -> locationName()
%% @see commit_movement/1
%% @doc rollback a movement
%%
%% This rolls back a movement returning the ware house to a state as if {@link init_movement/2) had
%% never been called. Returns the name of the Location where the Unit now is placed again.
rollback_movement(MovementId) ->
    Fun = fun() ->
        [Movement] = mnesia:read({movement, MovementId}),
        % get unit for Mui & get current location of mui
        [Unit] = mnesia:read({unit, Movement#movement.mui}),
        Source = get_mui_location(Movement#movement.mui),
        % fix destination
        [Destination] = mnesia:read({location, Movement#movement.to_location}),
        Newdestination = Destination#location{reserved_for=lists:filter(fun(X) -> X /= Unit#unit.mui end,
                                                                        Destination#location.reserved_for)},
        mnesia:write(Newdestination),
        % delete movement
        mnesia:delete({movement, MovementId}),
        log_unitaudit(Unit, "Umlagerung von " ++ Source#location.name ++ " nach "
                      ++ Destination#location.name ++ " abgebrochen", Movement#movement.id),
        Destination#location.name
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    {ok, Ret}.


% start a pick
% @private
init_pick(Quantity, Mui) when is_integer(Quantity) ->
    Fun = fun() ->
        % get unit for Mui & get current location of mui
        [Unit] = mnesia:read({unit, Mui}),
        UnitPickQuantity = Unit#unit.pick_quantity + Quantity,
        if
            UnitPickQuantity > Unit#unit.quantity ->
                % this really shouldn't happen
                {error, not_enough_goods};
            true ->
                % update Unit
                mnesia:write(Unit#unit{pick_quantity=UnitPickQuantity}),
                % generate Pick
                Pick = #pick{id=mypl_util:oid(), quantity=Quantity,
                             product=Unit#unit.product, from_unit=Unit#unit.mui},
                mnesia:write(Pick),
                {ok, Pick#pick.id}
        end
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

% dcommit a pick - this actualy makes goods vanish from the warehouse!
% @private
commit_pick() ->
    [].


% move something in the warehouse - internal use only
% this assumed to be called inside a mnesia transaction
% @private
teleport(Unit, Source, Destination) ->
    Newsource = Source#location{allocated_by=lists:filter(fun(X) -> X /= Unit#unit.mui end,
                                                          Source#location.allocated_by)},
    mnesia:write(Newsource),
    Newdestination = Destination#location{allocated_by=Destination#location.allocated_by ++ [Unit#unit.mui],
                                          reserved_for=lists:filter(fun(X) -> X /= Unit#unit.mui end,
                                          Destination#location.reserved_for)},
    mnesia:write(Newdestination).


%%%%
%%%% main myPL API - counting
%%%%

% internal use only
% @private
count_product_helper([]) -> 0;
count_product_helper(L) ->
    [P|T] = L,
    P#unit.quantity + count_product_helper(T).
% get the Quantity of Product
count_product(Product) ->
    L = do(qlc:q([X || X <- mnesia:table(unit), X#unit.product =:= Product])),
    count_product_helper(L).
    

% internal use only
% @private
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
    

% TODO: this ignores Multi Unit Locations
% @private
find_empty_location(Height) ->
    lists:reverse(lists:keysort(#location.preference, 
                                lists:keysort(#location.name,
                                              do(qlc:q([X || X <- mnesia:table(location), 
                                               X#location.height >= Height, 
                                               X#location.allocated_by =:= [], 
                                               X#location.reserved_for =:= [], 
                                               X#location.preference > 0]))))).
    

% @private
find_empty_floor_location(Height) ->
    lists:filter(fun(X) -> X#location.floorlevel =:= true end, find_empty_location(Height)).
    

% finds the best location for an Unit - used by initiate_movement_to_good_location
% @private
best_location(Unit) ->
    Candidates = find_empty_location(Unit#unit.height),
    % order by heigth, so we prefer lower locations
    [H|_] = lists:keysort(#location.height, Candidates),
    H.
    

% finds the location a unit is currently placed
% @private
get_mui_location(Mui) ->
    Locations = do(qlc:q([X || X <- mnesia:table(location), X#location.allocated_by /= []])),
    [H|[]] = lists:filter(fun(X) -> lists:member(Mui, X#location.allocated_by) end, Locations),
    H.

%% checks if a Unit can be moved, returns is_movable if so, else false
% @private
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
% @private
find_movable_units(Product) ->
    Candidates = do(qlc:q([X || X <- mnesia:table(unit), X#unit.product =:= Product,
                                                         X#unit.pick_quantity =< 0])),
    lists:filter(fun(X) -> unit_movable(X) =:= is_movable end, Candidates).
    

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


% auditlog - to be called whenever goods enter or leave the warehouse
% @private
log_articleaudit(Quantity, Product, Text, Mui, Transaction, References) ->
    Fun = fun() ->
            % check that location exists
            mnesia:write(#articleaudit{id="a" ++ mypl_util:oid(), quantity=Quantity, product=Product,
                                   text=Text, mui=Mui, transaction=Transaction,
                                   references=References, created_at=calendar:universal_time()})
          end,
    mnesia:transaction(Fun).
% @private
log_articleaudit(Quantity, Product, Text, Mui, Transaction) ->
    log_articleaudit(Quantity, Product, Text, Mui, Transaction, []).
% @private
log_articleaudit(Quantity, Product, Text, Mui) ->
    log_articleaudit(Quantity, Product, Text, Mui, undefined).
% @private
log_articleaudit(Quantity, Product, Text) ->
    log_articleaudit(Quantity, Product, Text, undefined).
% @private
log_articleaudit(Quantity, Product) ->
    log_articleaudit(Quantity, Product, "").

% unitaudit - to be called whenever Units are moved in the warehouse
% @private
log_unitaudit(Unit, Text, Transaction, References) ->
    Fun = fun() ->
            % check that location exists
            mnesia:write(#unitaudit{id="A" ++ mypl_util:oid(),
                                    mui=Unit#unit.mui, quantity=Unit#unit.quantity, product=Unit#unit.product,
                                    text=Text, transaction=Transaction,
                                    references=References, created_at=calendar:universal_time()})
          end,
    mnesia:transaction(Fun).
% @private
log_unitaudit(Unit, Text, Transaction) ->
    log_unitaudit(Unit, Text, Transaction, []).
% @private
log_unitaudit(Unit, Text) ->
    log_unitaudit(Unit, Text, undefined).
