%% @version 0.1
%% @copyright 2007 HUDORA GmbH
%% @author Maximillian Dornseif <md@hudora.de>
%% @doc myPL/kernel-E storage Engine
%%
%% This implements the main database functionallity of myPL/kernel-E and is the only module which is allowed
%% to actually write to the main databse. All other modules have read only access to the database.
%%
%% @end

-module(mypl_db).

-include_lib("/opt/local/lib/erlang/lib/stdlib-1.14.5/include/qlc.hrl").
-include("include/mypl.hrl").

-export([run_me_once/0, store_at_location/5, retrive/1,
 init_movement/2, init_movement_to_good_location/1, commit_movement/1, rollback_movement/1,
 init_pick/2, commit_pick/1, rollback_pick/1]).


init_table_info(Status, TableName) ->
    case Status of
        {atomic, ok} ->
            ?DEBUG("table '~w' created", [TableName]);
        {aborted, {already_exists, TableName}} ->
            ?DEBUG("using existing table '~w'", [TableName]);
        _ ->
            ?ERROR("cannot create table '~w'", [TableName], Status)
    end,
    Status.
    
%% @doc should be run before mnesia is started for the first time.
run_me_once() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    % the main tables are koept in RAM with a disk copy for fallback
    init_table_info(mnesia:create_table(location,     [{disc_copies, [node()]}, {attributes, record_info(fields, location)}]), location),
    init_table_info(mnesia:create_table(unit,         [{disc_copies, [node()]}, {attributes, record_info(fields, unit)}]), unit),
    init_table_info(mnesia:create_table(movement,     [{disc_copies, [node()]}, {attributes, record_info(fields, movement)}]), movement),
    init_table_info(mnesia:create_table(pick,         [{disc_copies, [node()]}, {attributes, record_info(fields, pick)}]), pick),
    init_table_info(mnesia:create_table(picklist,     [{disc_copies, [node()]}, {attributes, record_info(fields, picklist)}]), picklist),
    % the audit tables are kept ONLY on disk (slow!)
    init_table_info(mnesia:create_table(articleaudit, [{disc_only_copies, [node()]}, {attributes, record_info(fields, articleaudit)}]), articleaudit),
    init_table_info(mnesia:create_table(unitaudit,    [{disc_only_copies, [node()]}, {attributes, record_info(fields, unitaudit)}]), unitaudit),
    ok = mnesia:wait_for_tables([location, unit, movement, pick, picklist, articleaudit, unitaudit], 5000),
    ok.
    


%%%%
%%%% main myPL API - storage
%%%%

%%% @type locationName() = string().
%%%     Unique, human readable name of an location.

%%% @type muID() = string().
%%%     MovableUnitID, unique id of an Unit. Often an SSCC/NVE.

%%% @type movementID() = string().
%%%     MoventID, unique id of an movement.

%%% @type pickID() = string().
%%%     PickID, unique id of an pick.

%%% @type heigthMM() = integer().
%%%     Heigth of an Unit or Location im mm. If unsure it is suggested that you choose 1950.

%%% @type product() = string().
%%%     Opaque ID for an product. Artikelnummer/Item Number/SKU or EAN.


%%% @type locationRecord() = tuple().
%%%     A record describing a Location.

%%% @type unitRecord() = tuple().
%%%     A record describing a Unit.

%%% @type externalReferences() = List.
%%%       List = [{string(), string()}]
%%% A list of two-tuples encoding external references.





%% @private
%% @spec store_at_location(locationRecord(), unitRecord()) -> {ok, locationName()}
%% @doc create a new Unit and insert it into the warehouse
%%
%% This is an private helper for {@link store_at_location/5} actually saving the Unit to the Location.
%% On errors it might also return {error, duplicate_mui}.
store_at_location(Location, Unit) when Unit#unit.quantity > 0 ->
    Fun = fun() ->
        % check no unit record with this mui exists
        case mnesia:read({unit, Unit#unit.mui}) of
            [_ExistingUnitload] ->
                {error, duplicate_mui, {Location, Unit}};
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
            mypl_audit:articleaudit(Unit#unit.quantity, Unit#unit.product,
                             "Warenzugang von " ++ integer_to_list(Unit#unit.quantity) ++ "*" 
                             ++ Unit#unit.product ++ " auf " ++ Location#location.name, Unit#unit.mui),
            {ok, Unit#unit.mui}
        end
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

%% @spec store_at_location(locationName(), muID(), integer(), string(), heigthMM()) -> {ok, locationName()}
%% @doc create a new Unit and insert it into the warehouse
%%
%% On errors it might also return {error, unknown_location} or {error, duplicate_mui}.
store_at_location(Locname, Mui, Quantity, Product, Height) when Quantity > 0 ->
    Fun = fun() ->
        % check that location exists
        case mnesia:read({location, Locname}) of
            [] ->
                {error, unknown_location, {Locname}};
            [Location] ->
                Unit = #unit{mui=Mui, quantity=Quantity, product=Product, height=Height, pick_quantity=0,
                             created_at=calendar:universal_time()},
                mypl_audit:unitaudit(Unit, "Erzeugt auf " ++ Location#location.name),
                store_at_location(Location, Unit)
        end
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

%% @spec retrive(muID()) -> {ok, {Quantity::integer(), Product::string()}}
%% @doc remove a Unit and the goods on it from the warehouse
%%
%% This actually makes goods vanish from the warehouse!
%% returns the name of the location from where the Unit was removed
retrive(Mui) ->
    Fun = fun() ->
        Location = mypl_db_util:get_mui_location(Mui),
        Unit = mypl_db_util:mui_to_unit(Mui),
        % TODO: check no open picks exist.
        
        % update location
        NewLocation = Location#location{allocated_by=Location#location.allocated_by--[Unit#unit.mui]},
        mnesia:write(NewLocation),
        % delete unit record
        mnesia:delete({unit, Mui}),
        % log
        mypl_audit:unitaudit(Unit, "Aufgeloeesst auf " ++ Location#location.name),
        mypl_audit:articleaudit(-1 * Unit#unit.quantity, Unit#unit.product,
                         "Warenabgang auf " ++ Location#location.name, Unit#unit.mui),
        {ok, {Unit#unit.quantity, Unit#unit.product}}
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.

%%%%
%%%% main myPL API - movement
%%%%

%%% @spec init_movement(muID(), locationName()) -> movementID()
%%% @see commit_movement/1
%%% @doc start moving a unit from its current location to a new one
init_movement(Mui, DestinationName) ->
    Fun = fun() ->
        % get unit for Mui & get current location of mui
        Unit = mypl_db_util:mui_to_unit(Mui),
        Source = mypl_db_util:get_mui_location(Mui),
        % check destination exists
        [Destination] = mnesia:read({location, DestinationName}),
        % check that mui is movable
        case mypl_db_util:unit_movable(Unit) of
            no ->
                {error, not_movable, {Mui}};
            yes ->
                % now we can write to the database
                mnesia:write(Destination#location{reserved_for=Destination#location.reserved_for ++ [Mui]}),
                % generate movement record
                Movement = #movement{id=mypl_util:oid(), mui=Mui,
                                     from_location=Source#location.name,
                                     to_location=Destination#location.name},
                mnesia:write(Movement),
                mypl_audit:unitaudit(Unit, "Umlagerung von " ++ Source#location.name ++ " nach "
                              ++ Destination#location.name ++ " initialisiert", Movement#movement.id),
                {ok, Movement#movement.id}
        end
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

%% @spec init_movement_to_good_location(muID()) -> movementID()
%% @see mypl_db_util:best_location/1
%% @doc start moving a Unit to a location choosen automatically
%%
%% The system chooses the (hopfully) best fitting location for the Unit and then uses {@link init_movement/2}
%% to initiate a movement of the Unit tu that Location.
init_movement_to_good_location(Mui) ->
    % chooses the best location for an MUI and starts moving it there
    Fun = fun() ->
        Unit = mypl_db_util:mui_to_unit(Mui),
        Destination = mypl_db_util:best_location(Unit),
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
        Unit = mypl_db_util:mui_to_unit(Movement#movement.mui),
        Source = mypl_db_util:get_mui_location(Movement#movement.mui),
        % TODO: check Movement#movement.from_location = Source#location.name,
        % check destination exists
        [Destination] = mnesia:read({location, Movement#movement.to_location}),
        % change locationdatat
        teleport(Unit, Source, Destination),
        % delete movement
        mnesia:delete({movement, MovementId}),
        mypl_audit:unitaudit(Unit, "Umlagerung von " ++ Source#location.name ++ " nach "
                      ++ Destination#location.name ++ " comitted", Movement#movement.id),
        Destination#location.name
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    {ok, Ret}.
    

%% @spec rollback_movement(movementID()) -> locationName()
%% @see commit_movement/1
%% @doc rollback a movement
%%
%% This rolls back a movement returning the warehouse to a state as if {@link init_movement/2} had
%% never been called. Returns the name of the Location where the Unit now is placed (again).
rollback_movement(MovementId) ->
    Fun = fun() ->
        [Movement] = mnesia:read({movement, MovementId}),
        % get unit for Mui & get current location of mui
        Unit = mypl_db_util:mui_to_unit(Movement#movement.mui),
        Source = mypl_db_util:get_mui_location(Movement#movement.mui),
        % fix destination
        [Destination] = mnesia:read({location, Movement#movement.to_location}),
        Newdestination = Destination#location{reserved_for=lists:filter(fun(X) -> X /= Unit#unit.mui end,
                                                                        Destination#location.reserved_for)},
        mnesia:write(Newdestination),
        % delete movement
        mnesia:delete({movement, MovementId}),
        mypl_audit:unitaudit(Unit, "Umlagerung von " ++ Source#location.name ++ " nach "
                      ++ Destination#location.name ++ " abgebrochen", Movement#movement.id),
        Source#location.name
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    {ok, Ret}.


%% @spec init_pick(integer(), muID()) -> {ok, pickID()}
%% @see commit_pick/1
%% @doc start a new pick removing Quantity Products from Mui
init_pick(Quantity, Mui) when is_integer(Quantity) ->
    Fun = fun() ->
        % get unit for Mui & get current location of mui
        Unit = mypl_db_util:mui_to_unit(Mui),
        UnitPickQuantity = Unit#unit.pick_quantity + Quantity,
        if
            UnitPickQuantity > Unit#unit.quantity ->
                % this really shouldn't happen
                {error, not_enough_goods, {Quantity, Mui, UnitPickQuantity}};
            true ->
                % update Unit
                mnesia:write(Unit#unit{pick_quantity=UnitPickQuantity}),
                % generate Pick
                Pick = #pick{id=mypl_util:oid(), quantity=Quantity,
                             product=Unit#unit.product, from_unit=Unit#unit.mui},
                mnesia:write(Pick),
                mypl_audit:unitaudit(Unit, "Pick von " ++ integer_to_list(Pick#pick.quantity) 
                                     ++ " initialisiert. Verfügbarer Bestand " 
                                     ++ integer_to_list(Unit#unit.quantity-UnitPickQuantity),
                                     Pick#pick.id),
                {ok, Pick#pick.id}
        end
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

%% @spec commit_pick(pickID()) -> {ok, {Quantity::integer(), Product::string()}}
%% @see rollback_pick/1
%% @doc finish a a pick - this actually makes goods vanish from the warehouse!
%%
%% Commits a pick created previously by {@link init_pick/2}. This is by doing the actual
%% bookkeping of removing the goods from the warehouse. Returns the name of the Location
%% where the goods where removed from.
commit_pick(PickId) ->
    Fun = fun() ->
        % get Pick for PickId
        [Pick] = mnesia:read({pick, PickId}),
        Unit = mypl_db_util:mui_to_unit(Pick#pick.from_unit),
        NewUnit = Unit#unit{quantity=Unit#unit.quantity - Pick#pick.quantity,
                            pick_quantity=Unit#unit.pick_quantity - Pick#pick.quantity},
        if
            NewUnit#unit.quantity < 0 ->
                % this really shouldn't happen
                {error, not_enough_goods, {Pick#pick.quantity, PickId, Unit#unit.mui}};
            true ->
                % update Unit
                mnesia:write(NewUnit),
                
                % delete pick
                mnesia:delete({pick, PickId}),
                mypl_audit:articleaudit(-1 * Pick#pick.quantity, Pick#pick.product,
                                        "Pick auf " ++ Unit#unit.mui, Unit#unit.mui, PickId),
                mypl_audit:unitaudit(Unit, "Pick von " ++ integer_to_list(Pick#pick.quantity) 
                                     ++ " committed. neuer Bestand " ++ integer_to_list(Unit#unit.quantity),
                                     PickId),
            {Pick#pick.quantity, Pick#pick.product}
        end
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    {ok, Ret}.
    

%% @spec rollback_pick(pickId()) -> {ok, locationId()}
%% @see init_pick/2
%% @doc rollback a pick
%%
%% This rolls back a pick returning the warehouse to a state as if {@link init_pick/2} had
%% never been called. Returns the name of the Location where the Unit now is (again) in it's original state.
rollback_pick(PickId) ->
    Fun = fun() ->
        % get Pick for PickId
        [Pick] = mnesia:read({pick, PickId}),
        Unit = mypl_db_util:mui_to_unit(Pick#pick.from_unit),
        NewUnit = Unit#unit{pick_quantity=Unit#unit.quantity - Pick#pick.quantity},
        if
            NewUnit#unit.pick_quantity < 0 ->
                % this really shouldn't happen
                {error, internal_error, {Pick, Unit}};
            true ->
                % update Unit
                mnesia:write(NewUnit),
                
                % delete pick
                mnesia:delete({pick, PickId}),
                mypl_audit:unitaudit(Unit, "Pick von " ++ integer_to_list(Pick#pick.quantity) 
                                     ++ " abgebrochen.", PickId),
            {Pick#pick.quantity, Pick#pick.product}
        end
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    {ok, Ret}.


%% @private
%% @spec teleport(unitRecord(), locationRecord(), locationRecord()) -> term()
%% @doc move a mui in the warehouse - internal use only
%%
%% this assumed to be called inside a mnesia transaction
teleport(Unit, Source, Destination) ->
    Newsource = Source#location{allocated_by=lists:filter(fun(X) -> X /= Unit#unit.mui end,
                                                          Source#location.allocated_by)},
    mnesia:write(Newsource),
    Newdestination = Destination#location{allocated_by=Destination#location.allocated_by ++ [Unit#unit.mui],
                                          reserved_for=lists:filter(fun(X) -> X /= Unit#unit.mui end,
                                          Destination#location.reserved_for)},
    mnesia:write(Newdestination).




% ~~ Unit tests
-ifdef(EUNIT).
% -compile(export_all).

%%% @hidden
test_init() ->
    run_me_once(),
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
    {atomic,ok} = mnesia:transaction(fun() ->
        mnesia:write(#location{name="EINLAG", height=6000, floorlevel=true,  preference=0, allocated_by=[], reserved_for=[], attributes=[no_picks]}),
        mnesia:write(#location{name="AUSLAG", height=6000, floorlevel=true,  preference=0, allocated_by=[], reserved_for=[], attributes=[no_picks]}),
        mnesia:write(#location{name="010101", height=2000, floorlevel=true,  preference=6, allocated_by=[], reserved_for=[], attributes=[]}),
        mnesia:write(#location{name="010102", height=1950, floorlevel=false, preference=6, allocated_by=[], reserved_for=[], attributes=[]}),
        mnesia:write(#location{name="010103", height=1200, floorlevel=false, preference=5, allocated_by=[], reserved_for=[], attributes=[]}),
        mnesia:write(#location{name="010201", height=2000, floorlevel=true,  preference=7, allocated_by=[], reserved_for=[], attributes=[]})
    end),
    ok.

%%% @hidden
mypl_simple_movement_test() ->
    test_init(),
    % generate a MUI for testing
    Mui = "14601-" ++ mypl_util:generate_mui(),
    % generate and Store Unit of 5*14601 (1200mm high) on "EINLAG"
    {ok, Mui} = store_at_location("EINLAG", Mui, 5, "14601", 1200),
    
    % start movement to "010101".
    {ok, Movement1} = init_movement(Mui, "010101"),
    % finish movement
    {ok,"010101"} = commit_movement(Movement1),
    % check that Unit now is on the new Location
    Location1 = mypl_db_util:get_mui_location(Mui),
    %?assert(Location1#location.name == "010101"),
    
    % now move it to the best location the system can find for this Unit
    {ok, Movement2} = init_movement_to_good_location(Mui),
    % finish movement & check that Unit now is on the new Location - 010103 is best because it is lowest (1200mm)
    {ok, "010103"} = commit_movement(Movement2),
    
    % now try again to move to a "good" location - with lot's of checks
    {ok, Movement3} = init_movement_to_good_location(Mui),
    % while movement is initialized the unit is still booked on the old location
    Location2 = mypl_db_util:get_mui_location(Mui),
    
    % try to initiate an other movement on that Mui - shouldn't be possible
    {error, not_movable, _} = init_movement(Mui, "010101"),
    Location2 = mypl_db_util:get_mui_location(Mui),
    
    % we rollback the whole thing ... so the unit should still be in it's old location
    {ok, "010103"} = rollback_movement(Movement3),
    
    % check issues with two units
    Mui2 = "14601-" ++ mypl_util:generate_mui(),
    % generate and Store Unit of 6*14601 (1200mm high) on "EINLAG"
    {ok, Mui2} = store_at_location("EINLAG", Mui2, 6, "14601", 1200),
    {ok, Movement4} = init_movement_to_good_location(Mui),
    {ok, Movement5} = init_movement_to_good_location(Mui2),
    commit_movement(Movement5),
    commit_movement(Movement4),
    
    % remove Muis from warehouse
    {ok, {5, "14601"}} = retrive(Mui),
    {ok, {6, "14601"}} = retrive(Mui2).
    

%%% @hidden
mypl_simple_pick_test() ->
    test_init(),
    % generate a MUI for testing
    Mui = "14601-" ++ mypl_util:generate_mui(),
    % generate and Store Unit of 5*14601 (1200mm high) on "010101"
    {ok, Mui} = store_at_location("010101", Mui, 70, "14602", 1950),
    % start picking.
    {ok, Pick1} = init_pick(30, Mui),
    
    %% try to initiate an movement on that Mui - shouldn't be possible with open picks
    {error, not_movable, _} = init_movement_to_good_location(Mui),
    % because of the open pick the unit shouldn't be movable
    no = mypl_db_util:unit_movable(mypl_db_util:mui_to_unit(Mui)),    
    % commit it.
    {ok, {30, "14602"}} = commit_pick(Pick1),
    % now ot should be movable again
    yes = mypl_db_util:unit_movable(mypl_db_util:mui_to_unit(Mui)),
     
    % start picking.
    {ok, Pick2} = init_pick(25, Mui),
    ?DEBUG("Z", []),
    % commit it.
    {ok, {25, "14602"}} = commit_pick(Pick2),
    %TODO: test rollback
    
    % check if enough is left on unit
    {ok, {15, "14602"}} = retrive(Mui).
    

mypl_testrunner() ->
    mypl_simple_movement_test(),
    mypl_simple_pick_test().
    
-endif.
