%% @version 0.2
%% @copyright 2007 HUDORA GmbH
%% @author Maximillian Dornseif <md@hudora.de>
%% @doc myPL/kernel-E storage Engine
%%
%% This implements the main database functionallity of myPL/kernel-E and is the only module which is allowed
%% to actually write to the main databse. All other modules have read only access to the database.
%%
%% Note that {@link commit_retrieve/1} is actually a macro calling {@link commit_movement/1} and
%% {@link retrieve/1}.
%%
%% @end

-module(mypl_db).

-include_lib("stdlib/include/qlc.hrl").
-include("include/mypl.hrl").

-export([init_table_info/2, run_me_once/0, init_location/6, init_location/5,store_at_location/5, retrieve/1,
 init_movement/2, init_movement/3, init_movement_to_good_location/1, commit_movement/1, rollback_movement/1,
 commit_retrieval/1, rollback_retrieval/1,
 init_pick/2, commit_pick/1, rollback_pick/1]).


init_table_info(Status, TableName) ->
    case Status of
        {atomic, ok} ->
            ?WARNING("table '~w' created", [TableName]);
        {aborted, {already_exists, TableName}} ->
            ?DEBUG("using existing table '~w'", [TableName]);
        _ ->
            ?ERROR("cannot create table '~w' -- ~w", [TableName, Status], Status)
    end,
    Status.
    
%% @doc should be run before mnesia is started for the first time.
run_me_once() ->
    % ?WARNING("run_me_once() called", []),
    mnesia:create_schema([node()]),
    mnesia:start(),
    % the main tables are koept in RAM with a disk copy for fallback
    init_table_info(mnesia:create_table(location,         [{disc_copies, [node()]}, {attributes, record_info(fields, location)}]), location),
    init_table_info(mnesia:create_table(unit,             [{disc_copies, [node()]}, {attributes, record_info(fields, unit)}]), unit),
    mnesia:add_table_index(unit, #unit.product),
    init_table_info(mnesia:create_table(movement,         [{disc_copies, [node()]}, {attributes, record_info(fields, movement)}]), movement),
    init_table_info(mnesia:create_table(pick,             [{disc_copies, [node()]}, {attributes, record_info(fields, pick)}]), pick),
    init_table_info(mnesia:create_table(reservation,      [{disc_copies, [node()]}, {attributes, record_info(fields, reservation)}]), reservation),
    
    % give other modules to initialize database tables
    mypl_abcserver:run_me_once(),
    mypl_audit:run_me_once(),
    mypl_provpipeline:run_me_once(),
    
    ok = mnesia:wait_for_tables([location, unit, movement, pick, articleaudit, unitaudit], 30000),
    init_location("EINLAG", 3000, true,  0, [{no_picks}]),
    init_location("AUSLAG", 3000, true,  0, [{no_picks}]),
    init_location("FEHLER", 3000, true,  0, [{no_picks}]),
    init_location("K01",    3000, true,  0, []),
    init_location("K02",    3000, true,  0, []),
    init_location("K03",    3000, true,  0, []),
    init_location("K04",    3000, true,  0, []),
    init_location("K05",    3000, true,  0, []),
    init_location("K06",    3000, true,  0, []),
    init_location("K07",    3000, true,  0, []),
    init_location("K08",    3000, true,  0, []),
    init_location("K09",    3000, true,  0, []),
    init_location("K10",    3000, true,  0, []),
    % init_location("K11",    3000, true,  0, []),
    % init_location("K12",    3000, true,  0, []),
    % init_location("K13",    3000, true,  0, []),
    % init_location("K14",    3000, true,  0, []),
    % init_location("K15",    3000, true,  0, []),
    % init_location("K16",    3000, true,  0, []),
    % init_location("K17",    3000, true,  0, []),
    % init_location("K18",    3000, true,  0, []),
    % init_location("K19",    3000, true,  0, []),
    % init_location("K20",    3000, true,  0, []),
    ok.
    



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


%%%%
%%%% main myPL API - location data
%%%%

%% @spec init_location(locationName(), heigthMM(), boolean(), integer(), string(), list())  -> term()
%% @doc creates a new Location or updates an existing one.
%% 
%% Locations can be created at any time - even when the myPL bristles with activity..
%% There is no way of deleting Locations. Set their preference to 0 and let them rot.
%% returns {ok, created|updated}
init_location(Name, Height, Floorlevel, Preference, Info, Attributes)
    when is_integer(Height), is_boolean(Floorlevel), is_integer(Preference), 
         Preference >= 0, Preference < 10, is_list(Attributes) ->
    KnownAttributes = [{no_picks}],
    case lists:filter(fun(X) -> not lists:member(X, KnownAttributes) end, Attributes) of
        [] ->
            % all attributes are ok
            Fun = fun() ->
                Ret = case mnesia:read({location, Name}) of
                    [] ->
                        Location = #location{allocated_by=[], reserved_for=[]},
                        ?WARNING("Location ~w beeing created", [Name]),
                        created;
                    [Location] ->
                        updated
                end,
                NewLocation = Location#location{name=Name, height=Height, floorlevel=Floorlevel,
                                                preference=Preference,
                                                info=Info,
                                                attributes=Attributes,
                                                allocated_by=Location#location.allocated_by,
                                                reserved_for=Location#location.reserved_for},
                ok = mnesia:write(NewLocation),
                ?WARNING("Location ~w saved", [Name]),
                Ret
            end,
            {atomic, Ret} = mnesia:transaction(Fun),
            {ok, Ret};
        UnknownAttributes ->
            {error, unknown_attributes, {UnknownAttributes}}
    end.
    

%% @deprecated
init_location(Name, Height, Floorlevel, Preference, Attributes) -> 
    init_location(Name, Height, Floorlevel, Preference, "", Attributes).
    

%%%%
%%%% main myPL API - storage
%%%%


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
                ok = mnesia:write(Unit#unit{location = Location#location.name}),
                case Location#location.allocated_by of
                    undefined ->
                        Newloc = Location#location{allocated_by=[Unit#unit.mui]};
                    _ ->
                        Newloc = Location#location{allocated_by=[Unit#unit.mui|Location#location.allocated_by]}
                end,
            ok = mnesia:write(Newloc),
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
        Location = mypl_db_util:read_location(Locname),
        Unit = #unit{mui=Mui, quantity=Quantity, product=Product, height=Height, pick_quantity=0,
                     attributes=[], created_at=mypl_util:timestamp()},
        mypl_audit:unitaudit(Unit, "Erzeugt auf " ++ Location#location.name),
        store_at_location(Location, Unit)
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

%% @spec retrieve(muID()) -> {ok, {Quantity::integer(), Product::string()}}
%% @doc remove a Unit and the goods on it from the warehouse
%%
%% This actually makes goods vanish from the warehouse without further confirmation or committing.
%% returns the name of the location from where the Unit was removed.
retrieve(Mui) ->
    Fun = fun() ->
        Unit = mypl_db_util:mui_to_unit(Mui),
        Location = mypl_db_util:get_mui_location(Mui),
        
        % Guard-like expressions
        case {% Unit is placed on Location
              Unit#unit.location =:= Location#location.name,
              % no open picks and movements
              mypl_db_util:unit_movable(Unit),
              % Location knows about Unit
              [Unit#unit.mui] =:= [X || X <- Location#location.allocated_by, X =:= Unit#unit.mui]} of
            {true, yes, true} ->
                % update location
                ok = mnesia:write(Location#location{allocated_by=Location#location.allocated_by--[Unit#unit.mui]}),
                % delete unit record
                ok = mnesia:delete({unit, Mui}),
                % log
                mypl_audit:unitaudit(Unit, "Aufgeloeesst auf " ++ Location#location.name),
                mypl_audit:articleaudit(-1 * Unit#unit.quantity, Unit#unit.product,
                                 "Warenabgang auf " ++ Location#location.name, Unit#unit.mui),
                mypl_audit:archive(Unit, retieve),
                {ok, {Unit#unit.quantity, Unit#unit.product}};
            {_, no, _} ->
                erlang:error({internal_error, inconsistent_retrieve, {"Tried to retrieve a unit involved in a movement or pick",
                                                                     Mui, Unit, Location, mypl_db_util:unit_movement(Unit),
                                                                     mypl_db_util:unit_picks(Unit)}});
            Wrong ->
                erlang:error({internal_error, inconsistent_retrieve, {Mui, Unit, Location, Wrong}})
        end
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.

%% @doc Disbands (deletes) an unit.
%%
%% Expects to be caled within an transaction.
%% Checks that there are no goods on the unit and no open movements/picks
disband_unit(Unit) ->
    case {
          mypl_db_util:unit_movable(Unit), % this means no open picks & movements
          Unit#unit.quantity} of
        {yes, 0} ->
            retrieve(Unit#unit.mui);
        {Moving, _} ->
            {error, inconsistent_disband, {Unit#unit.mui, Unit, Moving}}
    end.


%%%%
%%%% main myPL API - movement
%%%%

%% @spec init_movement(muID(), locationName(), [{term()}])-> movementID()
%% @see commit_movement/1
%% @doc start moving a unit from its current location to a new one while setting attributes.
%%
%% Attributes can be used for arbitrary purposes. Those attributes starting with 'mypl' are
%% reserved for internal use by kernel-E. So far the following Attributes are used
%% <dl>
%%   <dt>mypl_notify_requestracker</dt> <dd>Upon committing the movement calls
%% {@link mypl_requestracker:movement_done/2}(Product).</dd>
%% </dl>
init_movement(Mui, DestinationName, Attributes) when is_list(Attributes) ->
    Fun = fun() ->
        % get unit for Mui & get current location of mui
        Unit = mypl_db_util:mui_to_unit(Mui),
        Source = mypl_db_util:get_mui_location(Mui),
        % check destination exists
        Destination = mypl_db_util:read_location(DestinationName),
        % check that mui is movable
        case mypl_db_util:unit_movable(Unit) of
            no ->
                {error, not_movable, {Mui}};
            yes ->
                % now we can write to the database
                ok = mnesia:write(Destination#location{reserved_for=Destination#location.reserved_for ++ [Mui]}),
                % generate movement record
                Movement = #movement{id=("m" ++ mypl_util:serial()), mui=Mui,
                                     from_location=Source#location.name,
                                     to_location=Destination#location.name,
                                     created_at=mypl_util:timestamp(),
                                     attributes=Attributes},
                ok = mnesia:write(Movement),
                Resevation = #reservation{id=(DestinationName ++ "-" ++ Mui),
                                         mui=Mui,
                                         location=DestinationName,
                                         reason=movement,
                                         attributes=Attributes},
                ok = mnesia:write(Resevation),
                mypl_audit:unitaudit(Unit, "Umlagerung von " ++ Source#location.name ++ " nach "
                              ++ Destination#location.name ++ " initialisiert", Movement#movement.id),
                {ok, Movement#movement.id}
        end
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    
%% @spec init_movement(muID(), locationName())-> movementID()
%% @see commit_movement/1
%% @doc start moving a unit from its current location to a new one
init_movement(Mui, DestinationName) ->
    init_movement(Mui, DestinationName, []).
    

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
        From = Movement#movement.from_location,
        From = Source#location.name,
        % check destination exists
        Destination = mypl_db_util:read_location(Movement#movement.to_location),
        % change locationdata
        teleport(Unit, Source, Destination),
        mypl_audit:archive(Movement, commit_movement),
        ok = mnesia:delete({movement, MovementId}),
        mypl_audit:unitaudit(Unit, "Umlagerung von " ++ Source#location.name ++ " nach "
                      ++ Destination#location.name ++ " comitted", Movement#movement.id),
        
        ok = mnesia:delete({reservation, (Movement#movement.to_location ++ "-" ++ Movement#movement.mui)}),
        case (lists:member({mypl_notify_requestracker}, Movement#movement.attributes) 
              andalso Movement#movement.to_location /= "AUSLAG") of
            true ->
                mypl_requesttracker:movement_done(Unit#unit.quantity, Unit#unit.product);
            _ -> []
        end,
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
        Destination = mypl_db_util:read_location(Movement#movement.to_location),
        Newdestination = Destination#location{reserved_for=lists:filter(fun(X) -> X /= Unit#unit.mui end,
                                                                        Destination#location.reserved_for)},
        ok = mnesia:write(Newdestination),
        mypl_audit:archive(Movement, rollback_movement),
        ok = mnesia:delete({movement, MovementId}),
        ok = mnesia:delete({reservation, (Movement#movement.to_location ++ "-" ++ Movement#movement.mui)}),
        mypl_audit:unitaudit(Unit, "Umlagerung von " ++ Source#location.name ++ " nach "
                      ++ Destination#location.name ++ " abgebrochen", Movement#movement.id),
        Source#location.name
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    {ok, Ret}.


%% @see commit_movement/1
%% @see retrieve/1
%% @doc commit movement/1 and afterwards retrieve the mui
commit_retrieval(MovementId) ->
    Fun = fun() ->
        [Movement] = mnesia:read({movement, MovementId}),
        % get unit for Mui & get current location of mui
        Unit = mypl_db_util:mui_to_unit(Movement#movement.mui),
        {ok, _ } = commit_movement(MovementId),
        {ok, {_, _}} = retrieve(Unit#unit.mui)
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    {ok, Ret}.
    

%% @see rollback_movement/1
%% @doc alias for rollback_movement/1
rollback_retrieval(MovementId) ->
    rollback_movement(MovementId).

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
                {error, not_enough_goods, {Quantity, UnitPickQuantity, Unit#unit.quantity, Mui, Unit}};
            true ->
                % update Unit
                ok = mnesia:write(Unit#unit{pick_quantity=UnitPickQuantity}),
                % generate Pick
                Pick = #pick{id=("P" ++ mypl_util:serial()), quantity=Quantity,
                             product=Unit#unit.product, from_unit=Unit#unit.mui,
                             created_at=mypl_util:timestamp()},
                ok = mnesia:write(Pick),
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
                error_logger:error_msg("Negative amount on unit: ~w ~s ~s",
                                       [Pick#pick.quantity, PickId, Unit#unit.mui]),
                {error, not_enough_goods, {Pick#pick.quantity, PickId, Unit#unit.mui}};
            true ->
                % update Unit
                ok = mnesia:write(NewUnit),
                mypl_audit:archive(Pick, commit_pick),
                ok = mnesia:delete({pick, PickId}),
                mypl_audit:articleaudit(-1 * Pick#pick.quantity, Pick#pick.product,
                                        "Pick auf " ++ Unit#unit.mui, Unit#unit.mui, PickId),
                mypl_audit:unitaudit(Unit, "Pick von " ++ integer_to_list(Pick#pick.quantity) 
                                     ++ " committed. neuer Bestand " ++ integer_to_list(Unit#unit.quantity),
                                     PickId),
                if
                    NewUnit#unit.quantity =:= 0 ->
                        % disband unit since it is empty now
                        disband_unit(NewUnit);
                    true -> 
                        ok
                end,
                mypl_abcserver:feed(pick, Pick, Unit#unit.location),
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
        NewUnit = Unit#unit{pick_quantity=Unit#unit.pick_quantity - Pick#pick.quantity},
        if
            NewUnit#unit.pick_quantity < 0 ->
                % this really shouldn't happen
                {error, internal_error, {Pick, Unit}};
            true ->
                % update Unit
                ok = mnesia:write(NewUnit),
                mypl_audit:archive(Pick, rollback_pick),
                ok = mnesia:delete({pick, PickId}),
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
    % check consistency
    case {% Unit is placed on Source
          Unit#unit.location =:= Source#location.name,
          % Source knows about Unit
          [Unit#unit.mui] =:= [X || X <- Source#location.allocated_by, X =:= Unit#unit.mui],
          % Destination knows about Unit
          [Unit#unit.mui] =:= [X || X <- Destination#location.reserved_for, X =:= Unit#unit.mui]} of
        {true, true, true} ->
            % do the move
            Newsource = Source#location{allocated_by=Source#location.allocated_by -- [Unit#unit.mui]},
            Newdestination = Destination#location{allocated_by=[Unit#unit.mui|Destination#location.allocated_by],
                                                  reserved_for=Destination#location.reserved_for -- [Unit#unit.mui]},
            ok = mnesia:write(Newsource),
            ok = mnesia:write(Newdestination),
            ok = mnesia:write(Unit#unit{location=Newdestination#location.name});
        Wrong ->
            erlang:error({internal_error, inconsistent_teleport1, {Unit, Source, Destination, Wrong}})
    end.
    

% ~~ Unit tests
-ifdef(EUNIT).
-compile(export_all).

%%% @hidden
test_init() ->
    run_me_once(),
    % flush database
    mnesia:start(),
    mnesia:clear_table(unit),
    mnesia:clear_table(location),   
    mnesia:clear_table(movement),   
    mnesia:clear_table(pick),       
    mnesia:clear_table(articleaudit),
    mnesia:clear_table(unitaudit),  
    % regenerate locations
    % init_location(Name, Height, Floorlevel, Preference, Attributes)
    init_location("EINLAG", 3000, true,  0, [{no_picks}]),
    init_location("AUSLAG", 3000, true,  0, [{no_picks}]),
    init_location("010101", 2000, true,  6, []),
    init_location("010102", 1950, false, 6, []),
    init_location("010103", 1200, false, 5, []),
    init_location("010201", 2000, true,  7, []),
    true = is_list(mypl_db_query:location_list()),
    {ok, {{name, "EINLAG"},
          {height, 3000},
          {floorlevel, true},
          {preference, 0},
          {info, []},
          {attributes,[{no_picks}]},
          {allocated_by, []},
          {reserved_for, []}}} = mypl_db_query:location_info("EINLAG"),
    ok.

%%% @hidden
mypl_simple_movement_test() ->
    test_init(),
    % generate a MUI for testing
    Mui = "a0001-" ++ mypl_util:generate_mui(),
    % generate and Store Unit of 5*14601 (1200mm high) on "EINLAG"
    {ok, Mui} = store_at_location("EINLAG", Mui, 5, "a0001", 1200),
    
    % simple test for unit_info
    {ok, _} = mypl_db_query:unit_info(Mui),
    
    % start movement to "010101".
    {ok, Movement1} = init_movement(Mui, "010101"),
    
    [Movement1] = mypl_db_query:movement_list(),
    {ok, _} = mypl_db_query:movement_info(Movement1),
    
    % finish movement
    {ok,"010101"} = commit_movement(Movement1),
    % check that Unit now is on the new Location
    %Location1 = mypl_db_util:get_mui_location(Mui),
    %?assert(Location1#location.name == "010101"),
    
    % now move it to the best location the system can find for this Unit
    {ok, Movement2} = init_movement_to_good_location(Mui),
    % finish movement & check that Unit now is on the new Location - 010103 is best because it is lowest (1200mm)
    {ok, "010103"} = commit_movement(Movement2),
    
    % now try again to move to a "good" location - with lot's of checks
    {ok, Movement3} = init_movement_to_good_location(Mui),
    % while movement is initialized the unit is still booked on the old location
    % Location2 = mypl_db_util:get_mui_location(Mui),
    
    % try to initiate an other movement on that Mui - shouldn't be possible
    {error, not_movable, _} = init_movement(Mui, "010101"),
    % Location2 = mypl_db_util:get_mui_location(Mui),
    
    % we rollback the whole thing ... so the unit should still be in it's old location
    {ok, "010103"} = rollback_movement(Movement3),
    
    % check issues with two units
    Mui2 = "14601-" ++ mypl_util:generate_mui(),
    % generate and Store Unit of 6*14601 (1200mm high) on "EINLAG"
    {ok, Mui2} = store_at_location("EINLAG", Mui2, 6, "a0001", 1200),
    {ok, Movement4} = init_movement_to_good_location(Mui),
    {ok, Movement5} = init_movement_to_good_location(Mui2),
    commit_movement(Movement5),
    commit_movement(Movement4),
    
    % remove Muis from warehouse
    {ok, {5, "a0001"}} = retrieve(Mui),
    {ok, {6, "a0001"}} = retrieve(Mui2).
    

%%% @hidden
mypl_simple_pick_test() ->
    test_init(),
    % generate a MUI for testing
    Mui = "a0002-" ++ mypl_util:generate_mui(),
    % generate and Store Unit of 5*14601 (1200mm high) on "010101"
    {ok, Mui} = store_at_location("010101", Mui, 70, "a0002", 1950),
    % start picking.
    {ok, Pick1} = init_pick(30, Mui),
    
    [Pick1] = mypl_db_query:pick_list(),
    {ok, _} = mypl_db_query:pick_info(Pick1),
    
    %% try to initiate an movement on that Mui - shouldn't be possible with open picks
    {error, not_movable, _} = init_movement_to_good_location(Mui),
    % because of the open pick the unit shouldn't be movable
    %no = mypl_db_util:transaction(mypl_db_util:unit_movable(mypl_db_util:mui_to_unit_trans(Mui))),
    % commit it.
    {ok, {30, "a0002"}} = commit_pick(Pick1),
    % now ot should be movable again
    %yes = mypl_db_util:transaction(mypl_db_util:unit_movable(mypl_db_util:mui_to_unit_trans(Mui))),
     
    % start picking.
    {ok, Pick2} = init_pick(25, Mui),
    % commit it.
    {ok, {25, "a0002"}} = commit_pick(Pick2),
    
    % try to get to much
    {error, not_enough_goods, _} = init_pick(25, Mui),
    
    % try to get to much
    {ok, Pick4} = init_pick(15, Mui),
    {ok, {15, "a0002"}} = rollback_pick(Pick4),
    
    % check if enough is left on unit
    {ok, {15, "a0002"}} = retrieve(Mui).
    

mypl_disbanding_test() ->
    test_init(),
    % generate a MUI for testing
    {ok, mui1} = store_at_location("010101", mui1, 1, "a0002", 1950),
    {ok, Pick1} = init_pick(1, mui1),
    % since the pick empties the unit this should lead to disbanding
    {ok, {1, "a0002"}} = commit_pick(Pick1),
    % now mui1 should be gone
    {error, unknown_mui, {mui1}} = mypl_db_query:unit_info(mui1),
    
    {ok, mui2} = store_at_location("010101", mui2, 1, "a0003", 1950),
    {ok, Movement2} = init_movement_to_good_location(mui2),
    Unit2 = mypl_db_util:mui_to_unit_trans(mui2),
    {atomic, {error, inconsistent_disband, _}} = mnesia:transaction(fun() -> disband_unit(Unit2) end),
    ok.
    

testrunner() ->
    mypl_simple_movement_test(),
    mypl_simple_pick_test(),
    mypl_disbanding_test().
    
-endif.
