%% @version 0.3
%% @copyright 2007, 2008 HUDORA GmbH
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
-include("mypl.hrl").

-export([init_table_info/2, run_me_once/0, init_location/6, init_location/5,
 store_at_location/5, store_at_location_multi/4, store_at_location_multi/1, update_unit/1, retrieve/1,
 init_movement/2, init_movement/3, init_movement_to_good_location/1,
 commit_movement/1, rollback_movement/1, update_movement/1,
 commit_retrieval/1, rollback_retrieval/1,
 init_pick/2, init_pick/3, commit_pick/1, rollback_pick/1, update_pick/1,
 backup/0,
 correction/6, correction/1]).

-type locationName() :: nonempty_string().
%%% @type locationName() = string().
%%%     Unique, human readable name of an location.
%%%
-type muID() :: nonempty_string().
%%% @type muID() = string().
%%%     MovableUnitID, unique id of an Unit. Often an SSCC/NVE.
%%%
-type movementID() :: nonempty_string().
%%% @type movementID() = string().
%%%     MoventID, unique id of an movement.
%%%
-type pickID() :: nonempty_string().
%%% @type pickID() = string().
%%%     PickID, unique id of an pick.
%%%
-type heigthMM() :: integer().
%%% @type heigthMM() = integer().
%%%     Heigth of an Unit or Location im mm. If unsure it is suggested that you choose 1950.
%%%
-type quantity() :: non_neg_integer().
-type content() :: nonempty_string().
%%% @type content() = string().
%%%     Opaque ID for an product. Artikelnummer/Item Number/SKU or EAN.
%%%
-type attributes() :: [{atom()}|{atom()|binary(),integer()|float()|atom(),string()|binary()}].
%%% @type locationRecord() = tuple().
%%%     A record describing a Location.
%%%
%%% @type unitRecord() = tuple().
%%%     A record describing a Unit.
-type jsondict() :: {[{atom()|binary(),integer()|float()|atom(),string()|binary()}]}.
%%% Hudora specific encoding of proplists for JSON


-spec init_table_info({'aborted',{'already_exists', atom()}} | {'atomic','ok'}, atom()) -> {'aborted',{'already_exists',_}} | {'atomic','ok'}.
init_table_info(Status, TableName) ->
    case Status of
        {atomic, ok} ->
            ?WARNING("table '~w' created", [TableName]);
        {aborted, {already_exists, TableName}} ->
            ok;
        _ ->
            ?ERROR("cannot create table '~w' -- ~w", [TableName, Status], Status)
    end,
    Status.
    
%% @doc should be run before mnesia is started for the first time.
-spec run_me_once() -> 'ok'.
run_me_once() ->
    % ?WARNING("run_me_once() called", []),
    mnesia:create_schema([node()]),
    % the main tables are kept in RAM with a disk copy for fallback
    init_table_info(mnesia:create_table(location,         [{disc_copies, [node()]}, {attributes, record_info(fields, location)}]), location),
    init_table_info(mnesia:create_table(unit,             [{disc_copies, [node()]}, {attributes, record_info(fields, unit)}]), unit),
    mnesia:add_table_index(unit, #unit.product),
    init_table_info(mnesia:create_table(movement,         [{disc_copies, [node()]}, {attributes, record_info(fields, movement)}]), movement),
    init_table_info(mnesia:create_table(pick,             [{disc_copies, [node()]}, {attributes, record_info(fields, pick)}]), pick),
    init_table_info(mnesia:create_table(multistorage,     [{disc_copies, [node()]}, {attributes, record_info(fields, multistorage)}]), multistorage),
    init_table_info(mnesia:create_table(correction,       [{disc_copies, [node()]}, {attributes, record_info(fields, correction)}]), correction),
    mnesia:add_table_index(correction, #correction.product),
    mnesia:add_table_index(correction, #correction.mui),
    
    % give other modules to initialize database tables
    mypl_abcserver:run_me_once(),
    mypl_audit:run_me_once(),
    mypl_provpipeline:run_me_once(),
    mypl_volumes:run_me_once(),
    
    ok = mnesia:wait_for_tables([location, unit, movement, pick, multistorage, correction,
                                 articleaudit, unitaudit%, provpipline, provisioninglist
                                ], 50000),
    
    % upgrade tables where needed
    Fields1 = record_info(fields, pick),
    case mnesia:table_info(pick, attributes) of
        Fields1 ->
            ok;
        [id,product,quantity,from_unit,created_at] ->
            ?WARNING("upgrading table picks with attributes field", []),
            mnesia:transform_table(pick,
                                   fun({_, I, P, Q, F, C}) -> 
                                       {pick, I, P, Q, F, C, []}
                                    end,
                                    [id, product, quantity, from_unit, created_at, attributes])
    end,
    
    Fields2 = record_info(fields, archive),
    case mnesia:table_info(archive, attributes) of
        Fields2 ->
            ok;
        [id,created_at,archived_by,body] ->
            ?WARNING("upgrading table archive with new fields", []),
            erlang:display({
            mnesia:transform_table(archive,
                                   fun({_, I, C, A, Body}) -> 
                                        {archive, I, C, A, element(1, Body), element(2, Body), Body}
                                    end,
                                    [id,created_at,archived_by,type,body_id,body])
                                    })
    end,
    
    init_location("EINLAG", 3000, true,  0, [{no_picks}]),
    init_location("AUSLAG", 3000, true,  0, [{no_picks}]),
    init_location("FEHLER", 3000, true,  0, [{no_picks}]),
    init_location("FNDLNG", 3000, true,  0, [{no_picks}]),
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
    init_location("K11",    3000, true,  0, []),
    init_location("K12",    3000, true,  0, []),
    init_location("K13",    3000, true,  0, []),
    init_location("K14",    3000, true,  0, []),
    init_location("K15",    3000, true,  0, []),
    init_location("K16",    3000, true,  0, []),
    init_location("K17",    3000, true,  0, []),
    init_location("K18",    3000, true,  0, []),
    init_location("K19",    3000, true,  0, []),
    init_location("K20",    3000, true,  0, []),
    ok.
    

%% @doc backup the whole database
-spec backup() -> 'ok'.
backup() ->
    Tables = [location, unit, movement, pick, abc_pick_detail, archive, articleaudit,
              pickpipeline, provisioninglist, provpipeline, provpipeline_processing,
              retrievalpipeline, unitaudit, multistorage],
    Day = calendar:day_of_the_week(date()),
    BkName = "Backup-tag-" ++ integer_to_list(Day),
    CPargs = [{name, BkName},
              {min, Tables},
              {allow_remote, false},
              {ram_overrides_dump, true}],
    Dir = mnesia:system_info(directory),
    File = filename:absname_join(Dir, BkName),
    mnesia:activate_checkpoint(CPargs),
    mnesia:backup_checkpoint(BkName,File),
    mnesia:deactivate_checkpoint(BkName),
    ok.
    

%%%%
%%%% main myPL API - location data
%%%%

%% @doc creates a new Location or updates an existing one.
%% 
%% Locations can be created at any time - even when the myPL bristles with activity..
%% There is no way of deleting Locations. Set their preference to 0 and let them rot.
%% returns {ok, created|updated
-spec init_location(locationName(),heigthMM(),bool(),0..9,string()|binary(),attributes()) ->
    {'ok',#location{}} | {'error','unknown_attributes',any()}.
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
                Ret
            end,
            {atomic, Ret} = mnesia:transaction(Fun),
            {ok, Ret};
        UnknownAttributes ->
            {error, unknown_attributes, {UnknownAttributes}}
    end.
    

%% @deprecated
-spec init_location(locationName(),heigthMM(),bool(),0..9,attributes()) ->
    {'ok',#location{}} | {'error','unknown_attributes',{[any(),...]}}.
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
-spec store_at_location(#location{},#unit{}) -> {ok, muID()}|{'error', 'duplicate_mui', term()}.
store_at_location(Location, Unit) when Unit#unit.quantity > 0 ->
    Fun = fun() ->
        % check no unit record with this mui exists
        case mnesia:read({unit, Unit#unit.mui}) of
            [ExistingUnitload] ->
                mypl_zwitscherserver:zwitscher("Versuch Unit ~s auf ~s einzulagern. Es existiert aber Bereits eine Unit mit dieser MUI auf ~s. #error",
                                               [Unit#unit.mui, Location#location.name, ExistingUnitload#unit.location]),
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
                             "Warenzugang von " ++ integer_to_list(Unit#unit.quantity) ++ " mal " 
                             ++ Unit#unit.product ++ " auf " ++ Location#location.name, Unit#unit.mui),
            {ok, Unit#unit.mui}
        end
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

% legacy emulation function
%% @deprecated
-spec store_at_location(locationName(),muID(),quantity(),content(),heigthMM()) -> {ok, muID()} | {error, Reason::atom(), Details::any()}.
store_at_location(Locname, Mui, Quantity, Product, Height) ->
store_at_location(Locname, Mui, Quantity, Product, Height, []).

%% @doc create a new Unit and insert it into the warehouse
%%
%% On errors it might also return {error, unknown_location} or {error, duplicate_mui}.
-spec store_at_location(locationName(),muID(),quantity(),content(),heigthMM(),attributes()) -> {ok, muID()} | {error, Reason::atom(), Details::any()}.
store_at_location(Locname, Mui, Quantity, Product, Height, Attributes)
  when Quantity > 0 andalso is_list(Mui) andalso is_list(Attributes) ->
    Fun = fun() ->
        % check that location exists
        Location = mypl_db_util:read_location(Locname),
        Unit = #unit{mui=Mui, quantity=Quantity, product=Product, height=Height, pick_quantity=0,
                     attributes=Attributes, created_at=mypl_util:timestamp()},
        mypl_audit:unitaudit(Unit, "Erzeugt auf " ++ Location#location.name),
        store_at_location(Location, Unit)
    end,
    mypl_db_util:transaction(Fun).
    

%% @doc creates several units within a single transaction.
%%
%% `Id' must be unique. If you call store_at_location_multi/4 more than once with the same Id,
%% successive calls return duplicate_id instead a list of NVEs.
%% See http://blogs.23.nu/disLEXia/2007/12/antville-16699/ for some rationale behind this function.
%% returns {ok, [Mui]}.
-spec store_at_location_multi(string()|binary(),locationName(),[{quantity(),content(),heigthMM()}],attributes()) ->
    {ok,[muID()]}|{'error', 'duplicate_id', any()}.
store_at_location_multi(Id, Locname, Elements, Attributes) ->
    Id2 = mypl_util:ensure_binary(Id),
    Attributes2 = mypl_util:proplist_cleanup(Attributes),
    Fun = fun() ->
        % check for dupes
        case mnesia:read({multistorage, Id2}) of
            [_Entry] ->
                {error, duplicate_id, Id2};
            [] ->
                % insert the data
                Muis = lists:map(fun({Quantity, Product, Height}) ->
                        Mui = mypl_nveserver:make_nve(),
                        {ok, _} = store_at_location(Locname, Mui, Quantity, Product, Height, Attributes2),
                        Mui;
                    % we have to catch lists instead of tuples for json compatibility
                    ([Quantity, Product, Height]) ->
                        Mui = mypl_nveserver:make_nve(),
                        {ok, _} = store_at_location(Locname, Mui, Quantity, Product, Height, Attributes2),
                        Mui
                     end, Elements),
                % we refrain from using some of the fields to save storage space
                % TODO: change Table/record accordingly
                mnesia:write(#multistorage{id=Id2, muis=[], attributes=[], created_at={}}),
                {ok, Muis}
        end
    end,
    mypl_db_util:transaction(Fun).
    

%% these are for messed up json data
store_at_location_multi({Id, Locname, Elements, Attributes}) ->
    store_at_location_multi(Id, Locname, Elements, Attributes);
store_at_location_multi([Id, Locname, Elements, Attributes]) ->
    store_at_location_multi(Id, Locname, Elements, Attributes).
    

%% @doc changes the heigth of an unit. This will influence choice of storage location.
-spec update_unit({height,muID(),heigthMM()}) -> 'ok'.
update_unit({height, Mui, Height}) ->
    Fun = fun() ->
        Unit = mypl_db_util:mui_to_unit(Mui),
        NewUnit = Unit#unit{height = Height},
        ok = mnesia:write(NewUnit),
        mypl_audit:unitaudit(NewUnit, "Hoehe geanedert"),
        ok
    end,
    mypl_db_util:transaction(Fun);
% this is for JSON compability
update_unit(["height", Mui, Height]) ->
    update_unit({height, Mui, Height}).

%% @doc remove a Unit and the goods on it from the warehouse
%%
%% This actually makes goods vanish from the warehouse without further confirmation or committing.
%% returns the name of the location from where the Unit was removed.
-spec retrieve(muID()) -> {'ok', {quantity(), content()}}.
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
                                 "Warenabgang von " ++  Unit#unit.mui
                                 ++ " auf " ++ Location#location.name, Unit#unit.mui),
                mypl_audit:archive(Unit, retrieve),
                {ok, {Unit#unit.quantity, Unit#unit.product}};
            {_, no, _} ->
                erlang:error({internal_error, inconsistent_retrieve, {"Tried to retrieve a unit involved in a movement or pick",
                                                                     Mui, Unit, Location, mypl_db_util:unit_movement(Unit),
                                                                     mypl_db_util:unit_picks(Unit)}});
            Wrong ->
                erlang:error({internal_error, inconsistent_retrieve, {Mui, Unit, Location, Wrong}})
        end
    end,
    mypl_db_util:transaction(Fun).
    

%% @doc Disbands (deletes) an unit.
%%
%% Expects to be called within an transaction.
%% Checks that there are no goods on the unit and no open movements/picks
-spec disband_unit(#unit{}) -> {'ok', {0, content()}}
                              |{'error', 'inconsistent_disband', {muID(), #unit{}, any()}}.
disband_unit(Unit) ->
    case {
          mypl_db_util:unit_movable(Unit), % this means no open picks & movements
          Unit#unit.quantity} of
        {yes, 0} ->
            retrieve(Unit#unit.mui);
        {Moving, _} ->
            {error, inconsistent_disband, {Unit#unit.mui, Unit, Moving}}
    end.


%% @see commit_movement/1
%% @doc start moving a unit from its current location to a new one while setting attributes.
%%
%% Attributes can be used for arbitrary purposes. Those attributes starting with 'mypl' are
%% reserved for internal use by kernel-E. So far the following Attributes are used
%% <dl>
%%   <dt>mypl_notify_requesttracker</dt> <dd>Upon committing the movement calls
%% {@link mypl_requesttracker:movement_done/2}(Product).</dd>
%% </dl>
-spec init_movement(muID(), locationName(), attributes()) -> {'ok', movementID()}
                                                            |{'error', 'not_movable', {muID()}}.
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
                case proplists:get_value(kernel_type, Attributes) of
                    retrieval ->
                        Id = "mr" ++ mypl_util:serial();
                    _ -> 
                        Id = "mb" ++ mypl_util:serial()
                end,
                case mnesia:read({movement, Id}) of
                    [] -> ok % anything else would be a nasty malfunction of mypl_util:serial()
                end,
                Movement = #movement{id=Id, mui=Mui,
                                     from_location=Source#location.name,
                                     to_location=Destination#location.name,
                                     created_at=mypl_util:timestamp(),
                                     attributes=Attributes},
                ok = mnesia:write(Movement),
                mypl_audit:unitaudit(Unit, "Umlagerung von " ++ Source#location.name ++ " nach "
                              ++ Destination#location.name ++ " initialisiert", Movement#movement.id),
                {ok, Movement#movement.id}
        end
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    
%% @deprecated
%% @see commit_movement/1
%% @doc start moving a unit from its current location to a new one
-spec init_movement(muID(),locationName()) -> {'ok', movementID()}|{'error', 'not_movable', {muID()}}.
init_movement(Mui, DestinationName) ->
    init_movement(Mui, DestinationName, []).
    

%% @see mypl_db_util:best_location/1
%% @doc start moving a Unit to a location choosen automatically
%%
%% The system chooses the (hopfully) best fitting location for the Unit and then uses {@link init_movement/2}
%% to initiate a movement of the Unit tu that Location.
-spec init_movement_to_good_location(muID()) -> movementID().
init_movement_to_good_location(Mui) ->
    % chooses the best location for an MUI and starts moving it there
    Fun = fun() ->
        Unit = mypl_db_util:mui_to_unit(Mui),
        Destination = mypl_db_util:best_location(Unit),
        init_movement(Mui, Destination#location.name)
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

%% @doc This is used by commit_movement and commit_retrieval
%% @private
%% Needs to be called within a transaction
-spec commit_movement_backend(#movement{}) -> {'ok',locationName()}.
commit_movement_backend(Movement) ->
    % get unit for Mui & get current location of mui
    Unit = mypl_db_util:mui_to_unit(Movement#movement.mui),
    Source = mypl_db_util:get_mui_location(Movement#movement.mui),
    From = Movement#movement.from_location,
    From = Source#location.name,
    % check destination exists
    Destination = mypl_db_util:read_location(Movement#movement.to_location),
    % change locationdata
    teleport(Unit, Source, Destination),
    mypl_audit:archive(Movement#movement{attributes=Movement#movement.attributes
                                         ++ [{committed_at, mypl_util:timestamp()}]},
                       commit_movement),
    ok = mnesia:delete({movement, Movement#movement.id}),
    mypl_audit:unitaudit(Unit, "Umlagerung von " ++ Source#location.name ++ " nach "
                  ++ Destination#location.name ++ " comitted", Movement#movement.id),
    
    case (lists:member({mypl_notify_requesttracker}, Movement#movement.attributes) 
          andalso Movement#movement.to_location /= "AUSLAG") of
        true ->
            mypl_requesttracker:movement_done(Unit#unit.quantity, Unit#unit.product);
        _ -> []
    end,
    {ok, Destination#location.name}.
    

%% @see rollback_movement/1
%% @doc finish a movement
%%
%% Commits a movement created previously by {@link init_movement/2}. This is by doing the actual
%% bookkeping of storing the unit on the new location and removing all previous information on the
%% formerly unfinished movement. Returns the name of the Location where the Unit is stored now.
%% TODO: seemingly this doesn't work as intended
-spec commit_movement(movementID()) -> locationName().
commit_movement(MovementId) ->
    Fun = fun() ->
        [Movement] = mnesia:read({movement, MovementId}),
        %%% check we don't commit a retrieval as a movement leaving goods lying arround on AUSLAG
        case proplists:get_value(kernel_type, Movement#movement.attributes) of
            retrieval ->
                mypl_zwitscherserver:zwitscher("Versuch ~s als Movement zurueckzumelden - Es handelt sich jedoch um ein retrieval. #error",
                                               [MovementId]),
                {error, retrieval_is_no_movement, [Movement]};
            "retrieval" ->
                mypl_zwitscherserver:zwitscher("Versuch ~s als Movement zurueckzumelden - Es handelt sich jedoch um ein retrieval. #error",
                                               [MovementId]),
                {error, retrieval_is_no_movement, [Movement]};
            <<"retrieval">> ->
                mypl_zwitscherserver:zwitscher("Versuch ~s als Movement zurueckzumelden - Es handelt sich jedoch um ein retrieval. #error",
                                               [MovementId]),
                {error, retrieval_is_no_movement, [Movement]};
            _ ->
                commit_movement_backend(Movement)
        end
    end,
    mypl_db_util:transaction(Fun).
    

%% @see commit_movement/1
%% @doc rollback a movement
%%
%% This rolls back a movement returning the warehouse to a state as if {@link init_movement/2} had
%% never been called. Returns the name of the Location where the Unit now is placed (again).
-spec rollback_movement(movementID()) -> {'ok', locationName()}.
rollback_movement(MovementId) ->
    Fun = fun() ->
        case mnesia:read({movement, MovementId}) of
            [Movement] ->
                % get unit for Mui & get current location of mui
                Unit = mypl_db_util:mui_to_unit(Movement#movement.mui),
                Source = mypl_db_util:get_mui_location(Movement#movement.mui),
                % fix destination
                Destination = mypl_db_util:read_location(Movement#movement.to_location),
                Newdestination = Destination#location{reserved_for=lists:filter(fun(X) -> X /= Unit#unit.mui end,
                                                                                Destination#location.reserved_for)},
                ok = mnesia:write(Newdestination),
                mypl_audit:archive(Movement#movement{attributes=Movement#movement.attributes
                                                     ++ [{rolled_back_at, mypl_util:timestamp()}]},
                                   rollback_movement),
                ok = mnesia:delete({movement, MovementId}),
                mypl_audit:unitaudit(Unit, "Umlagerung von " ++ Source#location.name ++ " nach "
                              ++ Destination#location.name ++ " abgebrochen", Movement#movement.id),
                {ok, Source#location.name};
            [] ->
                {error, unknown}
        end
    end,
    mypl_db_util:transaction(Fun).
    

%% @doc update the attributes of a movement
-spec update_movement({'attributes',movementID(),attributes()}) -> 'ok'.
update_movement({attributes, MovementId, Attributes}) when is_list(Attributes) ->
    Fun = fun() ->
        [Movement] = mnesia:read({movement, MovementId}),
        % TODO: this replaces the attributes. It would be better tu actually update them.
        mnesia:write(Movement#movement{attributes=Attributes})
    end,
    mypl_db_util:transaction(Fun),
    ok.
    

%% @see commit_movement/1
%% @see retrieve/1
%% @doc commit movement/1 and afterwards retrieve the mui
-spec commit_retrieval(movementID()) -> {'ok', {quantity(), content()}}.
commit_retrieval(MovementId) ->
    Fun = fun() ->
        [Movement] = mnesia:read({movement, MovementId}),
        % get unit for Mui & get current location of mui
        Unit = mypl_db_util:mui_to_unit(Movement#movement.mui),
        {ok, _ } = commit_movement_backend(Movement),
        {ok, {_, _}} = retrieve(Unit#unit.mui)
    end,
    mypl_db_util:transaction(Fun).
    

%% @see rollback_movement/1
%% @doc alias for rollback_movement/1
-spec rollback_retrieval(movementID()) -> {'ok',locationName()}.
rollback_retrieval(MovementId) ->
    rollback_movement(MovementId).


%% @deprecated
-spec init_pick(quantity(),content()) -> {'ok',pickID()}.
init_pick(Quantity, Mui) ->
    init_pick(Quantity, Mui, []).

%% @see commit_pick/1
%% @doc start a new pick removing Quantity Products from Mui
-spec init_pick(quantity(),muID(),attributes()) -> {'ok', pickID()}.
init_pick(Quantity, Mui, Attributes) when is_integer(Quantity) ->
    Fun = fun() ->
        % get unit for Mui & get current location of mui
        Unit = mypl_db_util:mui_to_unit(Mui),
        UnitPickQuantity = Unit#unit.pick_quantity + Quantity,
        if
            UnitPickQuantity > Unit#unit.quantity ->
                % this really shouldn't happen
                {error, not_enough_goods, {Quantity, UnitPickQuantity, Unit#unit.quantity, Mui, Unit}};
            true ->
                PickId = ("P" ++ mypl_util:serial()),
                case mnesia:read({pick, PickId}) of
                    [] -> ok % anything else would be a nasty malfunction of mypl_util:serial()
                end,
                % update Unit
                ok = mnesia:write(Unit#unit{pick_quantity=UnitPickQuantity}),
                % generate Pick
                Pick = #pick{id=PickId, quantity=Quantity,
                             product=Unit#unit.product, from_unit=Unit#unit.mui,
                             created_at=mypl_util:timestamp(),
                             attributes=Attributes},
                ok = mnesia:write(Pick),
                mypl_audit:unitaudit(Unit, "Pick von " ++ integer_to_list(Pick#pick.quantity) 
                                     ++ " initialisiert. Verfügbarer Bestand " 
                                     ++ integer_to_list(Unit#unit.quantity-UnitPickQuantity),
                                     Pick#pick.id),
                {ok, Pick#pick.id}
        end
    end,
    mypl_db_util:transaction(Fun).
    

%% @see rollback_pick/1
%% @doc finish a a pick - this actually makes goods vanish from the warehouse!
%%
%% Commits a pick created previously by {@link init_pick/2}. This is by doing the actual
%% bookkeping of removing the goods from the warehouse. Returns the name of the Location
%% where the goods where removed from.
-spec commit_pick(pickID()) -> {'ok',{quantity(),content()}}.
commit_pick(PickId) ->
    Fun = fun() ->
        % get Pick for PickId
        case mnesia:read({pick, PickId}) of
            [Pick] ->
                % hack to fix picks with quantity = 0 (where do they come from?)
                if
                    Pick#pick.quantity =:= 0 ->
                        ok = mnesia:delete({pick, PickId}),
                        ?WARNING("deleted empty Pick '~w'", [PickId]),
                        ok;
                    true ->
                        Unit = mypl_db_util:mui_to_unit(Pick#pick.from_unit),
                        NewQuantity = Unit#unit.quantity - Pick#pick.quantity,
                        if
                            NewQuantity < 0 ->
                                % this really shouldn't happen
                                error_logger:error_msg("Negative amount on unit: ~w ~s ~s",
                                                       [Pick#pick.quantity, PickId, Unit#unit.mui]),
                                {error, not_enough_goods, {Pick#pick.quantity, PickId, Unit#unit.mui}};
                            true ->
                                % update Unit
                                NewUnit = Unit#unit{quantity=NewQuantity,
                                                    pick_quantity=Unit#unit.pick_quantity - Pick#pick.quantity},
                                ok = mnesia:write(NewUnit),
                                mypl_audit:archive(Pick#pick{attributes=Pick#pick.attributes
                                                             ++ [{committed_at, mypl_util:timestamp()}]},
                                                   commit_pick),
                                ok = mnesia:delete({pick, PickId}),
                                mypl_audit:articleaudit(-1 * Pick#pick.quantity, Pick#pick.product,
                                                        "Pick auf " ++ Unit#unit.mui, Unit#unit.mui, PickId),
                                mypl_audit:unitaudit(Unit, "Pick von " ++ integer_to_list(Pick#pick.quantity) 
                                                     ++ " committed. neuer Bestand " ++ integer_to_list(NewUnit#unit.quantity),
                                                     PickId),
                                if
                                    NewUnit#unit.quantity =:= 0 ->
                                        % disband unit since it is empty now
                                        disband_unit(NewUnit);
                                    true -> 
                                        ok
                                end,
                                mypl_abcserver:feed(pick, Pick, Unit#unit.location),
                            {ok, {Pick#pick.quantity, Pick#pick.product}}
                        end
                    end;
            [] ->
                {errorn, unknown}
        end
    end,
    mypl_db_util:transaction(Fun).
    

%% @see init_pick/2
%% @doc rollback a pick
%%
%% This rolls back a pick returning the warehouse to a state as if {@link init_pick/2} had
%% never been called. Returns the name of the Location where the Unit now is (again) in it's original state.
-spec rollback_pick(pickID()) -> {'ok', {quantity(),content()}}.
rollback_pick(PickId) ->
    Fun = fun() ->
        % get Pick for PickId
        case mnesia:read({pick, PickId}) of
            [Pick] ->
                Unit = mypl_db_util:mui_to_unit(Pick#pick.from_unit),
                NewPickQuantity = Unit#unit.pick_quantity - Pick#pick.quantity,
                if
                    NewPickQuantity < 0 ->
                        % this really shouldn't happen
                        {error, internal_error, {Pick, Unit}};
                    true ->
                        % update Unit
                        NewUnit = Unit#unit{pick_quantity=NewPickQuantity},
                        ok = mnesia:write(NewUnit),
                        mypl_audit:archive(Pick#pick{attributes=Pick#pick.attributes
                                                     ++ [{rolled_back_at, mypl_util:timestamp()}]},
                                           rollback_pick),
                        ok = mnesia:delete({pick, PickId}),
                        mypl_audit:unitaudit(Unit, "Pick von " ++ integer_to_list(Pick#pick.quantity) 
                                             ++ " abgebrochen.", PickId),
                    {ok, {Pick#pick.quantity, Pick#pick.product}}
                end;
            [] ->
                {error, unknown}
        end
    end,
    mypl_db_util:transaction(Fun).
    

%% @doc update attributes of a pick
-spec update_pick({'attributes',pickID(),attributes()}) -> 'ok'.
update_pick({attributes, PickId, Attributes}) when is_list(Attributes) ->
    Fun = fun() ->
        [Pick] = mnesia:read({pick, PickId}),
        mnesia:write(Pick#pick{attributes=Attributes})
    end,
    mypl_db_util:transaction(Fun),
    ok.
    

%% @private
%% @spec teleport(unitRecord(), locationRecord(), locationRecord()) -> term()
%% @doc move a mui in the warehouse - internal use only
%%
%% this assumed to be called inside a mnesia transaction
-spec teleport(#unit{},#location{},#location{}) -> 'ok'.
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
    

%-spec correction([string()|binary(),muID(),quantity(),content(),quantity(),attributes()]) ->
%    {'ok',integer(),muID()}|{'error',atom(),term()}.
correction([Uid, Mui, OldQuantity, Product, ChangeQuantity, Attributes]) ->
    correction(Uid, Mui, OldQuantity, Product, ChangeQuantity, Attributes).
%% @doc changes the quantity on an unit after stocktaking
%% Uid is a arbitary unique  ID for this correction. The caller must provide ot old quantity and the product
%% to aivoid misbookings
-spec correction(string()|binary(),muID(),quantity(),content(),quantity(),attributes()) ->
    {'ok', integer(), muID()}|{'error', atom(), term()}.
correction(Uid, Mui, OldQuantity, Product, ChangeQuantity, Attributes) when is_list(Attributes)->
    Fun = fun() ->
        case mnesia:read({correction, Uid}) of
            [] ->
                % Id does not exist, we can go ahead
                case mypl_db_util:mui_to_unit(Mui) of
                    {error, T, M} ->
                        {error, T, M};
                    Unit ->
                        case {Unit#unit.quantity, Unit#unit.product} of
                            {OldQuantity, Product} ->
                                % the quantity is as expected, and the Product matches: we can go ahead
                                NewQuantity = Unit#unit.quantity + ChangeQuantity,
                                if
                                    NewQuantity < 0 ->
                                        % this really shouldn't happen
                                        error_logger:error_msg("Negative amount on unit during correction: ~w ~w ~w ~s",
                                                               [ChangeQuantity, Uid, Unit#unit.mui]),
                                        {error, not_enough_goods, {NewQuantity, Product, Uid, Mui}};
                                    true ->
                                        % update Unit
                                        NewUnit = Unit#unit{quantity=Unit#unit.quantity + ChangeQuantity},
                                        ok = mnesia:write(NewUnit),
                                        if
                                            NewUnit#unit.quantity =:= 0 ->
                                                % disband unit since it is empty now
                                                disband_unit(NewUnit);
                                            true ->
                                                ok
                                        end,
                                        % all fixed now - log correction
                                        ok = mnesia:write(#correction{id=Uid, old_quantity=OldQuantity, product=Product,
                                                                      change_quantity=ChangeQuantity, mui=Mui,
                                                                      location=Unit#unit.location, attributes=Attributes,
                                                                      text="Korrekturbuchung",
                                                                      created_at=calendar:universal_time()}),
                                        mypl_audit:articleaudit(ChangeQuantity, Product,
                                                                "Korrekturbuchung auf " ++ Unit#unit.mui,
                                                                Unit#unit.mui, Uid),
                                        mypl_audit:unitaudit(Unit, "Korrekturbuchung von " 
                                                             ++ integer_to_list(ChangeQuantity) 
                                                             ++ " Neuer Bestand " ++ integer_to_list(NewUnit#unit.quantity),
                                                             Uid),
                                        {ok, {NewUnit#unit.quantity, NewUnit#unit.mui}}
                                end;
                            _ ->
                                error_logger:error_msg("Product or Quantity mismatch during correction ~w ~s, ~w ~s ~s",
                                                               [OldQuantity, Product, Unit#unit.quantity, Unit#unit.product, Mui]),
                                {error, missmatch, {OldQuantity, Product, Unit#unit.quantity, Unit#unit.product, Mui}}
                    end
                end;
            Corrections ->
                error_logger:error_msg("Duplicate correction ~w ~s ~w, ~w",
                                               [OldQuantity, Product, Uid, Corrections]),
                {error, duplicate_id, {OldQuantity, Product, Uid, Corrections}}
        end
    end,
    mypl_db_util:transaction(Fun).
    


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
    mnesia:clear_table(multistorage),
    mnesia:clear_table(correction),
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
    ?assertMatch({ok, _}, mypl_db_query:unit_info(Mui)),
    
    % start movement to "010101".
    {ok, Movement1} = init_movement(Mui, "010101"),
    
    ?assertMatch([Movement1], mypl_db_query:movement_list()),
    ?assertMatch({ok, _}, mypl_db_query:movement_info(Movement1)),
    
    % finish movement
    ?assertMatch({ok,"010101"}, commit_movement(Movement1)),
    % check that Unit now is on the new Location
    %Location1 = mypl_db_util:get_mui_location(Mui),
    %?assert(Location1#location.name == "010101"),
    
    % now move it to the best location the system can find for this Unit
    {ok, Movement2} = init_movement_to_good_location(Mui),
    % finish movement & check that Unit now is on the new Location - 010103 is best because it is lowest (1200mm)
    ?assertMatch({ok, "010103"}, commit_movement(Movement2)),
    
    % now try again to move to a "good" location - with lot's of checks
    {ok, Movement3} = init_movement_to_good_location(Mui),
    % while movement is initialized the unit is still booked on the old location
    % Location2 = mypl_db_util:get_mui_location(Mui),
    
    % try to initiate an other movement on that Mui - shouldn't be possible
    ?assertMatch({error, not_movable, _}, init_movement(Mui, "010101")),
    % Location2 = mypl_db_util:get_mui_location(Mui),
    
    % we rollback the whole thing ... so the unit should still be in it's old location
    ?assertMatch({ok, "010103"}, rollback_movement(Movement3)),
    
    % check issues with two units
    Mui2 = "14601-" ++ mypl_util:generate_mui(),
    % generate and Store Unit of 6*14601 (1200mm high) on "EINLAG"
    {ok, Mui2} = store_at_location("EINLAG", Mui2, 6, "a0001", 1200),
    {ok, Movement4} = init_movement_to_good_location(Mui),
    {ok, Movement5} = init_movement_to_good_location(Mui2),
    commit_movement(Movement5),
    commit_movement(Movement4),
    
    % remove Muis from warehouse
    ?assertMatch({ok, {5, "a0001"}}, retrieve(Mui)),
    ?assertMatch({ok, {6, "a0001"}}, retrieve(Mui2)),
    ok.
    

%%% @hidden
mypl_simple_pick_test() ->
    test_init(),
    % generate a MUI for testing
    Mui = "a0002-" ++ mypl_util:generate_mui(),
    % generate and Store Unit of 5*14601 (1200mm high) on "010101"
    ?assertMatch({ok, Mui}, store_at_location("010101", Mui, 70, "a0002", 1950)),
    % start picking.
    {ok, Pick1} = init_pick(30, Mui),
    
    ?assertMatch([Pick1], mypl_db_query:pick_list()),
    ?assertMatch({ok, _}, mypl_db_query:pick_info(Pick1)),
    
    %% try to initiate an movement on that Mui - shouldn't be possible with open picks
    ?assertMatch({error, not_movable, _}, init_movement_to_good_location(Mui)),
    % because of the open pick the unit shouldn't be movable
    %no = mypl_db_util:transaction(mypl_db_util:unit_movable(mypl_db_util:mui_to_unit_trans(Mui))),
    % commit it.
    ?assertMatch({ok, {30, "a0002"}}, commit_pick(Pick1)),
    % now ot should be movable again
    %yes = mypl_db_util:transaction(mypl_db_util:unit_movable(mypl_db_util:mui_to_unit_trans(Mui))),
     
    % start picking.
    {ok, Pick2} = init_pick(25, Mui),
    % commit it.
    ?assertMatch({ok, {25, "a0002"}}, commit_pick(Pick2)),
    
    % try to get to much
    ?assertMatch({error, not_enough_goods, _}, init_pick(25, Mui)),
    
    % try to get to much
    {ok, Pick4} = init_pick(15, Mui),
    ?assertMatch({ok, {15, "a0002"}}, rollback_pick(Pick4)),
    
    % check if enough is left on unit
    ?assertMatch({ok, {15, "a0002"}}, retrieve(Mui)),
    ok.
    

mypl_disbanding_test() ->
    test_init(),
    Mui = "XXX-" ++ mypl_util:oid(),
    % generate a MUI for testing
    ?assertMatch({ok, Mui}, store_at_location("010101", Mui, 1, "a0002", 1950)),
    {ok, Pick1} = init_pick(1, Mui),
    % since the pick empties the unit this should lead to disbanding
    ?assertMatch({ok, {1, "a0002"}}, commit_pick(Pick1)),
    % now mui1 should be gone
    ?assertMatch({error, _, _}, mypl_db_util:mui_to_unit_trans(Mui)),
    
    ?assertMatch({ok, "mui2"}, store_at_location("010101", "mui2", 1, "a0003", 1950)),
    ?assertMatch({ok, _Movement2}, init_movement_to_good_location("mui2")),
    Unit2 = mypl_db_util:mui_to_unit_trans("mui2"),
    ?assertMatch({atomic, {error, inconsistent_disband, _}}, mnesia:transaction(fun() -> disband_unit(Unit2) end)),
    ok.
    

store_at_location_multi_test() ->
    Elements1 = [{7, "a0006", 1200}, {11, "a0007", 1000}],
    {ok, [_Mui1, _Mui2]} = store_at_location_multi(id1, "EINLAG", Elements1, []),
    % if called again with the same id, no new units should be insered
    duplicate_id = store_at_location_multi(id1, "EINLAG", Elements1, []),
    
    % check if this also works with lists instead of tuple - to help with Json
    Elements2 = [[7, "a0006", 1200], [11, "a0007", 1000]],
    {ok, [_Mui3, _Mui4]} = store_at_location_multi([id2, "EINLAG", Elements2, []]),
    % if called again with the same id, no new units should be insered
    duplicate_id = store_at_location_multi({id2, "EINLAG", Elements2, []}),
    ok.
    

mypl_correction_test() ->
    test_init(),
    ?assertMatch({ok, "mui1"}, store_at_location("EINLAG", "mui1", 15, "a0001", 1200)),
    ?assertMatch({ok, "mui2"}, store_at_location("EINLAG", "mui2", 99, "a0002", 1200)),
    ?assertMatch({ok, {14, "mui1"}}, correction(id1, "mui1", 15, "a0001", -1, [])),
    ?assertMatch({ok, {13, "mui1"}}, correction(id2, "mui1", 14, "a0001", -1, [])),
    
    % duplicate id
    ?assertMatch({error, duplicate_id, _}, correction(id2, "mui1", 13, "a0001", -1, [])),
    % wrong mui
    ?assertMatch({error, missmatch, {13, _, _, _, "mui2"}}, correction(id3, "mui2", 13, "a2222", -2, [])),
    % wrong quantity
    ?assertMatch({error, missmatch, {15, "a0001", 13, "a0001", "mui1"}}, correction(id4, "mui1", 15, "a0001", -2, [])),
    % wrong product
    ?assertMatch({error, missmatch, _}, correction(id5, "mui1", 13, "a2222", -2, [])),
    ok.
    
% Test Bugfix for Case 3463
correction_impossible_for_archived_units_f3463_test() ->
    test_init(),
    ?assertMatch({ok, "mui1"}, store_at_location("EINLAG", "mui1", 15, "a0001", 1200)),
    ?assertMatch({ok, {15, "a0001"}}, retrieve("mui1")),
    ?assertMatch({error,unknown_mui,{"mui1"}}, correction(id6, "mui1", 15, "a0001", -2, [])),
    ?assertMatch({error,unknown_mui,{"mui1"}}, correction(id7, "mui1", 15, "a0001", -2, [])),
    ok.
    

do_not_commit_retrievals_as_movements_f3463_test() ->
    test_init(),
    % generate a MUI for testing
    Mui = "a0001-" ++ mypl_util:oid(),
    ?assertMatch({ok, Mui}, store_at_location("EINLAG", Mui, 5, "a0001", 1200)),
    {ok, Movement1} = init_movement(Mui, "010101", [{kernel_type, retrieval}]),
    
    % This shouldn't work because retrievals can't be commited by commit_movement()
    ?assertMatch({error,retrieval_is_no_movement, _}, commit_movement(Movement1)),
    % ... but by commit_retrieval()
    ?assertMatch({ok,{5,"a0001"}}, commit_retrieval(Movement1)),
    {ok, Info} = mypl_db_query:unit_info(Mui),
    {Attributes} = proplists:get_value(attributes, Info),
    ?assertMatch(archived, proplists:get_value(status, Attributes)),
    ok.
    

mypl_update_movement_test() ->
    test_init(),
    {ok, Mui} = store_at_location("EINLAG", "mui1", 5, "a0001", 1200),
    {ok, Movement1} = init_movement(Mui, "010101"),
    ok = update_movement({attributes, Movement1, [{foo, bar}]}),
    {ok, Properties} = mypl_db_query:movement_info(Movement1),
    Attributes = proplists:get_value(attributes, Properties),
    ?assertMatch(bar, proplists:get_value(foo, Attributes)),
    ok.
    

mypl_update_pick_test() ->
    test_init(),
    {ok, Mui} = store_at_location("EINLAG", "mui1", 5, "a0001", 1200),
    {ok, Pick1} = init_pick(2, Mui),
    ok = update_pick({attributes, Pick1, [{foo, bar}]}),
    {ok, Properties} = mypl_db_query:pick_info(Pick1),
    Attributes = proplists:get_value(attributes, Properties),
    ?assertMatch(bar, proplists:get_value(foo, Attributes)),
    ok.
    

testrunner() ->
    test_init(),
    mypl_simple_movement_test(),
    mypl_simple_pick_test(),
    mypl_disbanding_test(),
    store_at_location_multi_test(),
    mypl_correction_test().
    
-endif.
