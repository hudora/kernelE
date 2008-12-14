%% @version 0.2
%% Created :  Created by Maximillian Dornseif on 2007-10-07.

%% @copyright 2007 HUDORA GmbH
%% @author Maximillian Dornseif <md@hudora.de>
%% @doc myPL/kernel-E storage Engine auxiliary functions
%%
%% This implements helper functions for {@link mypl_db}. The only interesting finction here is
%% {@link best_location/1} which implements a policy for storing goods which freshly enter the Warehouse.
%%
%% @end

-module(mypl_db_util).

-include_lib("stdlib/include/qlc.hrl").
-include("mypl.hrl").

% Bei der Distanzberechnung wollen wir, dass nicht sklavisch die geringste Distanz genommen wird, sondern
% "Klassen" von Entfernungen genommen werden. Zur zeit liegt die Maximale Distanz im Lager ca. bei 60 Einheiten.
% Indem wir durch 16 Teilen, bekommen wir ca. 4 Entfernungsklassen, nach denen dann Sortiert wird.
-define(DISTANCESCALINGFACTOR, 20).


%% API
-export([do/1, do_trans/1, transaction/1, get_mui_location/1,
         mui_to_unit/1, mui_to_unit_archive/1, mui_to_unit_archive_trans/1, mui_to_unit_trans/1,
         unit_picks/1, unit_movement/1, unit_moving/1, unit_movable/1,
         find_empty_location/1, find_empty_location_nice/1, best_location/1, best_locations/2,
         read_location/1, find_movable_units/1]).

%% @private
%% @doc helper function for wraping {@link qlc} queries in an {@link mnesia} transaction.
do(Q) ->
    qlc:e(Q).

do_trans(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

-spec transaction(function()) -> any().
transaction(Fun) when is_function(Fun)->
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.


% @private
%% @doc finds the location where a unit is currently placed
-spec get_mui_location(mypl_db:muID()) -> #location{allocated_by::[any()]}.
get_mui_location(Mui) ->
    Unit = mui_to_unit(Mui),
    Unit#unit.location,
    case mnesia:read({location, Unit#unit.location}) of
        [] ->
             erlang:error({internal_error, mui_without_location1, {Mui, Unit}});
        [Location] ->
            case [X || X <- Location#location.allocated_by, X =:= Unit#unit.mui] of
                [] ->
                    % we found a unit without a location - fix it py putting it onto FEHLER
                    % we have to use a different process to escape the failing transaction
                    spawn(fun() -> mypl_integrity:orphaned_unit(Unit) end),
                    % exit wit an error
                    erlang:error({internal_error, mui_without_location2, {"FEHLER! Unit behauptete auf '"
                                  ++ Unit#unit.location
                                  ++ "' zu stehen, aber die Location hatte keine entsprechenden Daten."
                                  ++ " Wurde auf FEHLER gebucht.",
                                  Mui, Unit, Location}});
                [_] -> ok
            end,
            Location
    end.
    

%% @private
%% @doc returns the Unit identified by a Mui
%% This expect to be called within a transaction
-spec mui_to_unit(mypl_db:muiID()) -> #unit{}|{'error', 'unknown_mui', term()}.
mui_to_unit(Mui) ->
    case mnesia:read({unit, Mui}) of
        [Unit] ->
            Unit;
        [] ->
            {error, unknown_mui, {Mui}};
        Wrong ->
            {error, unknown_mui, {Mui, Wrong}}
    end.
    

%% @doc this calles mui_to_unit/1 with a sorrunding transaction
-spec mui_to_unit_trans(mypl_db:muiID()) -> #unit{}|{'error', 'unknown_mui', term()}.
mui_to_unit_trans(Mui) ->
    Fun = fun() ->
        mui_to_unit(Mui)
    end,
    mypl_db_util:transaction(Fun).
    

%% @private
%% @doc like mui_to_unit() but pulls the unit from the archive if needed
%% This expect to be called within a transaction
-spec mui_to_unit_archive(mypl_db:muiID()) ->
    #unit{}|{'error', 'unknown_mui', {mypl_db:muiID()}|{mypl_db:muiID(),_}}.
mui_to_unit_archive(Mui) ->
    case mui_to_unit(Mui) of
        {error, _, _} ->
            % not found in the active database - check archive
            case mypl_audit:get_from_archive(unit, Mui) of
                [] ->
                    {error, unknown_mui, {Mui}};
                [Unit] ->
                    Unit#unit{attributes=Unit#unit.attributes ++ [{status, archived}]};
                Wrong ->
                    {error, unknown_mui, {Mui, Wrong}}
            end;
        Unit ->
            Unit
    end.
    

%% @private
%% @doc like mui_to_unit_archive() but does not neet to be called from within a transaction
-spec mui_to_unit_archive_trans(mypl_db:muiID()) -> #unit{}|{'error', 'unknown_mui', term()}.
mui_to_unit_archive_trans(Mui) ->
    Fun = fun() ->
        mui_to_unit_archive(Mui)
    end,
    mypl_db_util:transaction(Fun).



%% @private
%% @doc returns the movement record for a unit or false if unit is not moving.
-spec unit_movement(#unit{}) -> false|#movement{}.
unit_movement(Unit) ->
    case do(qlc:q([X || X <- mnesia:table(movement), X#movement.mui =:= Unit#unit.mui])) of
        [] ->
            false;
        [Movement] ->
            Movement
    end.
    
%% @private
%% @doc checks if a Unit can be moved, returns yes if so, else no
-spec unit_moving(#unit{}) -> 'yes'|'no'.
unit_moving(Unit) ->
    % check for no open movements
    case unit_movement(Unit) of
        false -> no;
        _ -> yes
    end.
    

%% @private
%% @doc returns the pick records for a unit.
-spec unit_picks(#unit{}) -> [#pick{}].
unit_picks(Unit) ->
    do(qlc:q([X || X <- mnesia:table(pick), X#pick.from_unit =:= Unit#unit.mui])).
    

%% @private
%% @doc checks if a Unit can be moved, returns yes if so, else no
-spec unit_movable(#unit{}) -> 'yes'|'no'.
unit_movable(Unit) ->
    % Check if there is no other open movements and no open picks.
    if 
        % check for no open picks
        Unit#unit.pick_quantity =< 0 -> 
            % check for no open movements
            case unit_moving(Unit) of
                yes -> no;
                no -> yes
            end;
        true -> 
            no
    end.
    

%% @doc returns a list of location ordered bo "goodness"
%% In here there are major policy decisions encoded.
%% Which means in this function there is a lot of the myPL specific know How encoded.
%%
%% Paletten, die auf EINLAG stehen, erhalten hier eine Sonderbehandlung.
-spec best_location_helper(#unit{}) -> [#location{}].
best_location_helper(Unit) ->
    % locations with preference == 0 are never considered
    Candidates = [X || X <- find_empty_location(Unit#unit.height), X#location.preference > 0],
    if
        % for Units on EINLAG we do no distance calculations but base on ABC classification
        "EINLAG" =:= Unit#unit.location -> Class = mypl_abcserver:get_abcclass(Unit#unit.product);
        true -> Class = unknown
    end,
    % order by heigth, so we prefer lower locations (and in addition order by preference)
    lists:sort(fun(A, B) ->
                   if 
                       "EINLAG" =:= Unit#unit.location ->
                            AdistanceExact = BdistanceExact = 0, % we ignore exact distance
                            % for Units on EINLAG we do no distance calculations but base on ABC classification
                            case Class of
                                a -> % order by distance from front - divide by 20 to get groups/bins of distances
                                    AdistanceClass = mypl_distance:distance("061301", A#location.name) div ?DISTANCESCALINGFACTOR,
                                    BdistanceClass = mypl_distance:distance("061301", B#location.name) div ?DISTANCESCALINGFACTOR;
                                b -> % products of class B are placed "randomly"
                                    AdistanceClass = 0,
                                    BdistanceClass = 0;
                                _ ->  % order by distance from back - divide by 10 to get groups/bins of distances
                                    AdistanceClass = mypl_distance:distance("194001", A#location.name) div ?DISTANCESCALINGFACTOR,
                                    BdistanceClass = mypl_distance:distance("194001", B#location.name) div ?DISTANCESCALINGFACTOR
                            end;
                        true -> % else
                            % movement within the warehouse ("Umlagerung")
                            % divide by 20 to get groups/bins of distances
                            AdistanceExact = mypl_distance:distance(Unit#unit.location, A#location.name),
                            BdistanceExact = mypl_distance:distance(Unit#unit.location, B#location.name),
                            AdistanceClass = AdistanceExact div ?DISTANCESCALINGFACTOR,
                            BdistanceClass = BdistanceExact div ?DISTANCESCALINGFACTOR
                    end,
                    % if one of the locations has preference == 1 this location should always be 
                    % sorted towards the end
                    if
                        (A#location.preference < 2) or (B#location.preference < 2) ->
                            % sort by giving preference the highest priority
                            {B#location.preference, AdistanceClass, A#location.height, AdistanceExact} 
                                < {A#location.preference, BdistanceClass, B#location.height, BdistanceExact};
                        true ->
                            % order by as near as possible, as low as possible, preference as high as possible
                            {AdistanceClass, A#location.height, B#location.preference, AdistanceExact}        % preference is inverted!
                                < {BdistanceClass, B#location.height, A#location.preference, AdistanceExact}  % higher preference values should be used first
                    end
               end, Candidates).
    % TODO: try to avoid floorlevel locations for not recently needed articles
    

%% @private
%% @see mypl_db:init_movement_to_good_location/1
%% @doc finds the best location for an Unit
-spec best_location(#unit{}) -> 'no_location_available' | #location{}.
best_location(Unit) when is_record(Unit, unit) ->
    Locations = best_location_helper(Unit),
    case Locations of
        [] ->
            error_logger:warning_msg("can't find a suitable location for ~w.", [Unit]),
            no_location_available;
        [H|_] ->
            H
    end.
    

%% @doc returns the "best" location to store a certain unit
-spec best_location('floorlevel'|'higherlevel',#unit{},[[]|#location{}]) -> []|#location{}.
best_location(floorlevel, Unit, Ignore) when is_record(Unit, unit) ->
    % order by heigth, so we prefer lower locations
    [H|_] = [X || X <- best_location_helper(Unit), X#location.floorlevel =:= true] -- Ignore,
    H;
best_location(higherlevel, Unit, Ignore) when is_record(Unit, unit) ->
    % order by heigth, so we prefer lower locations
    case [X || X <- best_location_helper(Unit), X#location.floorlevel =:= false] -- Ignore of
        [H|_] -> H;
        [] -> []
    end.

%% @doc suggest for each unit in units where it could be moved
%% returns a list of locations.
-spec best_locations('floorlevel'|'higherlevel',[#unit{}],[[]|#location{}]) -> [[]|#location{}].
best_locations(floorlevel, [], _) -> [];
best_locations(floorlevel, Units, Ignore) -> 
    [H|T] = Units,
    Best = best_location(floorlevel, H, Ignore),
    [Best|best_locations(floorlevel, T, [Best|Ignore])];
best_locations(higherlevel, [], _) -> [];
best_locations(higherlevel, Units, Ignore) -> 
    [H|T] = Units,
    Best = best_location(higherlevel, H, Ignore),
    [Best|best_locations(higherlevel, T, [Best|Ignore])].

%% @deprecated
-spec best_locations('floorlevel'|'higherlevel',[#unit{}]) -> [[]|#location{}].
best_locations(floorlevel, Units) -> 
    best_locations(floorlevel, Units, []);
best_locations(higherlevel, Units) -> 
    best_locations(higherlevel, Units, []).


%% @private
%% @doc reads a Location record
%% expects to be called within a mnesia transaction
-spec read_location(mypl_db:location()) -> unknown_location|#location{}.
read_location(Locname) when is_list(Locname)->
    case mnesia:read({location, Locname}) of
        [] ->
            error_logger:error_msg("unknown_location ~s", [Locname]),
            unknown_location;
        [Location] ->
            Location;
        L ->
            erlang:error({internal_error, Locname, L})
    end.
    

%% @private
%% @doc returns a list of all movable units for a product
-spec find_movable_units(mypl_db:content()) -> [#unit{}].
find_movable_units(Product) -> 
    Candidates = [X || X <- mnesia:match_object(#unit{product = Product, _ = '_'}), X#unit.pick_quantity =< 0],
    lists:filter(fun(X) -> mypl_db_util:unit_movable(X) =:= yes end, Candidates).
    

%% TODO: this ignores Multi Unit Locations
%% TODO: rename to find_empty_locationS
%% @private
%% @doc This generates a list of locations where a Unit of Heigth mm can be stored. The list is ordered
%% so that locations at the beginning of the list are preferable to the ones at the end of the list.
-spec find_empty_location(mypl_db:heigthMM()) -> [#location{}].
find_empty_location(Height) ->
    lists:reverse(lists:keysort(#location.preference, 
                                lists:keysort(#location.name,
                                              do(qlc:q([X || X <- mnesia:table(location),
                                               X#location.height >= Height,
                                               X#location.allocated_by =:= [],
                                               X#location.reserved_for =:= [],
                                               X#location.preference > 0]))))).


%% @deprecated
-spec find_empty_location_nice(mypl_db:heigthMM()) -> mypl_db:location().
find_empty_location_nice(Height) ->
    Fun = fun() ->
        [X#location.name || X <- find_empty_location(Height)]
    end,
    mypl_db_util:transaction(Fun).


% ~~ Unit tests
-ifdef(EUNIT).
-compile(export_all).

%%% @hidden
test_init() ->
    mypl_db:run_me_once(),
    % flush database
    mnesia:start(),
    mnesia:clear_table(unit),
    mnesia:clear_table(location),
    mnesia:clear_table(movement),
    mnesia:clear_table(pick),
    mnesia:clear_table(picklist),
    % regenerate locations
    % init_location(Name, Height, Floorlevel, Preference, Attributes)
    mypl_db:init_location("EINLAG", 6000, true,  0, [{no_picks}]),
    mypl_db:init_location("AUSLAG", 6000, true,  0, [{no_picks}]),
    mypl_db:init_location("010102", 1950, false, 6, []),
    
    mypl_db:init_location("011001", 2000, true,  2, []),
    mypl_db:init_location("011002", 2000, false, 7, []),
    mypl_db:init_location("011003", 2000, false, 6, []),
    
    mypl_db:init_location("012002", 2000, false, 7, []),
    
    mypl_db:init_location("013001", 1500, true,  2, []),
    mypl_db:init_location("013002", 1500, false, 7, []),
    mypl_db:init_location("013003", 1500, false, 6, []),
    
    mypl_db:init_location("200101", 2000, true,  9, []),
    mypl_db:init_location("200102", 1900, false, 9, []),
    mypl_db:init_location("200103", 1200, false, 9, []),
    mypl_db:init_location("200104", 3000, false, 9, []),
    ok.

%%% @hidden
%%% test if counting works as expected
choose_location_test() ->
    test_init(),
    {ok, "mui1"} = mypl_db:store_at_location("012002", "mui1", 5, "a0001", 1200),
    #location{name="013002"} = transaction(fun() -> best_location(mui_to_unit("mui1")) end),
    
    {ok, "mui2"} = mypl_db:store_at_location("012002", "mui2", 5, "a0001", 1999),
    #location{name="011002"} = transaction(fun() -> best_location(mui_to_unit("mui2")) end),
    
    {ok, "mui3"} = mypl_db:store_at_location("012002", "mui3", 5, "a0001", 1900),
    #location{name="010102"} = transaction(fun() -> best_location(mui_to_unit("mui3")) end),
    
    % very high, so it is moved to the back
    {ok, "mui4"} = mypl_db:store_at_location("012002", "mui4", 5, "a0001", 2100),
    #location{name="200104"} = transaction(fun() -> best_location(mui_to_unit("mui4")) end),
    ok.
    

choose_locations_test() ->
    test_init(),
    {ok, "mui1"} = mypl_db:store_at_location("012002", "mui1", 5, "a0001", 1200),
    % 013001 is the lowest fitting location
    [#location{name="013001"}] = transaction(fun() -> best_locations(floorlevel, [mui_to_unit("mui1")]) end),
    
    {ok, "mui2"} = mypl_db:store_at_location("012002", "mui2", 5, "a0001", 1999),
    [#location{name="011001"}] = transaction(fun() -> best_locations(floorlevel, [mui_to_unit("mui2")]) end),
    
    {ok, "mui3"} = mypl_db:store_at_location("012002", "mui3", 5, "a0001", 1900),
    [#location{name="011001"}] = transaction(fun() -> best_locations(floorlevel, [mui_to_unit("mui3")]) end),
    ok.
    

best_location_helper_test() ->
    % test that locations with priority 1 are ignored whereever possible
    test_init(),
    mypl_db:init_location("013001", 1500, true,  1, []),
    mypl_db:store_at_location("013002", "mui1", 5, "a0001", 1500),
    mypl_db:store_at_location("013003", "mui2", 5, "a0001", 1500),
    
    % the first result must not be 013001 - it is the best fit, but has priority 1,
    % so it should be the last element
    Candidates = transaction(fun() -> best_location_helper(mui_to_unit("mui1")) end),
    [#location{name="013001"}|_] = lists:reverse(Candidates),
    ok.
    

testrunner() ->
    choose_location_test(),
    choose_locations_test(),
    best_location_helper_test(),
    ok.
    
-endif.
