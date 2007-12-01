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

%% API
-export([do/1, do_trans/1, transaction/1, get_mui_location/1, mui_to_unit/1, mui_to_unit_trans/1,
         unit_picks/1, unit_movement/1, unit_moving/1, unit_movable/1,
         best_location/1, best_locations/2,
         read_location/1, find_movable_units/1]).

%% @private
%% @doc helper function for wraping {@link qlc} queries in an {@link mnesia} transaction.
do(Q) ->
    qlc:e(Q).

do_trans(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

transaction(Fun) ->
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.


% @private
%% @spec get_mui_location(muiID()) -> locationRecord()
%% @doc finds the location where a unit is currently placed
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
                    erlang:error({internal_error, mui_without_location2, {"FEHLER! Unit behauptete auf '" ++ Unit#unit.location ++ "' zu stehen, aber die Location hatte keine entsprechenden Daten. Wurde auf FEHLER gebucht.",
                                                                          Mui, Unit, Location}});
                [_] -> ok
            end,
            Location
    end.
    

%% @private
%% @spec mui_to_unit(muiID()) -> unitRecord()
%% @doc returns the Unit identified by a Mui
%%
%% This expect to be called within a transaction
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
mui_to_unit_trans(Mui) ->
    Fun = fun() ->
        mui_to_unit(Mui)
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

%% @private
%% @spec unit_movement(unitRecord()) -> mypl_db:movementRecort()
%% @doc returns the movement record for a unit or false if unit is not moving.
unit_movement(Unit) ->
    case do(qlc:q([X || X <- mnesia:table(movement), X#movement.mui =:= Unit#unit.mui])) of
        [] ->
            false;
        [Movement] ->
            Movement
    end.
    
%% @private
%% @spec unit_picks(unitRecord()) -> mypl_db:movementRecort()
%% @doc returns the pick records for a unit.
unit_picks(Unit) ->
    do(qlc:q([X || X <- mnesia:table(pick), X#pick.from_unit =:= Unit#unit.mui])).
    

%% @private
%% @spec unit_moving(unitRecord()) -> atom()
%% @doc checks if a Unit can be moved, returns yes if so, else no
unit_moving(Unit) ->
    % check for no open movements
    case unit_movement(Unit) of
        false -> no;
        _ -> yes
    end.
    

%% @private
%% @spec unit_movable(unitRecord()) -> atom()
%% @doc checks if a Unit can be moved, returns yes if so, else no
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
    

best_location_helper(Unit) ->
    % locations with preference == 0 are never considered
    Candidates = [X || X <- find_empty_location(Unit#unit.height), X#location.preference > 0],
    % order by heigth, so we prefer lower locations (and in addition order by preference)
    lists:sort(fun(A, B) ->
                   {A#location.height, A#location.preference} < {B#location.height, B#location.preference}
               end, Candidates).

%% @private
%% @spec best_location(unitRecord()) -> locationRecord()
%% @see mypl_db:init_movement_to_good_location/1
%% @doc finds the best location for an Unit
best_location(Unit) ->
    Locations = best_location_helper(Unit),
    case Locations of
        [] ->
            error_logger:warning_msg("can't find a suitable location for ~w.", [Unit]),
            no_location_available;
        [H|_] ->
            H
    end.
    

best_location(floorlevel, Unit, Ignore) ->
    % order by heigth, so we prefer lower locations
    [H|_] = [X || X <- best_location_helper(Unit), X#location.floorlevel =:= true] -- Ignore,
    H.
%best_location(floorlevel, Unit) -> best_location(floor, Unit, []).

best_locations(floorlevel, [], _) -> [];
best_locations(floorlevel, Units, Ignore) -> 
    [H|T] = Units,
    Best = best_location(floorlevel, H, Ignore),
    [Best|best_locations(floorlevel, T, [Best|Ignore])].
best_locations(floorlevel, Units) -> 
    best_locations(floorlevel, Units, []).

%% @private
%% @spec read_location(string()) -> mypl_db:unitRecord()
%% @doc reads a Unit record
%%
%% expects to be called within a mnesia transaction
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
%% find_movable_units(string()) -> [mypl_db:unitRecord()]
%% @doc returns a list of all movable units for a product
find_movable_units(Product) -> 
    Candidates = mypl_db_util:do(qlc:q([X || X <- mnesia:table(unit), X#unit.product =:= Product,
                                                         X#unit.pick_quantity =< 0])),
    lists:filter(fun(X) -> mypl_db_util:unit_movable(X) =:= yes end, Candidates).
    

%% TODO: this ignores Multi Unit Locations
%% TODO: rename to find_empty_locationS
%% @private
%% @spec find_empty_location(heightMM()) -> List
%%      List = [locationRecord()]
%% @doc This generates a list of locations where a Unit of Heigth mm can be stored. The list is ordered
%% so that locations at the beginning of the list are preferable to the ones at the end of the list.
find_empty_location(Height) ->
    lists:reverse(lists:keysort(#location.preference, 
                                lists:keysort(#location.name,
                                              do(qlc:q([X || X <- mnesia:table(location), 
                                               X#location.height >= Height, 
                                               X#location.allocated_by =:= [], 
                                               X#location.reserved_for =:= [], 
                                               X#location.preference > 0]))))).

%find_empty_floor_location(Height) ->
%    lists:filter(fun(X) -> X#location.floorlevel =:= true end, find_empty_location(Height)).
