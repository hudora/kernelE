%%%-------------------------------------------------------------------
%%% File    : mypl_db_util
%%% Author  : Maximillian Dornseif
%%% Description : 
%%%
%%% Created :  Created by Maximillian Dornseif on 2007-10-07.
%%%-------------------------------------------------------------------

%% @version 0.1
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
-export([do/1, get_mui_location/1, mui_to_unit/1, unit_movable/1, best_location/1]).

%% @private
%% @doc helper function for wraping {@link qlc} queries in an {@link mnesia} transaction.
do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

% @private
%% @spec get_mui_location(muiID()) -> locationRecord()
%% @doc finds the location where a unit is currently placed
get_mui_location(Mui) ->
    Locations = do(qlc:q([X || X <- mnesia:table(location), X#location.allocated_by /= []])),
    [H|[]] = lists:filter(fun(X) -> lists:member(Mui, X#location.allocated_by) end, Locations),
    H.

%% @private
%% @spec mui_to_unit(muiID()) -> unitRecord()
%% @doc returns the Unit identified by a Mui
mui_to_unit(Mui) ->
    % TODO: add transaction
    Fun = fun() ->
        case mnesia:read({unit, Mui}) of
            [Unit] ->
                Unit;
            [] ->
                {error, unknown_mui, {Mui}};
            X ->
                {error, internal_erroe, {Mui, X}}
        end
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

%% @private
%% @spec unit_movable(unitRecord()) -> atom()
%% @doc checks if a Unit can be moved, returns yes if so, else no
unit_movable(Unit) ->
    % Check if there is no other open movements and no open picks.
    if 
        % check for no open picks
        Unit#unit.pick_quantity =< 0 -> 
            % check for no open movements
            L = do(qlc:q([X || X <- mnesia:table(movement), X#movement.mui =:= Unit#unit.mui])),
            if
                L =:= [] -> yes;
                true -> no
            end;
        true -> no
    end.

%% @private
%% @spec best_location(unitRecord()) -> locationRecord()
%% @see mypl_db:init_movement_to_good_location/1
%% @doc finds the best location for an Unit
best_location(Unit) ->
    Candidates = find_empty_location(Unit#unit.height),
    % order by heigth, so we prefer lower locations
    [H|_] = lists:keysort(#location.height, Candidates),
    H.
    

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
