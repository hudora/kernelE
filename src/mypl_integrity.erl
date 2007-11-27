%% @version 0.2
%%% File    : mypl_check
%%% Author  : Maximillian Dornseif
%%% Created :  Created by Maximillian Dornseif on 2007-10-31.
%% @doc self_test functionality for mypl
-module(mypl_integrity).

-include_lib("stdlib/include/qlc.hrl").
-include("mypl.hrl").

-import(lists).
-import(mnesia).
-import(mypl_db_util).

-export([selftest/0, locations_pointing_nowhere/0, orphaned_units/0, orphaned_unit/1,
         orphaned_pickpipeline/0, orphaned_retrievalpipeline/0]).


%% @doc check for Units which are not actually booked into a Location
orphaned_units() ->
    lists:all(fun(X) -> X =:= ok end, mypl_db_util:do_trans(qlc:q([orphaned_unit(X) || X <- mnesia:table(unit)]))).
    
orphaned_unit(Unit) ->
    Location = mypl_db_util:read_location(Unit#unit.location),
    case lists:member(Unit#unit.mui, Location#location.allocated_by) of
        true ->
            ok;
        false ->
            % we found a unit without a location - fix it py putting it onto FEHLER
            Destination = mypl_db_util:read_location("FEHLER"),
            ErrorText = "FEHLER! Unit behauptete auf '" ++ Unit#unit.location
                        ++ "' zu stehen, aber die Location hatte keine entsprechenden Daten."
                        ++ " Wurde auf FEHLER gebucht.",
            TransFun = fun() ->
                ok = mnesia:write(Destination#location{allocated_by=[Unit#unit.mui|Destination#location.allocated_by]}),
                ok = mnesia:write(Unit#unit{location=Destination#location.name}),
                mypl_audit:unitaudit(Unit, ErrorText),
                error_logger:error_msg(ErrorText)
            end,
            {atomic, _} = mnesia:transaction(TransFun),
            erlang:display(ErrorText),
            error
    end.


%% verify location records actually have their allocated_by and reserved_by fields pointing to something actually existing
locations_pointing_nowhere() ->
    Fun = fun() ->
        Locations = mypl_db_util:do(qlc:q([location_pointing_nowhere(X) || X <- mnesia:table(location)])),
        lists:all(fun(X) -> X =:= ok end, Locations)
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

location_pointing_nowhere(Location) ->
    lists:map(fun(Mui) -> location_pointing_nowhere_allocated_helper(Location, Mui) end,
              Location#location.allocated_by).

location_pointing_nowhere_allocated_helper(Location, Mui) ->
    case mnesia:read({unit, Mui}) of
        [Unit] ->
            % check if unit also think it is located here.
            case Unit#unit.location =:= Location#location.name of
                true ->
                    ok;
                false ->
                    ErrorText = "FEHLER! Location '" ++ Location#location.name 
                                ++ "' hat MUI '" ++ Mui ++ "' als allocated_by verbucht, diese"
                                ++ " Unit denkt jedoch, sie stehe auf '" ++ Unit#unit.location
                                ++ "'. allocated_by wurde angepasst.",
                    ok = mnesia:write(Location#location{allocated_by=Location#location.allocated_by -- [Mui]}),
                    mypl_audit:unitaudit_mui(Mui, ErrorText),
                    error_logger:error_msg(ErrorText),
                    error
            end;
        [] ->
            % The location claims to be have a certain unit on it, but that unit does not exist.
            ErrorText = "FEHLER! Location '" ++ Location#location.name 
                        ++ "' hat MUI '" ++ Mui ++ "' als allocated_by verbucht, diese"
                        ++ " Unit ist jedoch unbekannt."
                        ++ " Eintrag wurde entfernt.",
            ok = mnesia:write(Location#location{allocated_by=Location#location.allocated_by -- [Mui]}),
            mypl_audit:unitaudit_mui(Mui, ErrorText),
            error_logger:error_msg(ErrorText),
            error
    end.
    

% TODO: check for every entry in retrivalpipeline there are the associated movements
% dito for pickpipeline
%% @doc finds pickepipeline entries which don't have associated picks anymore.
orphaned_pickpipeline() ->
    lists:all(fun(X) -> X =:= ok end, 
              mypl_db_util:do_trans(qlc:q([orphaned_pickpipeline(X) || X <- mnesia:table(pickpipeline)]))).
    
orphaned_pickpipeline(Entry) ->
    lists:map(fun(PickId) ->
                  {ok, _} = mypl_db_query:pick_info(PickId)
              end, Entry#pickpipeline.pickids).

orphaned_retrievalpipeline() ->
    lists:all(fun(X) -> X =:= ok end, 
              mypl_db_util:do_trans(qlc:q([orphaned_retrievalpipeline(X) || X <- mnesia:table(retrievalpipeline)]))).
    
orphaned_retrievalpipeline(Entry) ->

    lists:map(fun(RetrievalId) ->
                  {ok, _} = mypl_db_query:movement_info(RetrievalId)
              end, Entry#retrievalpipeline.retrievalids).


run_a_test(Testname) ->
    {TimeMicro , Res} =  timer:tc(?MODULE, Testname, []),
    TimeMilli = TimeMicro div 1000,
    TimeSec = TimeMilli / 1000,
    io:format("~w   ~f s   ~w~n", [Testname, TimeSec, Res]).
    
selftest() ->
    TestList = [locations_pointing_nowhere, orphaned_units, orphaned_pickpipeline, orphaned_retrievalpipeline],
    lists:map(fun(X) -> run_a_test(X) end, TestList),
    ok.
    