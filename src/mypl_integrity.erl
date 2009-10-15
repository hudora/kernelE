%% @version 0.2
%%% File    : mypl_integrity.erl
%%% Author  : Maximillian Dornseif
%%% Created :  Created by Maximillian Dornseif on 2007-10-31.
%% @doc self_test functionality for mypl
-module(mypl_integrity).

-include_lib("stdlib/include/qlc.hrl").
-include("mypl.hrl").

-import(lists).
-import(mnesia).
-import(mypl_db_util).

-export([selftest/0, pick_quantity_per_unit/0, locations_pointing_nowhere/0, orphaned_units/0, orphaned_unit/1,
         kommischein_defekt/0, orphaned_pickpipeline/0, orphaned_retrievalpipeline/0, kommiauftrag_offen/0]).


%% @doc check that pick_quantity corrospondents to the number of picks
pick_quantity_per_unit() ->
    lists:all(fun(X) -> X =:= ok end,
              mypl_db_util:do_trans(qlc:q([pick_quantity_per_unit_helper(X) || X <- mnesia:table(unit)]))).
    
pick_quantity_per_unit_helper(Unit) ->
    PickIds  = mypl_db_util:do(qlc:q([X#pick.id || X <- mnesia:table(pick), X#pick.from_unit =:= Unit#unit.mui])),
    RealPickQuantity = lists:sum(lists:map(fun(PickId) ->
                                    [Pick] = mnesia:read({pick, PickId}),
                                    Pick#pick.quantity
                                    end, 
                                    PickIds) ++ [0]),
    case Unit#unit.pick_quantity /= RealPickQuantity of
        true ->
            ErrorText = "FEHLER! Unit '" ++ Unit#unit.mui ++ "' behauptete eine Pickmenge von '" ++ 
                        integer_to_list(Unit#unit.pick_quantity) ++ " tatsaechliche Pickmenge ist aber " ++
                        integer_to_list(RealPickQuantity) ++ ". Pickmenge wurde korregiert.",
            TransFun = fun() ->
                erlang:display(ErrorText),
                ok = mnesia:write(Unit#unit{pick_quantity=RealPickQuantity}),
                mypl_audit:unitaudit(Unit, ErrorText),
                error_logger:error_msg(ErrorText)
            end,
            {atomic, _} = mnesia:transaction(TransFun),
            erlang:display({RealPickQuantity, Unit#unit.pick_quantity, Unit}),
            error;
        _ ->
            ok
    end.
    

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
                erlang:display(ErrorText),
                ok = mnesia:write(Destination#location{allocated_by=[Unit#unit.mui|Destination#location.allocated_by]}),
                ok = mnesia:write(Unit#unit{location=Destination#location.name}),
                mypl_audit:unitaudit(Unit, ErrorText),
                error_logger:error_msg(ErrorText)
            end,
            {atomic, _} = mnesia:transaction(TransFun),
            error
    end.


kommiauftrag_offen() ->
    lists:all(fun(X) -> X =:= ok end, [kommiauftrag_offen_per_auftrag(Y) || Y <- mypl_prov_query:provpipeline_list_processing()]).
    

kommiauftrag_offen_per_auftrag(Auftrag) ->
    {_, Attributes, Positions} = Auftrag,
    [offen_per_kommischein(X) || X <- proplists:get_value(provisioninglists, Attributes)].

    
kommischein_defekt() ->
    [offen_per_kommischein(X) || X <- mypl_prov_query:provisioninglist_list()].

% gibt false zurueck, fals der kommischein nicht existiert
offen_per_kommischein(KommischeinId) ->
    case mypl_prov_query:provisioninglist_info(KommischeinId) of
        {ok, Kommischein} ->
            Positionen = [offen_per_kommipos(X) || X <- proplists:get_value(provisioning_ids, Kommischein)],
            case lists:all(fun(X) -> X =:= false end, Positionen) of
                true ->
                    Fun = fun() ->
                        case mnesia:read({provpipeline, Kommischein#provisioninglist.provpipeline_id}) of
                            [PPEntry] ->
                                mypl_audit:kommiauftragaudit(Kommischein#provisioninglist.provpipeline_id,
                                                             "Kommischein " ++ KommischeinId ++ "geloescht, da die einzelnen Positionen nicht mehr existieren",
                                                             integrity_offen_per_kommischein,
                                                             [{kommischein, KommischeinId}]);
                            _ ->
                                error_logger:warning_msg("Kommischein ~s gehoert zu Kommiauftrag ~s - dieser existiert aber nciht mehr",
                                                         [KommischeinId, Kommischein#provisioninglist.provpipeline_id]),
                                hat_keinen_kommiauftrag_mehr
                        end,
                        mypl_audit:archive(Kommischein#provisioninglist{status=deleted,
                                           attributes=[{commited_at, mypl_util:timestamp2binary()}
                                                        |Kommischein#provisioninglist.attributes]},
                                           commit_anything),
                        ok = mnesia:delete({provisioninglist, KommischeinId})
                    end,
                    mypl_db_util:transaction(Fun),
                    mypl_zwitscherserver:zwitscher("Kommischein ~s hat keine offenen Positionen - wird geloescht #error",
                                                   [KommischeinId]),
                    error_logger:warning_msg("Kommischein ~s hat keine offenen Positionen - wird geloescht #error",
                                                   [KommischeinId]),
                    false;
                _ ->
                    true
            end;
        {error,unknown_provisioninglist, _} ->
            false
    end.

% gibt false zurueck, fallss die position nicht existiert
offen_per_kommipos(KommiPodId) ->
    case mypl_db_query:pick_info(KommiPodId) of
        {error, _, _} ->
            case mypl_db_query:movement_info(KommiPodId) of
                {error, _, _} ->
                    false;
                _ ->
                    true
            end;
        _ ->
            true
    end.



%% verify location records actually have their allocated_by and reserved_by fields pointing to something actually existing
locations_pointing_nowhere() ->
    Fun = fun() ->
        lists:all(fun(X) -> X =:= ok end, lists:flatten(mypl_db_util:do(qlc:q([location_pointing_nowhere(X) || X <- mnesia:table(location)]))))
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

% find entries in provpipeline_processing that
% a) have no corrospondendin entry in provpipeline
% b) have non existing movements or picks


run_a_test(Testname) ->
    {TimeMicro , Res} =  timer:tc(?MODULE, Testname, []),
    TimeMilli = TimeMicro div 1000,
    TimeSec = TimeMilli / 1000,
    io:format("~w   ~f s   ~w~n", [Testname, TimeSec, Res]).
    
selftest() ->
    TestList = [pick_quantity_per_unit, locations_pointing_nowhere, orphaned_units, orphaned_pickpipeline, orphaned_retrievalpipeline],
    lists:map(fun(X) -> run_a_test(X) end, TestList),
    ok.
    