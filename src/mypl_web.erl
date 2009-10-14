%% @author Maximillian Dornseif <md@hudora.de>
%% @copyright 2009 Maximillian Dornseif.

%% @doc Web server for mypl.

-module(mypl_web).
-author('Maximillian Dornseif <md@hudora.de>').

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                "" ->
                    Req:respond({200, [{"Content-Type", "text/plain"}], list_to_binary([
                                        "try: /location /unit /movement /pick /kommiauftrag"
                                        ])});

                "location" ->
                    Req:respond({200, [{"Content-Type", "application/json; charset=utf-8"}],
                                myjson:encode([list_to_binary(X) || X <- mypl_db_query:location_list()])});
                [$l,$o,$c,$a,$t,$i,$o,$n,$/|LocationId] ->
                    case mypl_db_query:location_info(LocationId) of
                        {ok, Info} ->  send_json(Req, {mypl_util:proplist_cleanup_binary(Info)});
                        {error, Type, Info} -> send_json(Req, 404, Type)
                    end; 
                
                "unit" ->
                    Req:respond({200, [{"Content-Type", "application/json; charset=utf-8"}],
                                myjson:encode([list_to_binary(X) || X <- mypl_db_query:unit_list()])});
                % /unit/123456789012345678
                [$u,$n,$i,$t,$/|UnitId] ->
                    case mypl_db_query:unit_info(UnitId) of
                        {ok, Info} -> send_json(Req, {mypl_util:proplist_cleanup_binary(Info)});
                        {error, Type, Info} -> send_json(Req, 404, Type)
                    end;                

                "movement" ->
                    Req:respond({200, [{"Content-Type", "application/json; charset=utf-8"}],
                                myjson:encode([list_to_binary(X) || X <- mypl_db_query:movement_list()])});
                % /movement/1235
                [$m,$o,$v,$e,$m,$e,$n,$t,$/|MovementId] ->
                    case mypl_db_query:movement_info(MovementId) of
                        {ok, Info} -> send_json(Req, {mypl_util:proplist_cleanup_binary(Info)});
                        {error, Type, Info} -> send_json(Req, 404, Type)
                    end;
                
                "pick" ->
                    Req:respond({200, [{"Content-Type", "application/json; charset=utf-8"}],
                                myjson:encode([list_to_binary(["/pick/"|X]) || X <- mypl_db_query:pick_list()])});
                % /pick/12345
                
                % kommiauftrag (provpipeline)
                "kommiauftrag" ->
                    Req:respond({200, [{"Content-Type", "application/json; charset=utf-8"}],
                                myjson:encode([list_to_binary(X) || X <- mypl_prov_query:provpipeline_list()])});
                [$k,$o,$m,$m,$i,$a,$u,$f,$t,$r,$a,$g,$/|KommiauftragNr] ->
                    Req:respond({200, [{"Content-Type", "application/json; charset=utf-8"}],
                                myjson:encode([mypl_prov_query:provpipeline_info()])});
                    
                
                %"statistik" ->
                % puffer, fuer picks, die noch ausgegeben werden muessen
                %pickpipeline
                % puffer, fuer retrievals, die noch ausgegeben werden muessen
                %retrievalpipeline
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.


% send_json is from couchdb
send_json(Req, Value) ->
    send_json(Req, 200, Value).

send_json(Req, Code, Value) ->
    send_json(Req, Code, [], Value).

send_json(Req, Code, Headers, Value) ->
    DefaultHeaders = [
        {"Content-Type", "application/json"},
        {"Cache-Control", "must-revalidate"}
    ],
    Req:respond({Code, DefaultHeaders ++ Headers, myjson:encode(Value) ++ "\n"}).

