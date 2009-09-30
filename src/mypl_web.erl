%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for mypl.

-module(mypl_web).
-author('author <author@example.com>').

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
                "location" ->
                    Req:respond({200, [{"Content-Type", "application/json; charset=utf-8"}],
                                myjson:encode([list_to_binary(X) || X <- mypl_db_query:location_list()])});
                [$l,$o,$c,$a,$t,$i,$o,$n,$/|LocationId] ->
                    {ok, Info} = mypl_db_query:location_info(LocationId),
                    send_json(Req, {Info});
                
                "unit" ->
                    Req:respond({200, [{"Content-Type", "application/json; charset=utf-8"}],
                                myjson:encode([list_to_binary(X) || X <- mypl_db_query:unit_list()])});
                % /unit/123456789012345678
                [$u,$n,$i,$t,$/|UnitId] ->
                    {ok, Info} = mypl_db_query:unit_info(UnitId),
                    send_json(Req, {Info});
                
                "movement" ->
                    Req:respond({200, [{"Content-Type", "application/json; charset=utf-8"}],
                                myjson:encode([list_to_binary(X) || X <- mypl_db_query:movement_list()])});
                % /movement/1235
                
                "pick" ->
                    Req:respond({200, [{"Content-Type", "application/json; charset=utf-8"}],
                                myjson:encode([list_to_binary(X) || X <- mypl_db_query:pick_list()])});
                % /pick/12345
                
                % kommiauftrag (provpipeline)
                "kommiauftrag" ->
                    Req:respond({200, [{"Content-Type", "application/json; charset=utf-8"}],
                                myjson:encode([list_to_binary(X) || X <- mypl_prov_query:provpipeline_list()])});
                
                % statistics
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

