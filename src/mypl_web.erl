%% @author Maximillian Dornseif <md@hudora.de>
%% @copyright 2009 Maximillian Dornseif.

%% @doc Web server for mypl.

-module(mypl_web).
-author('Maximillian Dornseif <md@hudora.de>').

-export([start/1, stop/0, loop/2]).

%% External API

-spec start([any()]) -> any().
start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).
    

-spec stop() -> any().
stop() ->
    mochiweb_http:stop(?MODULE).
    

-spec loop(atom(),_) -> any().
loop(Req, _DocRoot) ->
    "/" ++ Path = Req:get(path),
    mypl_log:log("~s ~s", [Req:get(method), Req:get(path)], {[{level, http}]}),
    case Path of
        "" ->
            case Req:get(method) of
                Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                    Req:respond({200, [{"Content-Type", "text/plain"}], list_to_binary([
                                        "try: /statistics /abc /requesttracker\n"
                                        "/kommiauftrag /kommischein /location /unit /movement /pick /product\n"
                                        ])});
                _ ->
                    Req:respond({501, [], []})
            end;
        
        "statistics" ->
            case Req:get(method) of
                Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                    send_json(Req, {mypl_statistics:statistics()});
                _ ->
                    Req:respond({501, [], []})
            end;
        
        "abc" ->
            case Req:get(method) of
                Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                    {Araw, Braw, Craw} = mypl_abcserver:get_abc(),
                    send_json(Req, {[{a, [[N, mypl_util:ensure_binary(A)] || {N, A} <- Araw]},
                                     {b, [[N, mypl_util:ensure_binary(A)] || {N, A} <- Braw]},                                     
                                     {c, [[N, mypl_util:ensure_binary(A)] || {N, A} <- Craw]}
                                     ]});
                _ ->
                    Req:respond({501, [], []})
            end;
        
        "requesttracker" ->
            case Req:get(method) of
                Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                    Data = [{[{artnr, mypl_util:ensure_binary(ArtNr)},
                              {menge, Quantity},
                              {lastseen, mypl_util:ensure_binary(Timestamp)},
                              {priority, mypl_util:ensure_binary(lists:flatten(io_lib:format("~p", [Priority])))}
                             ]}
                              || {ArtNr, Quantity, Timestamp, Priority} <- mypl_requesttracker:dump_requests()],
                    send_json(Req, Data);
                _ ->
                    Req:respond({501, [], []})
            end;
        
        % kommiauftrag (provpipeline)
        "kommiauftrag" ->
            case Req:get(method) of
                Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                    Req:respond({200, [{"Content-Type", "application/json; charset=utf-8"}],
                                myjson:encode([list_to_binary(X) || X <- mypl_prov_query:provpipeline_list()])});
                _ ->
                    Req:respond({501, [], []})
            end;
        [$k,$o,$m,$m,$i,$a,$u,$f,$t,$r,$a,$g,$/|KommiauftragNr] ->
            case Req:get(method) of
                Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                    case mypl_prov_query:provpipeline_info(KommiauftragNr) of
                        unknown -> send_json(Req, 404, <<"unknown Kommiauftrag">>);
                        Info -> send_json(Req, Info)
                    end;
                'DELETE' ->
                    Message = Req:recv_body(),
                    case mypl_prov_special:delete_kommiauftrag(KommiauftragNr, Message) of
                        unknown -> send_json(Req, 410, <<"unknown Kommiauftrag">>);
                        cant_delete_already_open -> send_json(Req, 403, <<"Kommiauftrag already in processing">>);
                        ok -> send_json(Req, 204, <<"ok">>)
                    end;
                'POST' ->
                    RPath = lists:reverse(KommiauftragNr),
                    [$y,$t,$i,$r,$o,$i,$r,$p,$/|KommiauftragNrReverse] = RPath,
                    KommiauftragNrStripped = lists:reverse(KommiauftragNrReverse),
                    case mypl_prov_query:provpipeline_info(KommiauftragNrStripped) of
                        unknown -> send_json(Req, 404, <<"unknown Kommiauftrag">>);
                        _Info -> 
                            Body = Req:recv_body(),
                            {Props} = myjson:decode(Body),
                            Priority = proplists:get_value(<<"priority">>, Props, 2),
                            Explanation = proplists:get_value(<<"explanation">>, Props,
                                             <<"Prioritaet geaendert">>),
                            mypl_prov_special:update_pipeline({priority, KommiauftragNrStripped, Priority, Explanation}),
                            send_json(Req, 201, {[{priority, Priority}]})
                    end;
                _ ->
                    Req:respond({501, [], []})
            end;
        
        "kommischein" ->
            case Req:get(method) of
                Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                    Req:respond({200, [{"Content-Type", "application/json; charset=utf-8"}],
                                myjson:encode([list_to_binary(X) || X <- mypl_prov_query:provisioninglist_list()])});
                _ ->
                    Req:respond({501, [], []})
            end;
        [$k,$o,$m,$m,$i,$s,$c,$h,$e,$i,$n,$/|KommischeinNr] ->
            case Req:get(method) of
                Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                    case mypl_prov_query:provisioninglist_info2(KommischeinNr) of
                        unknown -> send_json(Req, 404, <<"unknown Kommischein">>);
                        Info -> send_json(Req, Info)
                    end;
                'POST' ->
                    case mypl_prov_query:provisioninglist_info2(KommischeinNr) of
                        unknown ->
                            send_json(Req, 404, <<"unknown Kommischein">>);
                        _Info ->
                            DoneStatus = mypl_provpipeline:commit_anything(KommischeinNr, [], []),
                            send_json(Req, 200, mypl_util:ensure_binary(DoneStatus))
                    end;
                _ ->
                    Req:respond({501, [], []})
            end;
        
        "movement" ->
            case Req:get(method) of
                Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                    Req:respond({200, [{"Content-Type", "application/json; charset=utf-8"}],
                                 myjson:encode([list_to_binary(X) || X <- mypl_db_query:movement_list()])});
                'POST' ->
                    Body = Req:recv_body(),
                    {Props} = myjson:decode(Body),
                    case mypl_movements:create_automatic_movement(mypl_util:proplist_cleanup(Props)) of
                        nothing_available ->
                            send_json(Req, 404, <<"nothing available">>);
                        MovementId ->
                            % TODO: add location header
                            send_json(Req, 201, mypl_db_query:movement_info2(MovementId))
                    end;
                _ ->
                    Req:respond({501, [], []})
            end;
        [$m,$o,$v,$e,$m,$e,$n,$t,$/|MovementId] ->
            case Req:get(method) of
                Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                    case mypl_db_query:movement_info2(MovementId) of
                        unknown -> send_json(Req, 404, <<"unknown movement">>);
                        Info -> send_json(Req, Info)
                    end;
                'POST' ->
                    % fuehrt ein Commit aus
                    case mypl_db:commit_movement(MovementId) of
                        {error, _} -> send_json(Req, 404, <<"error while committing movement">>);
                        {ok, _} -> send_json(Req, 204, <<"ok">>)
                    end;
                'DELETE' ->
                    % fuehrt ein Rollback aus
                    case mypl_db:rollback_movement(MovementId) of
                        {error, unknown} -> send_json(Req, 404, <<"unknown movement">>);
                        {ok, _} -> send_json(Req, 204, <<"ok">>)
                    end;
                _ ->
                    Req:respond({501, [], []})
            end;
        
        "pick" ->
            case Req:get(method) of
                Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                    Req:respond({200, [{"Content-Type", "application/json; charset=utf-8"}],
                                myjson:encode([list_to_binary(X) || X <- mypl_db_query:pick_list()])});
                _ ->
                    Req:respond({501, [], []})
            end;
        [$p,$i,$c,$k,$/|PickId] ->
            case Req:get(method) of
                Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                    case mypl_db_query:pick_info2(PickId) of
                        unknown -> send_json(Req, 404, <<"unknown Pick">>);
                        Info -> send_json(Req, Info)
                    end; 
                _ ->
                    Req:respond({501, [], []})
            end;
        
        "unit" ->
            case Req:get(method) of
                Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                    Req:respond({200, [{"Content-Type", "application/json; charset=utf-8"}],
                                myjson:encode([list_to_binary(X) || X <- mypl_db_query:unit_list()])});
                _ ->
                    Req:respond({501, [], []})
            end;
        [$u,$n,$i,$t,$/|UnitId] ->
            case Req:get(method) of
                Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                    case mypl_db_query:unit_info2(UnitId) of
                        unknown -> send_json(Req, 404, <<"unknown Unit">>);
                        Info -> send_json(Req, Info)
                    end;
                'POST' ->
                    case mypl_db_query:unit_info2(UnitId) of
                        unknown -> send_json(Req, 404, <<"unknown Unit">>);
                        _Info ->
                            Body = Req:recv_body(),
                            {Props} = myjson:decode(Body),
                            case proplists:get_value(<<"height">>, Props, undefined) of
                                undefined ->
                                    Req:respond({400, [{"Content-Type", "text/plain"}],
                                                "Parameter 'height' is missing."});
                                Height ->
                                    mypl_db:update_unit({height, UnitId, Height}),
                                    send_json(Req, 201, mypl_db_query:unit_info2(UnitId))
                            end
                    end;
                _ ->
                    Req:respond({501, [], []})
            end;
        
        "location" ->
            case Req:get(method) of
                Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                    Req:respond({200, [{"Content-Type", "application/json; charset=utf-8"}],
                                myjson:encode([list_to_binary(X) || X <- mypl_db_query:location_list()])});
                _ ->
                    Req:respond({501, [], []})
            end;
        [$l,$o,$c,$a,$t,$i,$o,$n,$/|LocationId] ->
            case Req:get(method) of
                Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                    case mypl_db_query:location_info2(LocationId) of
                        unknown -> send_json(Req, 404, <<"unknown location">>);
                        Info -> send_json(Req, Info)
                    end; 
                _ ->
                    Req:respond({501, [], []})
            end;
        
        "product" ->
            case Req:get(method) of
                Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                    Req:respond({200, [{"Content-Type", "application/json; charset=utf-8"}],
                                myjson:encode([mypl_util:ensure_binary(element(1, X)) || X <- mypl_db_query:count_products()])});
                _ ->
                    Req:respond({501, [], []})
            end;
        [$p,$r,$o,$d,$u,$c,$t,$/|Product] ->
            case Req:get(method) of
                Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                    {{FullQuantity,AvailableQuantity,PickQuantity,MovementQuantity},Muis} = mypl_db_query:count_product(Product),
                    send_json(Req, {[{artnr, mypl_util:ensure_binary(Product)},
                                     {full_quantity, FullQuantity},
                                     {available_quantity,AvailableQuantity},
                                     {pick_quantity,PickQuantity},
                                     {movement_quantity,MovementQuantity},
                                     {muis, [mypl_util:ensure_binary(X) || X <- Muis]}]});
                'POST' ->
                    Body = Req:recv_body(),
                    {Props} = myjson:decode(Body),
                    Menge = proplists:get_value(<<"menge">>, Props, 1),
                    case mypl_choose:find_provisioning_candidates(Menge, Product, {[]}) of
                        {error,not_enough} ->
                            send_json(Req, 403, <<"Nicht genug Bestand am Lager">>);
                        {error, no_fit} ->
                            send_json(Req, 404, <<"Kann zur Zeit nicht erfuellt werden">>);
                        {ok, Retrievals, Picks} ->
                            send_json(Req, 200, {[{retrievals, [mypl_util:ensure_binary(Mui) || Mui <- Retrievals]},
                                                  {picks, [{[{menge, Mng}, {mui, mypl_util:ensure_binary(Mui)}]} || {Mng, Mui} <- Picks]}]})
                    end;
                _ ->
                    Req:respond({501, [], []})
            end;
        _ ->
            Req:respond({404, [], []})
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
    Req:respond({Code, DefaultHeaders ++ Headers, [myjson:encode(Value), "\n"]}).

