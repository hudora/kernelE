%%%-------------------------------------------------------------------
%%% File:      mypl_prov_query.erl
%%% @author    Maximillian Dornseif <> []
%%% @copyright 2008 Maximillian Dornseif
%%% @doc  
%%%
%%% @end  
%%%
%%% @since 2008-02-21 by Maximillian Dornseif
%%%-------------------------------------------------------------------
-module(mypl_prov_query).
-author('Maximillian Dorneif').

-include_lib("stdlib/include/qlc.hrl").
-include("mypl.hrl").

%% API
-export([
provpipeline_list_new/0,
provpipeline_list_prepared/0,
provpipeline_list/0,
provpipeline_list_processing/0,
provpipeline_info/1,             %% Kommiauftrag
format_pipeline_record2/1,
provisioninglist_list/0,
provisioninglist_info/1,         %% Kommischein
provisioninglist_info2/1,        
format_provisioninglist_record2/1,
pipelinearticles/0
]).

%%====================================================================
%% API
%%====================================================================

%% @doc returns the unprocessed contents of provpipeline in the approximate order in which they will
%% be processed
provpipeline_list_new() ->
    [format_pipeline_record(X) || X <- mypl_prov_util:sort_provpipeline(mypl_db_util:do_trans(
                                           qlc:q([X || X <- mnesia:table(provpipeline),
                                                       X#provpipeline.status =:= new])))].

%% processing
provpipeline_list_processing() ->
    [format_pipeline_record(X) || X <- mypl_prov_util:sort_provpipeline(mypl_db_util:do_trans(
                                           qlc:q([X || X <- mnesia:table(provpipeline),
                                                       X#provpipeline.status =:= processing])))].

provpipeline_list() -> 
    [element(1, X) || X <- (provpipeline_list_new() ++ provpipeline_list_processing())].


%% 
%% this is named wrong since it DOESN't return provpipeline entries
provpipeline_list_prepared() ->
    mypl_db_util:do_trans(qlc:q([X || X <- mnesia:table(pickpipeline)])) ++
    mypl_db_util:do_trans(qlc:q([X || X <- mnesia:table(retrievalpipeline)])).
    

%% @doc return information on a provpipeline entry
provpipeline_info(CId) ->
    Fun = fun()->
        mnesia:read({provpipeline, CId})
    end,
    case mypl_db_util:transaction(Fun) of
        [PPEntry] ->
            format_pipeline_record2(PPEntry);
        _ ->
            unknown
    end.
    

%% @doc create a nice proplist representation of the provpipeline
format_pipeline_record(Record) ->
    {Record#provpipeline.id,
     [{tries, Record#provpipeline.tries},
      {provisioninglists, Record#provpipeline.provisioninglists},
      {priority, Record#provpipeline.priority},
      {shouldprocess, mypl_prov_util:shouldprocess(Record)},
      {status, Record#provpipeline.status},
      {volume, Record#provpipeline.volume},
      {weigth, Record#provpipeline.weigth}] ++ Record#provpipeline.attributes,
     Record#provpipeline.orderlines
    }.
    
%% @doc create a nice proplist representation of the provpipeline following the Kommiauftrag Protocol
format_pipeline_record2(Record) ->
    % mypl_util:proplist_cleanup_binary()
    {[{kommiauftragsnr, mypl_util:ensure_binary(Record#provpipeline.id)},
      {liefertermin, proplists:get_value(liefertermin, Record#provpipeline.attributes, [])},
      {liefertermin_ab, proplists:get_value(liefertermin_ab, Record#provpipeline.attributes, [])},
      {versandtermin, proplists:get_value(versandtermin, Record#provpipeline.attributes, [])},
      {versandtermin_ab, proplists:get_value(versandtermin_ab, Record#provpipeline.attributes, [])},
      {fixtermin, proplists:get_value(fixtermin, Record#provpipeline.attributes)},
      {gewicht, Record#provpipeline.weigth},
      {volumen, Record#provpipeline.volume},
      {land, mypl_util:ensure_binary(proplists:get_value(land, Record#provpipeline.attributes))},
      {plz, mypl_util:ensure_binary(proplists:get_value(plz, Record#provpipeline.attributes))},
      {info_kunde, mypl_util:ensure_binary(proplists:get_value(info_kunde, Record#provpipeline.attributes, []))},
      {auftragsnr, mypl_util:ensure_binary(proplists:get_value(auftragsnummer, Record#provpipeline.attributes))},
      {kundenname, mypl_util:ensure_binary(proplists:get_value(kundenname, Record#provpipeline.attributes))},
      {kundennr, mypl_util:ensure_binary(proplists:get_value(kernel_customer, Record#provpipeline.attributes))},
      %% myPL spezifische inhalte
      {tries, Record#provpipeline.tries},
      {provisioninglists, [mypl_util:ensure_binary(X) || X <- Record#provpipeline.provisioninglists]},
      {priority, Record#provpipeline.priority},
      {shouldprocess, mypl_prov_util:shouldprocess(Record)},
      {status, Record#provpipeline.status},
      {orderlines, format_pipeline_orderlines2(Record#provpipeline.orderlines)}
      ] ++ Record#provpipeline.attributes}.
      

format_pipeline_orderlines2([]) ->
    [];
format_pipeline_orderlines2([Orderline|Rest]) ->
    {Quantity, Product, Attributes} = Orderline,
    [{[{menge, Quantity},
      {artnr, mypl_util:ensure_binary(Product)},
      {auftragsposition, proplists:get_value(auftragsposition, Attributes)}
     ] ++ Attributes}
    ] ++ format_pipeline_orderlines2(Rest).

%% @doc returns a list of all (pick|retrieval)list ids.
provisioninglist_list() ->
    lists:sort(mypl_db_util:transaction(fun() -> mnesia:all_keys(provisioninglist) end)).

%% @doc get information concerning a (pick|retrieval)list
provisioninglist_info(Id) ->
    Fun = fun() ->
        case mnesia:read({provisioninglist, Id}) of
            [] -> {error, unknown_provisioninglist, {Id}};
            [Plist] -> 
                {ok, 
                 [{id ,              Plist#provisioninglist.id},
                  {type,             Plist#provisioninglist.type},
                  {provpipeline_id,  Plist#provisioninglist.provpipeline_id},
                  {destination,      Plist#provisioninglist.destination},
                  {parts,            Plist#provisioninglist.parts},
                  {attributes,       Plist#provisioninglist.attributes},
                  {status,           Plist#provisioninglist.status},
                  {created_at,       Plist#provisioninglist.created_at},
                  {provisioning_ids, [element(1, X) || X <- Plist#provisioninglist.provisionings]}
                 ]}
        end
    end,
    mypl_db_util:transaction(Fun).


%% @doc get information concerning a (pick|retrieval)list
provisioninglist_info2(Id) ->
    Fun = fun() ->
        case mnesia:read({provisioninglist, Id}) of
            [] -> unknown;
            [Plist] -> format_provisioninglist_record2(Plist)
        end
    end,
    mypl_db_util:transaction(Fun).
    

format_provisioninglist_record2(Plist) ->
    {[{id ,              mypl_util:ensure_binary(Plist#provisioninglist.id)},
      {type,             Plist#provisioninglist.type},
      {provpipeline_id,  mypl_util:ensure_binary(Plist#provisioninglist.provpipeline_id)},
      {destination,      mypl_util:ensure_binary(Plist#provisioninglist.destination)},
      {parts,            Plist#provisioninglist.parts},
      {status,           mypl_util:ensure_binary(Plist#provisioninglist.status)},
      {created_at,       mypl_util:ensure_binary(Plist#provisioninglist.created_at)}
      %{provisioning_ids, [mypl_util:ensure_binary(element(1, X)) || X <- Plist#provisioninglist.provisionings]}
     % gewicht
     % volumen
     ] ++ Plist#provisioninglist.attributes}.


piplinearticles_helper2([], Dict) -> Dict;
piplinearticles_helper2([{Quantity, Product}|Tail], Dict) ->
    % piplinearticles_helper2(Tail, dict:update_counter(Product, 1, Dict)).
    piplinearticles_helper2(Tail, dict:update(Product, fun({Count, SumQuantity}) ->
                                                          {Count + 1, SumQuantity + Quantity}
                                                       end,
                                              {1, Quantity}, Dict)).
     

% this is called which a list of orderlines
piplinearticles_helper1([], Dict) -> Dict;
piplinearticles_helper1([Orderline|Tail], Dict) ->
    piplinearticles_helper1(Tail, piplinearticles_helper2([{Quantity, Product} || {Quantity, Product, _} <- Orderline], Dict)).

%% @doc get a list of all articles in the provisioning pipeline and in how many orders they exist
pipelinearticles() ->
    Orderlines = mypl_db_util:do_trans(qlc:q([X#provpipeline.orderlines || X <- mnesia:table(provpipeline),
                                              X#provpipeline.status /= provisioned,
                                              X#provpipeline.status /= deleted,
                                              mypl_prov_util:shouldprocess(X) =:= yes])),
    ProductDict = piplinearticles_helper1(Orderlines, dict:new()),
    lists:reverse(lists:sort(lists:map(fun({A, B}) -> {B, A} end, dict:to_list(ProductDict)))).
