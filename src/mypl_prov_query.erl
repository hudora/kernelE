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
provpipeline_list_processing/0,
provpipeline_info/1,
provisioninglist_list/0,
provisioninglist_info/1,
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
    

%% 
provpipeline_list_prepared() ->
    mypl_db_util:do_trans(qlc:q([X || X <- mnesia:table(pickpipeline)])) ++
    mypl_db_util:do_trans(qlc:q([X || X <- mnesia:table(retrievalpipeline)])).
    

%% processing
provpipeline_list_processing() ->
    [format_pipeline_record(X) || X <- mypl_prov_util:sort_provpipeline(mypl_db_util:do_trans(
                                           qlc:q([X || X <- mnesia:table(provpipeline),
                                                       X#provpipeline.status =:= processing])))].
    

%% @doc return information on a provpipeline entry
provpipeline_info(CId) ->
    Fun = fun()->
        mnesia:read({provpipeline, CId})
    end,
    [PPEntry] = mypl_db_util:transaction(Fun),
    format_pipeline_record(PPEntry).
    

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
                  {provisioning_ids, [element(1, X) || X <- Plist#provisioninglist.provisionings]}
                 ]}
        end
    end,
    mypl_db_util:transaction(Fun).
    

piplinearticles_helper2([], Dict) -> Dict;
piplinearticles_helper2([Product|Tail], Dict) ->
    piplinearticles_helper2(Tail, dict:update_counter(Product, 1, Dict)).
    
piplinearticles_helper1([], Dict) -> Dict;
piplinearticles_helper1([Orderline|Tail], Dict) ->
    piplinearticles_helper1(Tail, piplinearticles_helper2([Product || {_, Product, _} <- Orderline], Dict)).
    
%% @doc get a list of all articles in the provisioning pipeline and in how many orders they exist
pipelinearticles() ->
    Orderlines = mypl_db_util:do_trans(qlc:q([X#provpipeline.orderlines || X <- mnesia:table(provpipeline),
                                              X#provpipeline.status /= provisioned])),
    ProductDict = piplinearticles_helper1(Orderlines, dict:new()),
    lists:reverse(lists:sort(lists:map(fun({A, B}) -> {B, A} end, dict:to_list(ProductDict)))).
