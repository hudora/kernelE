%%%-------------------------------------------------------------------
%%% File:      mypl_prov_special.erl
%%% @author    Maximillian Dornseif <> []
%%% @copyright 2008 Maximillian Dornseif
%%% @doc  
%%%
%%% @end  
%%%
%%% @since 2008-02-21 by Maximillian Dornseif
%%%-------------------------------------------------------------------
-module(mypl_prov_special).
-author('Maximillian Dorneif').

-include_lib("stdlib/include/qlc.hrl").
-include("mypl.hrl").

%% API
-export([
update_pipeline/1,
delete_pipeline/1,
flood_requestracker/0,
provpipeline_find_by_product/1
]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec 
%% @doc
%% @end 
%%--------------------------------------------------------------------

%% @doc change values on an existing pipeline entry
update_pipeline({priority, CId, Priority}) ->
    Fun = fun() ->
        [PPEntry] = mnesia:read({provpipeline, CId}),
        NewAttributes = [{kernel_updated_at, calendar:universal_time()}|
                         proplists:delete(priority, PPEntry#provpipeline.attributes)],
        mnesia:write(PPEntry#provpipeline{priority=Priority, attributes=NewAttributes})
    end,
    mypl_db_util:transaction(Fun),
    ok;
update_pipeline({fixtermin, CId, Fixtermin}) when is_boolean(Fixtermin) ->
    Fun = fun() ->
        [PPEntry] = mnesia:read({provpipeline, CId}),
        NewAttributes = [{kernel_updated_at, calendar:universal_time()}|
                         proplists:delete(fixtermin, PPEntry#provpipeline.attributes)],
        mnesia:write(PPEntry#provpipeline{attributes=[{fixtermin, Fixtermin}] ++ NewAttributes})
    end,
    mypl_db_util:transaction(Fun),
    ok;
update_pipeline({liefertermin, CId, Liefertermin}) ->
    Fun = fun() ->
        [PPEntry] = mnesia:read({provpipeline, CId}),
        NewAttributes = [{kernel_updated_at, calendar:universal_time()}|
                         proplists:delete(liefertermin, PPEntry#provpipeline.attributes)],
        mnesia:write(PPEntry#provpipeline{attributes=[{liefertermin, Liefertermin}] ++ NewAttributes})
    end,
    mypl_db_util:transaction(Fun),
    ok;
update_pipeline({versandtermin, CId, Versandtermin}) ->
    Fun = fun() ->
        [PPEntry] = mnesia:read({provpipeline, CId}),
        NewAttributes = [{kernel_updated_at, calendar:universal_time()}|
                         proplists:delete(versandtermin, PPEntry#provpipeline.attributes)],
        mnesia:write(PPEntry#provpipeline{attributes=[{versandtermin, Versandtermin}|NewAttributes]})
    end,
    mypl_db_util:transaction(Fun),
    ok.
    



%% @spec delete_pipeline(CId::string()) -> ok|error
%% @doc removes an unprocessed order from the provisioningpipeline
%%
%% Returns `aborted' if the order can't be removed because it is currently processed.
%% Returns `ok' if the order has been successfully removed
%% @TODO: fixme
delete_pipeline(CId) ->
    Fun = fun() ->
        [Entry] = mnesia:read({provpipeline, CId}),
        if 
            Entry#provpipeline.status /= new ->
                % we can't reinsert something which is not new
                {error, cant_delete_already_open, {CId, Entry}};
            true ->
                mypl_audit:archive(Entry, delete),
                mnesia:delete({provpipeline, CId}),
                ok
        end
    end,
    mypl_db_util:transaction(Fun).
    



% @doc ensure that the requestracker is informed about the products we need
flood_requestracker() ->
    Candidates = [X || X <- mypl_db_util:transaction(fun() -> 
                                                   mnesia:match_object(#provpipeline{status = new, _ = '_'})
                                               end),
                                          mypl_prov_util:shouldprocess(X) /= no],
    flood_requestracker(Candidates).

flood_requestracker([]) -> ok;
flood_requestracker([Entry|CandidatesTail]) ->
    Orderlines = [{Quantity, Product} || {Quantity, Product, _Attributes} <- Entry#provpipeline.orderlines],
    mypl_provisioning:find_provisioning_candidates_multi(Orderlines, mypl_prov_util:sort_provpipeline_helper(Entry)),
    flood_requestracker(CandidatesTail).
    

provpipeline_find_by_product({Quantity, Product}) ->
    Candidates = [X || X <- mypl_db_util:transaction(fun() ->
                                                   mnesia:match_object(#provpipeline{status = new, _ = '_'})
                                               end),
                       mypl_prov_util:shouldprocess(X) /= no,
                       length(X#provpipeline.orderlines) =:= 1,
                       orderline_matching_helper(Quantity, Product, X#provpipeline.orderlines) =:= true],
    Candidates.
    
orderline_matching_helper(Quantity, Product, Orderlines) ->
    case Orderlines of
        [{Quantity, Product, _}] ->
            true;
        _ ->
            false
    end.
    
    
