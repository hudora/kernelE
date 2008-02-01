
%% @version 0.2
%%% File    : mypl_provpipeline
%%% Author  : Maximillian Dornseif
%%% Created :  Created by Maximillian Dornseif on 2007-11-07.
-module(mypl_provpipeline).
-define(SERVER, mypl_provpipeline).

-include_lib("stdlib/include/qlc.hrl").
-include("mypl.hrl").

%% API
-export([insert_pipeline/1, provpipeline_list_new/0, provpipeline_list_processing/0, delete_pipeline/1,
         update_pipeline/1,
         get_picklists/0, get_retrievallists/0, get_movementlist/0,
         commit_picklist/1, commit_retrievallist/1, % commit_movementlist/1,
         flood_requestracker/0,
         is_provisioned/1, run_me_once/0, pipelinearticles/0]).

run_me_once() ->
    % komissionierbelege, so wie sie aus SoftM kommen
    mnesia:create_table(provpipeline, [{disc_copies, [node()]},
                                       {attributes, record_info(fields, provpipeline)}
                                      ]),
    mnesia:create_table(provpipeline_processing, [{disc_copies, [node()]},
                                       {attributes, record_info(fields, provpipeline_processing)}
                                      ]),
    % retrievallists und picklists, wie sie an nach aussen gegeben wurden
    mnesia:create_table(provisioninglist, [{disc_copies, [node()]},
                                       {attributes, record_info(fields, provisioninglist)}
                                      ]),
    % puffer, fuer picks, die noch ausgegeben werden muessen
    mnesia:create_table(pickpipeline, [{disc_copies, [node()]},
                                       {attributes, record_info(fields, pickpipeline)}
                                      ]),
    % puffer, fuer retrievals, die noch ausgegeben werden muessen
    mnesia:create_table(retrievalpipeline, [{disc_copies, [node()]},
                                       {attributes, record_info(fields, retrievalpipeline)}
                                      ]).



%% @spec insert_pipeline({string(), Orderlines, integer(), string(), integer(), float(), Attributes}) -> ok
%%           Orderlines = [{Quanity::integer(), Product::string(), Attributes}]
%%           Attributes = [{name, value}]
%% @doc adds an order to the provisioningpipeline.
%%
%% `CId' is a unique Id used by the client to refer to this Picking order, e.g. the "Lieferscheinnummer" 
%% or something similar. `Orderlines' is a list of Articles to 
%% provision. The List elements are tuples `{Quanity, Product, Attributes}' where Attributes contains
%% arbitrary data for use at tha client side.
%% The higher the `priority' the more likely it is, that the Order is processed early.
%% In addition we consider the attributes `versandtermin' and `liefertermin' to determine processing order.
%% 'Customer' is to aggregate shippments to the same customer. 'Weigth' and 'Volume' are the calculated.
%% See {@link sort_provpipeline/1} for details.
%% total Weigth and Volume of the shippment and are used to make scheduling descisions.
%%
%% Example:
%% ```insert_pipeline(Id, [{20, 10106, [{"auftragsposition", "1"}, {"gewicht", "34567"}]},
%%                         {70, 14650, [{"auftragsposition", "2"}, {"gewicht", "35667"}]},
%%                         {30, 76500, [{"auftragsposition", "3"}, {"gewicht", "12367"}]}],
%%                    28, "34566", 345000, 581.34,
%%                    [{"auftragsnumer", "123432"}, {"liefertermin", "2007-12-23"}]).'''
insert_pipeline({CId, Orderlines, Priority, Customer, Weigth, Volume, Attributes}) ->
    insert_pipeline([CId, Orderlines, Priority, Customer, Weigth, Volume, Attributes]);
insert_pipeline([CId, Orderlines, Priority, Customer, Weigth, Volume, Attributes]) ->
    Fun = fun() ->
        case mnesia:read({provpipeline, CId}) of
            % ensure we don't update an entry which already wxists and is NOT new
            [ExistingEntry] ->
                if 
                    ExistingEntry#provpipeline.status /= new ->
                        % we can't reinsert something which is not new
                        {error, cant_reinsert_already_open, {CId, ExistingEntry}};
                    true ->
                        insert_pipeline_helper(CId, Orderlines, Priority, Customer, Weigth, Volume,
                                               [{kernel_updated_at, calendar:universal_time()}|Attributes])
                end;
            [] ->
                insert_pipeline_helper(CId, Orderlines, Priority, Customer, Weigth, Volume,
                                       [{kernel_enqueued_at, calendar:universal_time()}|Attributes])
        end
    end,
    mypl_db_util:transaction(Fun).
    
%% @private
insert_pipeline_helper(CId, Orderlines, Priority, Customer, Weigth, Volume, Attributes) ->
    PPline = #provpipeline{id=CId, priority=Priority, weigth=Weigth, volume=Volume,
                           attributes=[{kernel_customer, Customer}
                                      ] ++ proplistlist_to_proplisttuple(Attributes),
                           provisioninglists=[], tries=0, status=new,
                           % normalize on tuples instead of lists
                           % TODO: normalize attributes to tuple
                           orderlines=lists:map(fun({Quantity, Product, OlAttributes}) -> 
                                                        {Quantity, Product, OlAttributes};
                                                    ([Quantity, Product, OlAttributes]) -> 
                                                        {Quantity, Product, OlAttributes} end, Orderlines)},
    mnesia:write(PPline),
    [mypl_requesttracker:in(Quantity, Product) || {Quantity, Product, _} <- PPline#provpipeline.orderlines],
    ok.
    

%% @doc change values on an existing pipeline entry
update_pipeline({priority, CId, Priority}) ->
    Fun = fun() ->
        [PPEntry] = mnesia:read({provpipeline, CId}),
        NewAttributes = [{kernel_updated_at, calendar:universal_time()}|
                         proplists:delete(versandtermin, PPEntry#provpipeline.attributes)],
        mnesia:write(PPEntry#provpipeline{priority=Priority, attributes=NewAttributes})
    end,
    mypl_db_util:transaction(Fun),
    ok;
update_pipeline({versandtermin, CId, Versandtermin}) ->
    Fun = fun() ->
        [PPEntry] = mnesia:read({provpipeline, CId}),
        NewAttributes = [{kernel_updated_at, calendar:universal_time()}|
                         proplists:delete(versandtermin, PPEntry#provpipeline.attributes)],
        mnesia:write(PPEntry#provpipeline{attributes=[{versandtermin, Versandtermin}|NewAttributes]}),
        erlang:display({CId, mnesia:read({provpipeline, CId})})
    end,
    mypl_db_util:transaction(Fun),
    ok.
    

%% @doc converts
%% [{tries,0},
%%  {kernel_customer,"14529"},
%%  ["auftragsnummer",647105],
%%  ["liefertermin","2007-12-03"]]
%% to
%% [{tries,0},
%%  {kernel_customer,"14529"},
%%  {"auftragsnummer",647105},
%%  {"liefertermin","2007-12-03"}]
%% mainly for fixing data gotten via json
proplistlist_to_proplisttuple(L) ->
    lists:map(fun({Name, Value}) -> 
                  {Name, Value};
                 ([Name, Value]) when is_atom(Name) -> 
                  {Name, Value};
                 ([Name, Value]) when is_list(Name) -> 
                  {erlang:list_to_atom(Name), Value} end, L).
    

%% @doc returns the unprocessed contents of provpipeline in the approximate order in which they will be processed
provpipeline_list_new() ->
    [format_pipeline_record(X) || X <- sort_provpipeline(mypl_db_util:do_trans(
                                           qlc:q([X || X <- mnesia:table(provpipeline),
                                                       X#provpipeline.status =:= new])))].
    

%% 
provpipeline_list_prepared() ->
    mypl_db_util:do_trans(qlc:q([X || X <- mnesia:table(pickpipeline)])) ++
    mypl_db_util:do_trans(qlc:q([X || X <- mnesia:table(retrievalpipeline)])).
    

%% processing
provpipeline_list_processing() ->
    [format_pipeline_record(X) || X <- sort_provpipeline(mypl_db_util:do_trans(
                                           qlc:q([X || X <- mnesia:table(provpipeline),
                                                       X#provpipeline.status =:= processing])))].
    

format_pipeline_record(Record) ->
    {Record#provpipeline.id,
     [{tries, Record#provpipeline.tries},
      {provisioninglists, Record#provpipeline.provisioninglists},
      {priority, Record#provpipeline.priority}] ++ Record#provpipeline.attributes,
     Record#provpipeline.orderlines
    }.
    

%% @TODO: removeme 
provpipeline_processing_list_all() ->
    mypl_db_util:do_trans(qlc:q([X || X <- mnesia:table(provpipeline_processing)])).
    

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
    

%% @spec get_picklists() -> PicklistList|nothing_available
%%      PicklistList = [{Id::string(), CId::string(), Destination::string(), Attributes, Parts::integer(),
%%                      [{LineId::string(), Mui::string(), Location::string(),
%%                        Quantity::integer(), Product::string(), Attributes}]}]
%%      Attributes = [{name, value}]
%%
%% @doc gets the next Picklist to be processed.
%%
%% If there is noting to pick at the moment it returns `nothing_available'.
%% Else it returns a List of Picklist Tuples. These Tuples each represent a "Kommissionierbeleg" and
%% consist of an Id to be used in {@link commit_picklist/1}, the CId which was
%% used in the call to {@link insert_pipeline/7}, a Destination, where the Picked gods should be dropped of,
%% a list of arrtributes provided to {@link add/7}. Parts indicates in how many parts this Order is divided.
%% So far only the values 1 for Pick only and 2 for Pick and Retrieval are used.
%%
%% The Picklist ends with a list of "Orderlines" consisting of the Location where to get the goods, a
%% Quantity on how much to Pick and a String Representing the Produkt ID (SKU).
get_picklists() ->
    % check if we have picks available
    case mypl_db_util:transaction(fun() -> mnesia:first(provpipeline) end) of
        '$end_of_table' ->
            no_more_provisionings_requested;
        _ ->
            Fun = fun() ->
                case choose_next_pick() of
                    nothing_available ->
                        nothing_available;
                    {ok, P} ->
                        Id = "p" ++ P#pickpipeline.id,
                        [PPEntry] = mnesia:read({provpipeline, P#pickpipeline.provpipelineid}),
                        % Volume for the Picklist
                        Positions=lists:map(fun(PickId) ->
                                                {ok, PickInfo} = mypl_db_query:pick_info(PickId),
                                                  {proplists:get_value(quantity, PickInfo),
                                                  proplists:get_value(product, PickInfo)}
                                             end, P#pickpipeline.pickids),
                        VolumeAttributes = mypl_volumes:volume_proplist(Positions),
                        
                        Picklist = #provisioninglist{id=Id, type=picklist,
                                        provpipeline_id=PPEntry#provpipeline.id,
                                        destination="AUSLAG",
                                        attributes=VolumeAttributes ++ PPEntry#provpipeline.attributes,
                                        parts=1 + lists:min([length(P#pickpipeline.retrievalids),1]),
                                        provisionings=lists:map(fun(PickId) ->
                                            {ok, PickInfo} = mypl_db_query:pick_info(PickId),
                                            FromLocation = mypl_db_util:get_mui_location(proplists:get_value(from_unit, PickInfo)),
                                            % Volume for the Posistion
                                            mypl_volumes:volume_proplist([{proplists:get_value(quantity, PickInfo),
                                                                           proplists:get_value(product, PickInfo)}]),
                                            {PickId,
                                             proplists:get_value(from_unit, PickInfo),
                                             FromLocation#location.name,
                                             proplists:get_value(quantity, PickInfo),
                                             proplists:get_value(product, PickInfo),
                                             []}
                                            end, P#pickpipeline.pickids)},
                        mnesia:write(Picklist),
                        mnesia:write(#provpipeline_processing{id=Id, provpipelineid=PPEntry#provpipeline.id,
                                                              pickids=P#pickpipeline.pickids, retrievalids=[]}),
                        mnesia:write(PPEntry#provpipeline{provisioninglists=[Picklist#provisioninglist.id|PPEntry#provpipeline.provisioninglists]}),
                        % fixme: this has to return a list of provisioninglists - has to be fixed in python too
                        [format_provisioninglist(Picklist)]
                end
            end,
            mypl_db_util:transaction(Fun)
    end.


%% @doc formats a picklist entry according to the return value of get_picklists/0 and get_retrievallists/0
%% @see get_picklists/0
format_provisioninglist(PList) ->
    {PList#provisioninglist.id,
     PList#provisioninglist.provpipeline_id,
     PList#provisioninglist.destination,
     PList#provisioninglist.attributes,
     PList#provisioninglist.parts,
     PList#provisioninglist.provisionings}.
    

%% @doc this returns the same as get_picklist but uses retrievals, not picks.
%% @see get_picklists/0
get_retrievallists() ->
    % check if we have picks available
    case mypl_db_util:transaction(fun() -> mnesia:first(provpipeline) end) of
        '$end_of_table' ->
            nothing_available;
        _ ->
            Fun = fun() ->
                case choose_next_retrieval() of
                    nothing_available ->
                        nothing_available;
                    {ok, R} ->
                        Id = "r" ++ R#retrievalpipeline.id,
                        [PPEntry] = mnesia:read({provpipeline, R#retrievalpipeline.provpipelineid}),
                        % Volume for the Picklist
                        Positions=lists:map(fun(RetrievalId) ->
                                                {ok, MovementInfo} = mypl_db_query:movement_info(RetrievalId),
                                                  {proplists:get_value(quantity, MovementInfo),
                                                  proplists:get_value(product, MovementInfo)}
                                             end, R#retrievalpipeline.retrievalids),
                        VolumeAttributes = mypl_volumes:volume_proplist(Positions),
                        
                        Retrievallist = #provisioninglist{id=Id, type=retrievallist,
                                            provpipeline_id=PPEntry#provpipeline.id,
                                            destination="AUSLAG",
                                            attributes=VolumeAttributes ++ PPEntry#provpipeline.attributes,
                                            parts=1 + lists:min([length(R#retrievalpipeline.pickids),1]),
                                            provisionings=lists:map(fun(RetrievalId) ->
                                                {ok, MovementInfo} = mypl_db_query:movement_info(RetrievalId),
                                                FromLocation = mypl_db_util:read_location(proplists:get_value(from_location, MovementInfo)),
                                                {RetrievalId,
                                                 proplists:get_value(mui, MovementInfo),
                                                 FromLocation#location.name,
                                                 proplists:get_value(quantity, MovementInfo),
                                                 proplists:get_value(product, MovementInfo),
                                                 mypl_volumes:volume_proplist([{proplists:get_value(quantity, MovementInfo),
                                                                                proplists:get_value(product, MovementInfo)}])
                                                 }
                                                 end, R#retrievalpipeline.retrievalids)},
                        mnesia:write(Retrievallist),
                        mnesia:write(#provpipeline_processing{id=Id, provpipelineid=PPEntry#provpipeline.id,
                                                              pickids=[], retrievalids=R#retrievalpipeline.retrievalids}),
                        mnesia:write(PPEntry#provpipeline{provisioninglists=[Retrievallist#provisioninglist.id|PPEntry#provpipeline.provisioninglists]}),
                        % fixme: this has to return a list of provisioninglists - has to be fixed in python too
                        [format_provisioninglist(Retrievallist)]
                end
            end,
            mypl_db_util:transaction(Fun)
    end.


% @private
% 
choose_next_pick() ->
    % refill pipeline if that is needed
    case mypl_db_util:transaction(fun() -> mnesia:first(pickpipeline) end) of
        '$end_of_table' ->
            refill_pickpipeline();
        _ ->
            ok
    end,
    % now return first entry, unless pipeline is still empty
    case mypl_db_util:transaction(fun() -> mnesia:first(pickpipeline) end) of
        '$end_of_table' ->
            nothing_available;
        PipelineId ->
            Fun = fun() ->
                % get entry from DB
                [PipelineEntry] = mnesia:read({pickpipeline, PipelineId}),
                % remove entry from the pipeline
                mnesia:delete({pickpipeline, PipelineId}),
                % return pickids
                {ok, PipelineEntry}
            end,
            mypl_db_util:transaction(Fun)
    end.
    
    
choose_next_retrieval() ->
    % refill pipeline if that is needed
    case mypl_db_util:transaction(fun() -> mnesia:first(retrievalpipeline) end) of
        '$end_of_table' ->
            refill_retrievalpipeline();
        _ ->
            ok
    end,
    % now return first entry, unless pipeline is still empty
    case mypl_db_util:transaction(fun() -> mnesia:first(retrievalpipeline) end) of
        '$end_of_table' ->
            nothing_available;
        PipelineId ->
            Fun = fun() ->
                % get entry from DB
                [PipelineEntry] = mnesia:read({retrievalpipeline, PipelineId}),
                % remove entry from the pipeline
                mnesia:delete({retrievalpipeline, PipelineId}),
                % return pickids
                {ok, PipelineEntry}
            end,
            mypl_db_util:transaction(Fun)
    end.
    
    
%% @doc adds entries to the pickpipeline
refill_pickpipeline() -> refill_pipeline(picks).
%% @doc adds entries to the pickpipeline
refill_retrievalpipeline() -> refill_pipeline(retrievals).
    

refill_pipeline(Type) ->
    % check provisinings until we find one which would generate picks
    Candidates = mypl_db_util:do_trans(qlc:q([X || X <- mnesia:table(provpipeline),
                                                   X#provpipeline.status =:= new])),
    refill_pipeline(Type, sort_provpipeline(Candidates)).
    

refill_pipeline(_Type, []) -> no_fit;
refill_pipeline(Type, Candidates) ->
    [Entry|CandidatesTail] = Candidates,
    Orderlines = [{Quantity, Product} || {Quantity, Product, _Attributes} <- Entry#provpipeline.orderlines],
    case mypl_provisioning:find_provisioning_candidates_multi(Orderlines, sort_provpipeline_helper(Entry)) of
        {error, no_fit} ->
            % update number of tries
            mypl_db_util:transaction(fun() ->
                                        mnesia:write(Entry#provpipeline{tries=Entry#provpipeline.tries+1})
                                     end),
            % retry with the next candidate
            refill_pipeline(Type, CandidatesTail);
        {ok, Retrievals, Picks} ->
            if
                ((Type =:= picks) and (length(Picks) > 0)) 
                or
                ((Type =:= retrievals) and (length(Retrievals) > 0)) 
                ->
                    % we have got a match - add to the two queues, remove from pipeline and we are done here
                    {ok, RetrievalIds, PickIds} = mypl_provisioning:init_provisionings_multi(Orderlines,
                                                         [{provpipeline_id, Entry#provpipeline.id},
                                                          {allocated_at, calendar:universal_time()}],
                                                          sort_provpipeline_helper(Entry)),
                    Fun = fun() ->
                        case RetrievalIds of
                            [] -> ignore;
                            _ -> mnesia:write(#retrievalpipeline{id=mypl_util:serial(), 
                                                                 provpipelineid=Entry#provpipeline.id,
                                                                 retrievalids=RetrievalIds, pickids=PickIds})
                        end,
                        case PickIds of
                            [] -> ignore;
                            _ -> mnesia:write(#pickpipeline{id=mypl_util:serial(),
                                                            provpipelineid=Entry#provpipeline.id,
                                                            pickids=PickIds, retrievalids=RetrievalIds})
                        end,
                        % mark the order in the pipeline as  beeing processed
                        mnesia:write(Entry#provpipeline{status=processing}),
                        ok
                    end,
                    mypl_db_util:transaction(Fun);
                true ->
                    % nochmal versuchen
                    refill_pipeline(Type, CandidatesTail)
            end
    end.
    

% @doc ensure that the requestracker is informed about the products we need
flood_requestracker() ->
    Candidates = mypl_db_util:do_trans(qlc:q([X || X <- mnesia:table(provpipeline),
                                                   X#provpipeline.status =:= new])),
    flood_requestracker(Candidates).

flood_requestracker([]) -> ok;
flood_requestracker([Entry|CandidatesTail]) ->
    Orderlines = [{Quantity, Product} || {Quantity, Product, _Attributes} <- Entry#provpipeline.orderlines],
    mypl_provisioning:find_provisioning_candidates_multi(Orderlines, sort_provpipeline_helper(Entry)),
    flood_requestracker(CandidatesTail).
    

% @doc sort provpipeline records in order which they should be handled
%
% sorts by the attributes `versandtermin', `liefertermin' the priority cusotmer ID
%% and the number of unsuccessfull tries
sort_provpipeline(Records) ->
    lists:sort(fun(A, B) -> sort_provpipeline_helper(A) < sort_provpipeline_helper(B) end, Records).

% @private
% creates a key for sorting
sort_provpipeline_helper(Record) ->
    % proplistlist_to_proplisttuple() is to fix broken legavy data
    {% concat versandtermin and liefertermin so if no versandtermin is set we sort by liefertermin
     proplists:get_value(versandtermin, proplistlist_to_proplisttuple(Record#provpipeline.attributes), "") ++
     proplists:get_value(liefertermin,  proplistlist_to_proplisttuple(Record#provpipeline.attributes), ""), 
     100 - Record#provpipeline.priority, % higher priorities mean lower values mean beeing sorted first
     proplists:get_value(kernel_customer, Record#provpipeline.attributes, "99999"),
     100 - Record#provpipeline.tries % higer tries mean lower values mean being sorted first
    }.
    

%% @spec get_movementlist() -> MovementlistList|nothing_available
%%      MovementlistList = [{Id::string(), CId::string(), Destination::string(), Attributes, Parts::integer(),
%%                          [{LineId::string(), Location::string(), Product::string()}]}]
%% @see get_picks/0
%% @doc gets the next Movements/Retrievals to be processed.
%%
%% This function is very simmilar to {@link get_picks/1} but returns a List of Retrievals. Occasionally the
%% System decides to prefer that a internal Movement is done to optimize the Warehouse instead of a Retrieval
%% for actually fullfilling an order. In such cases the `CId == ""'.
%% @TODO: fixme
get_movementlist() ->
    mypl_movements:create_automatic_movements().
    
%% spec commit_movements(Id::string()) -> ok
%% TODO: fixme
%commit_movementlist(Id) -> ok.


commit_picklist(Id) ->
    commit_picklist(Id, [], []).

%% @spec commit_picklist(Id::string(), Attributes, [{LineId::string(), Quantity::integer()}]) -> provisioned|unfinished
%%      Attributes = [{name, value}]
%% @doc commit a Picklist you got from get_picks/0
%% 
%% Attributes can be used for later statistics. It is suggested you add at least something like
%% `[{"picker", "biondo"}]'.
%% 
%% Returns `provisioned' if the CId is finished (no additional Picks or Movements).
%% @TODO:  save attributes
commit_picklist(Id, Attributes, Lines) ->
    commit_anything(Id, Attributes, Lines).

commit_retrievallist(Id) -> commit_retrievallist(Id, [], []).
%% @spec commit_retrievallist(Id::string(), Attributes, [{LineId::string(), Quantity::integer()}]) -> provisioned|unfinished
%%      Attributes = [{name, value}]
%% @doc commit a Picklist you got from get_picks/0
%% @TODO: fixme
commit_retrievallist(Id, Attributes, Lines) ->
    commit_anything(Id, Attributes, Lines).

commit_anything(Id, _Attributes, _Lines) ->
    Fun = fun() ->
        [Processing] = mnesia:read({provpipeline_processing, Id}),
        lists:map(fun(PId) ->
                      {ok, _} = mypl_db:commit_pick(PId)
                  end,
                  Processing#provpipeline_processing.pickids),
        lists:map(fun(RId) ->
                      {ok, _} = mypl_db:commit_retrieval(RId)
                  end,
                  Processing#provpipeline_processing.retrievalids),
        mnesia:delete({provpipeline_processing, Id}),
        
        % find out if finished or if there are other picks/retrievals for this order
        case mypl_db_util:do_trans(qlc:q([X || X <- mnesia:table(provpipeline_processing),
                                               X#provpipeline_processing.provpipelineid 
                                               =:= Processing#provpipeline_processing.provpipelineid])) of
            [_Foo] ->
                Ret = unfinished;
            [] ->
                Ret = provisioned,
                % mark in provpipeline as done
                [PPEntry] = mnesia:read({provpipeline, Processing#provpipeline_processing.provpipelineid}),
                mnesia:write(PPEntry#provpipeline{status=provisioned});
            X ->
                Ret = unfinished,
                error_logger:error_msg("Unexpected provpipeline_processing content in regard to ~w|~w",
                                       [Processing, X])
        end,
        Ret
    end,
    mypl_db_util:transaction(Fun).


%% @spec is_provisioned(CId) -> provisioned|unfinished
%% @doc check if a Order is fully procesed and can be marked as "Delivered" in the ERP.
is_provisioned(CId) ->
    [PPEntry] = mypl_db_util:transaction(fun() -> mnesia:read({provpipeline, CId}) end),
    case PPEntry#provpipeline.status of
        provisioned ->
            provisioned;
        _ ->
            unfinished
    end.
    

%% @spec mark_as_finished(CId) -> ok|unfinished
%% 
%% @doc marks an order as finished and processed indicationg that kernelE kan remove all state on this order.
%% 
%% Returns ok if successfull or unknown if this order is already marked as finished or was never known.
mark_as_finished(CId) -> 
    case is_provisioned(CId) of
        unfinished ->
            unfinished;
        provisioned ->
            % remove from the provpipeline
            Fun = fun() ->
                [PPEntry] = mnesia:read({provpipeline, CId}),
                mypl_audit:archive(PPEntry, provpipeline_finished),
                mnesia:delete({provpipeline, CId})
            end,
            mypl_db_util:transaction(Fun),
            ok
    end.
    

piplinearticles_helper2([], Dict) -> Dict;
piplinearticles_helper2([Product|Tail], Dict) ->
    piplinearticles_helper2(Tail, dict:update_counter(Product, 1, Dict)).
    
piplinearticles_helper1([], Dict) -> Dict;
piplinearticles_helper1([Orderline|Tail], Dict) ->
    piplinearticles_helper1(Tail, piplinearticles_helper2([Product || {_, Product, _} <- Orderline], Dict)).
    
%% @doc get a list of all articles in the provisioning pipeline
pipelinearticles() ->
    Orderlines = mypl_db_util:do_trans(qlc:q([X#provpipeline.orderlines || X <- mnesia:table(provpipeline),
                                              X#provpipeline.status /= provisioned])),
    ProductDict = piplinearticles_helper1(Orderlines, dict:new()),
    lists:reverse(lists:sort(lists:map(fun({A, B}) -> {B, A} end, dict:to_list(ProductDict)))).
    

% ~~ Unit tests
-ifdef(EUNIT).
-compile(export_all).

%%% @hidden
test_init() ->
    mypl_db:run_me_once(),
    % flush database
    mnesia:start(),
    mnesia:clear_table(unit),
    mnesia:clear_table(location),
    mnesia:clear_table(movement),
    mnesia:clear_table(pick),
    mnesia:clear_table(picklist),
    mnesia:clear_table(articleaudit),
    mnesia:clear_table(unitaudit),
    mnesia:clear_table(provpipeline),
    mnesia:clear_table(pickpipeline),
    mnesia:clear_table(provpipeline_processing),
    mnesia:clear_table(provisioninglist),
    mnesia:clear_table(retrievalpipeline),
    % regenerate locations
    % init_location(Name, Height, Floorlevel, Preference, Attributes)
    mypl_db:init_location("EINLAG", 6000, true,  0, [{no_picks}]),
    mypl_db:init_location("AUSLAG", 6000, true,  0, [{no_picks}]),
    mypl_db:init_location("010101", 2000, true,  6, []),
    mypl_db:init_location("010102", 1950, false, 6, []),
    mypl_db:init_location("010103", 1200, false, 5, []),
    mypl_db:init_location("010201", 2000, true,  7, []),
    mypl_db:init_location("010202", 2000, false,  7, []),
    mypl_db:init_location("010203", 2000, false,  7, []),
    mypl_db:init_location("010301", 2000, true,  7, []),
    mypl_db:init_location("010302", 2000, false,  7, []),
    mypl_db:init_location("010303", 2000, false,  7, []),
    
    {ok, _} = mypl_db:store_at_location("EINLAG", "mui1",  5, "a0003", 1200),
    {ok, _} = mypl_db:store_at_location("010101", "mui2",  5, "a0003", 1200),
    {ok, _} = mypl_db:store_at_location("EINLAG", "mui3", 17, "a0004", 1200),
    {ok, _} = mypl_db:store_at_location("010201", "mui4", 19, "a0004", 1200),
    {ok, _} = mypl_db:store_at_location("010301", "mui5", 61, "a0005", 1200),
    {ok, _} = mypl_db:store_at_location("010302", "mui6", 10, "a0005", 1200),
    [{"a0003",10,10,0,0},{"a0004",36,36,0,0},{"a0005",71,71,0,0}] = mypl_db_query:count_products(),
    % provpipeline empty?
    [] = provpipeline_list_new(),
    [] = provpipeline_list_prepared(),
    
    % fill it up!
    insert_pipeline([lieferschein1, [{10, "a0005", []}, {1,  "a0004", []}], 3, "kunde01", 0, 0,
        [{liefertermin, "2007-12-15"}, {versandtermin, "2007-12-14"}]]),
    insert_pipeline([lieferschein2, [{10, "a0005", []}, {1,  "a0004", []}], 3, "kunde02", 0, 0,
        [{liefertermin, "2007-12-13"}, {versandtermin, "2007-12-16"}]]),
    
    insert_pipeline([lieferschein3, [{50, "a0005", []}, {16, "a0004", []}], 4, "kunde02", 0, 0,
        [{liefertermin, "2007-12-13"}, {versandtermin, "2007-12-16"}]]),
    
    insert_pipeline([lieferschein4, [{1,  "a0005", []}, {1,  "a0004", []}], 1, "kunde03", 0, 0,
        [{liefertermin, "2007-12-10"}]]),
    
    ok.
    

%%% @hidden
provpipeline_list_test() ->
    test_init(),
    [] = provpipeline_list_prepared(),
    % lieferschein4 is first because it is the oldest
    [{lieferschein4,_,[{1,"a0005",[]},{1,"a0004",[]}]},
     % lieferschein1 is second because it is the second oldest
     {lieferschein1,_,[{10,"a0005",[]},{ 1,"a0004",[]}]},
     % lieferschein3 is as old as lieferschein2 but has a higher priority
     {lieferschein3,_,[{50,"a0005",[]},{16,"a0004",[]}]},
     % lieferschein2 is the youngest with the owest priority
     {lieferschein2,_,[{10,"a0005",[]},{ 1,"a0004",[]}]}] = provpipeline_list_new(),
    ok.
        

mypl_simple_test() ->
    test_init(),
    P4 = get_picklists(),
    [{P4id,lieferschein4,"AUSLAG",_,1,[{_,"mui4","010201",1,"a0004",[]},
                                       {_,"mui5","010301",1,"a0005",[]}]}] = P4,
    P1 = get_picklists(),
    [{P1id,lieferschein1,"AUSLAG",_,2,[{"P00000249","mui4","010201",1,"a0004",[]}]}] = P1,
    P3 = get_picklists(),
    [{P3id,lieferschein3,"AUSLAG",_,1,[{_,"mui4","010201",16,"a0004",[]},
                                       {_,"mui5","010301",50,"a0005",[]}]}] = P3,
    P2 = get_picklists(),
    [{P2id,lieferschein2,"AUSLAG",_,1,[{_,"mui4","010201",1,"a0004",[]},
                                       {_,"mui5","010301",10,"a0005",[]}]}] = P2,
    
    % at this time we should have one retrieval prepared
    ?assertMatch([{retrievalpipeline,_,lieferschein1,_,_}], provpipeline_list_prepared()),
    
    R1 = get_retrievallists(),
    [{R1id,lieferschein1,"AUSLAG",_,2,[{_,"mui6","010302",10,"a0005",_}]}] = R1,
    
    [Id1,_Id2,_Id3,_Id4,_Id5] = provisioninglist_list(),
    ?assertMatch({ok, _}, provisioninglist_info(Id1)),
    
    % TODO: test
    % update_pipeline({versandtermin, lieferschein1, "2007-10-01"}),
    
    % provpipeline should be empty now
    ?assertMatch([], provpipeline_list_prepared()),
    ?assertMatch([], provpipeline_list_new()),
    ?assertMatch([{lieferschein4,_,[{1,"a0005",[]},{1,"a0004",[]}]},
                  {lieferschein1,_,[{10,"a0005",[]},{1,"a0004",[]}]},
                  {lieferschein3,_,[{50,"a0005",[]},{16,"a0004",[]}]},
                  {lieferschein2,_,[{10,"a0005",[]},{1,"a0004",[]}]}],
                 provpipeline_list_processing()),
    % checks that goods are reserved for picking and retrieval
    ?assertMatch([{"a0003",10,10,0,0},{"a0004",36,17,19,0},{"a0005",71,0,61,10}],
                 mypl_db_query:count_products()),
    
    ?assertMatch(unfinished, is_provisioned(lieferschein1)),
    ?assertMatch(unfinished, commit_retrievallist(R1id)),
    ?assertMatch(unfinished, is_provisioned(lieferschein1)),
    ?assertMatch(provisioned, commit_picklist(P1id)),
    ?assertMatch(provisioned, is_provisioned(lieferschein1)),
    ?assertMatch(provisioned, commit_picklist(P2id)),
    ?assertMatch(provisioned, commit_picklist(P3id)),
    ?assertMatch(provisioned, commit_picklist(P4id)),
    mark_as_finished(lieferschein1),
    mark_as_finished(lieferschein2),
    mark_as_finished(lieferschein3),
    mark_as_finished(lieferschein4),
    
    % check that goods are removed from warehouse now
    ?assertMatch([{"a0003",10,10,0,0},{"a0004",17,17,0,0}], mypl_db_query:count_products()),
    ok.
    

reinsert_test() ->
    test_init(),
    ok = insert_pipeline([lieferschein4, [{1,  "a0005", []}, {1,  "a0004", []}], 1, "kunde03", 0, 0, []]),
    _P1 = get_picklists(),
    _P2 = get_picklists(),
    _P3 = get_picklists(),
    _P4 = get_picklists(),
    % now we shouldn't be able to insert again because the state isn't "new" anymore
    {error, _, _} = insert_pipeline([lieferschein4, [{1,  "a0005", []}, {1,  "a0004", []}], 1, "kunde03", 0, 0, []]),
    ok.
    

delete_test() ->
    test_init(),
    ok = delete_pipeline(lieferschein1),
    [{lieferschein4,_,[{1,"a0005",[]},{1,"a0004",[]}]},
     {lieferschein3,_,[{50,"a0005",[]},{16,"a0004",[]}]},
     {lieferschein2,_,[{10,"a0005",[]},{1,"a0004",[]}]}] = provpipeline_list_new(),
    % can't delete because picks are all already open.
    _P1 = get_picklists(),
    _P2 = get_picklists(),
    _P3 = get_picklists(),
    {error, _, _} = delete_pipeline(lieferschein4),
    ok.
    

%%% @hidden
testrunner() ->
    mypl_simple_test(),
    reinsert_test(),
    delete_test(),
    ok.
    

-endif.
