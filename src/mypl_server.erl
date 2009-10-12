%% @version 0.2
%%%-------------------------------------------------------------------
%%% File    : mypl_db_server
%%% Author  : Maximillian Dornseif
%%% Description : 
%%%
%%% Created :  Created by Maximillian Dornseif on 2007-10-09.
%%%-------------------------------------------------------------------
%% @doc this allows access to mysql_db by implementing the gen_server behaviour.
%% all access to mypy_db should happen through this module

-module(mypl_server).

-behaviour(gen_server).

-define(SERVER, mypl_server).

%% API
-export([start_link/0, start/0, stop/0]).
-export([backup/0,init_location/6,store_at_location/5,store_at_location_multi/1,retrieve/1,init_movement/2,init_movement_to_good_location/1,commit_movement/1,rollback_movement/1,commit_retrieval/1,rollback_retrieval/1,init_pick/2,commit_pick/1,rollback_pick/1,correction/1,update_unit/1,count_product/1,count_products/0,unit_list/0,unit_info/1,location_list/0,location_info/1,movement_list/0,movement_info/1,pick_list/0,pick_info/1,find_empty_location_nice/1,find_provisioning_candidates/2,find_provisioning_candidates_multi/1,init_provisionings_multi/1,insert_pipeline/1,commit_picklist/1,commit_retrievallist/1,get_picklists/0,get_retrievallists/0,get_movementlist/0,provpipeline_info/1,provpipeline_list_new/0,provpipeline_list_processing/0,provpipeline_list_prepared/0,provisioninglist_list/0,provisioninglist_info/1,delete_pipeline/1,update_pipeline/1,get_articleaudit/1,get_unitaudit/1,get_articlecorrection/1,get_recent_from_archive/1,get_abc/0,get_abcclass/1,make_oid/0,make_nve/0,dump_requests/0,feed_eap/16,statistics/0,bewegungen/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], [{timeout, 40000}]).

start() -> 
    gen_server:start({local, ?SERVER}, ?MODULE, [], [{timeout, 40000}]). 

stop() -> 
    gen_server:cast(?SERVER, stop). 


% counting API
% quantities -> (quantity, pick_quantity, movement_quantity, available_quantity)
% count_products() -> [{product, quantities}, ...]
% count_article(Product) -> [{location, quantities, units}}
% location_info() -> ???
% unit_info(unit) -> (quantities, picks, movement, location)
% pick_info
% movement_info

%% gen_server callbacks

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    % every 3 seconds try to transfer audit data from temporary tables to their final destination
    timer:apply_interval(3000,  mypl_audit_transfer, spawn_audit_transfer, []),
    % dump database once a day
    timer:apply_interval(1000*60*60*24,  mypl_db, backup, []),
    % TODO: find a way to disable these in development mode
    % move abc_summary to CouchDB once a day by giving it once a hour the chance to run
    %timer:apply_interval(1000*60*59,  mypl_abcserver, spawn_abc_transfer, []),
    % move abc_summary to CouchDB once a day by giving it once a hour the chance to run
    %timer:apply_interval(1000*60*59,  mypl_audit_transfer, spawn_audit_transfer, []),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

% include autogenerated API and handle_call code
% this code is generated by genkernelinterface.py
-include("auto_genserverapi.hrl").

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
