%% @version 0.1
%%% Created :  Created by Maximillian Dornseif on 2009-10-20.
-module(mypl_to_ic).

-behaviour(gen_server).
-define(SERVER, mypl_to_ic).

-include("amqp_client.hrl").

%% API
-export([start_link/0, nullen/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {connection, channel}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> {ok,pid()} | ignore | {error,_}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec nullen(string(), [{}, ...], string()|binary()) -> ok.

nullen(KommiauftragNr, Positionen, Message) ->
    Positiondict = [{[{posnr, proplists:get_value(posnr, X)},
                      {menge, 0},
                      {artnr, proplists:get_value(artnr, X)}]} || {X} <- Positionen],
    Data1 = [{positionen, Positiondict},
             {kommiauftragnr, mypl_util:ensure_binary(KommiauftragNr)},
             {action, <<"nullen">>},
             {created_at, mypl_util:timestamp2binary()},
             {created_by, mypl_util:ensure_binary(mypl_to_ic)},
             {audit_trail, mypl_util:ensure_binary(Message)},
             {guid, mypl_util:ensure_binary([mypl_util:oid(), "#", atom_to_list(node())])}],
    Data2 = myjson:encode({Data1}),
    Data3 = lists:flatten(Data2),
    Data = list_to_binary(Data3),
    gen_server:call(?SERVER, {nullen, Data}).
    

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
-spec init([]) -> {'ok',#state{}}.
init([]) ->
    Connection = amqp_connection:start_network(#amqp_params{username = <<"mypl">>,
                                                            password = <<"iajoQuoomu6Woosh7Ief">>,
                                                            host="rabbitmq.local.hudora.biz"}),
    Channel = amqp_connection:open_channel(Connection),
    {ok, #state{connection=Connection, channel=Channel}}.
    

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({nullen, Data}, _From, State) ->
    Publish = #'basic.publish'{exchange = <<"erp.cs-wms.rueckmeldung#spezial">>, 
                               routing_key = <<"erp.cs-wms.rueckmeldung#spezial">>},
    ok = amqp_channel:call(State#state.channel, Publish, #amqp_msg{payload = Data}),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
-spec handle_cast(_,_) -> {'noreply',_}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
-spec handle_info(_,_) -> {'noreply',_}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
-spec terminate(_,#state{}) -> #state{}.
terminate(_Reason, State) ->
    State.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

