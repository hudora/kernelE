%% @version 0.1
%%% Created :  Created by Maximillian Dornseif on 2009-10-13.
-module(mypl_zwitscherserver).

-behaviour(gen_server).
-define(SERVER, mypl_zwitscherserver).

-include("amqp_client.hrl").

%% API
-export([start_link/0, zwitscher/1, zwitscher/2]).

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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%-spec zwitscher(string()) -> term().
-spec zwitscher(Format :: string()) -> 'ok'.
zwitscher(Format) ->
    zwitscher(Format, []).

-spec zwitscher(Format :: string(), Args :: list()) -> 'ok'.
zwitscher(Format, Args) ->
    % start the server if it is not already running
    case whereis(?SERVER) of
        undefined -> start_link();
        _ -> ok
    end,
    gen_server:cast(?SERVER, {zwitscher, {lists:flatten(io_lib:format(Format, Args))}}).
    

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
init([]) ->
    Connection = amqp_connection:start_network(#amqp_params{username = <<"mypl">>,
                                                            password = <<"iajoQuoomu6Woosh7Ief">>,
                                                            host="mail.hudora.biz"}),
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
handle_call(_Req, _From, State) ->
    {reply, {error, badrequest}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({zwitscher, {Tweet}}, State) ->
    Publish = #'basic.publish'{exchange = <<"zwitscher">>, routing_key = <<"zwitscher">>},
    Data = list_to_binary(lists:flatten(myjson:encode({[{text, mypl_util:ensure_binary(Tweet)},
                                        {username, <<"mypl">>},
                                        {password, <<"mypl">>},
                                        {created_by, mypl_zwitscherserver},
                                        {audit_trail, <<"">>},
                                        {guid, mypl_util:ensure_binary([mypl_util:oid(), "#", atom_to_list(node())])}]}))),
    ok = amqp_channel:call(State#state.channel, Publish, #amqp_msg{payload = Data}),
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
-spec terminate(_,#state{}) -> #state{}.
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

