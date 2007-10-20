%%%-------------------------------------------------------------------
%%% File    : mypl_queue
%%% Author  : Maximillian Dornseif
%%% Description : 
%%%
%%% Created :  Created by Maximillian Dornseif on 2007-10-08.
%%%-------------------------------------------------------------------
%% @doc this server keeps track of the requests for movements (e.g. "Fixplatznachschub")

-module(mypl_requesttracker).

-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").

-define(SERVER, mypl_requesttracker).

%% API
-export([start_link/0, start/0, stop/0, in/2, out/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {queue, table}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() -> 
    gen_server:start({local, ?SERVER}, ?MODULE, [], []). 

in(Quantity, Product) -> 
    %% TODO: check if we have an open movement before adding
    gen_server:cast(?SERVER, {in, {Quantity, Product}}). 

out() -> 
    gen_server:call(?SERVER, {out}). 

stop() -> 
    gen_server:cast(?SERVER, stop). 

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
    process_flag(trap_exit, true),
    {ok, #state{queue=queue:new(), table=ets:new(requesttracker, [ordered_set])}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({out}, _From, State) ->
    case lists:sort(fun({_, _, TSa}, {_, _, TSb}) -> TSa < TSb end,
                    qlc:e(qlc:q([Y || Y <- ets:table(State#state.table)]))) of
        [] ->
            {reply, {empty}, State};
        [H|T] ->
            {Product, Quantity, _} = H,
            ets:delete(State#state.table, Product),
            {reply, {ok, {Quantity, Product}}, State}
    end.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({in, {Quantity, Product}}, State) ->
    case ets:lookup(State#state.table, Product) of
        [] ->
            ets:insert(State#state.table, {Product, Quantity, mypl_util:timestamp()});
        [_] ->
            ets:update_counter(State#state.table, Product, {2, Quantity})
    end,
    {noreply, State#state{queue=queue:in({Quantity, Product}, State#state.queue)}};
handle_cast({stop}, State) ->
    terminate(unknown, State),
    {stop, normal, State}.


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
terminate(_Reason, State) ->
    io:format("Left over movementsuggestions: ~w~n", [queue:to_list(State#state.queue)]),
    ets:delete(State#state.table),
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
