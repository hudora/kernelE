%% Author  : Maximillian Dornseif
%% Created :  Created by Maximillian Dornseif on 2007-10-21.
%% @doc this is not about the database product but about predicting the future

-module(mypl_oracle).

-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").

-define(SERVER, mypl_oracle).

%% API
-export([start_link/0, start/0, stop/0, products_needed_today/0, init_dayforcast/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {table}).

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

products_needed_today() -> 
    gen_server:call(?SERVER, {products_needed_today}). 

% needed_today(Product) ->
% needed_this_week(Product) ->
% not_needed_in_near_future(Product) ->

%% @spec feed_dayforecast([{Quantity, Product}]).
%% @doc to be called with all open orderline for the next 12h or so.
%% Call it often - e.g. every 20 minutes or so.
init_dayforcast(Orderlines) ->
    gen_server:cast(?SERVER, {init_dayforcast, {Orderlines}}).

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
    {ok, #state{table=ets:new(mypl_oracle, [set])}}.

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
        [H|_] ->
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
handle_cast({init_dayforcast, {L}}, State) ->
    ets:delete_all_objects(State#state.table),
    lists:map(fun({Quantity, Product}) -> 
                  case ets:lookup(State#state.table, Product) of
                      [] ->
                          ets:insert(State#state.table, {Product, Quantity, mypl_util:timestamp()});
                      [_] ->
                          ets:update_counter(State#state.table, Product, {2, Quantity})
                  end
              end, L),
    {noreply, State};
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
    ets:tab2file(State#state.table, "oracle_data.bak"),
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
