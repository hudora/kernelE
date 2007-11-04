%% Author  : Maximillian Dornseif
%% Created :  Created by Maximillian Dornseif on 2007-10-08.
%% @doc this server keeps track of the requests for movements (e.g. "Fixplatznachschub")

-module(mypl_requesttracker).

-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").

-define(SERVER, mypl_requesttracker).

%% API
-export([start_link/0, start/0, stop/0, in/2, out/0, movement_done/2]).

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

in(Quantity, Product) -> 
    gen_server:cast(?SERVER, {in, {Quantity, Product}}).

out() -> 
    gen_server:call(?SERVER, {out}). 

movement_done(Quantity, Product) ->
    gen_server:cast(?SERVER, {movement_done, {Quantity, Product}}). 

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
    {ok, #state{table=ets:new(requesttracker, [set])}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

%% @doc get a request {Quantity, Product} for to most urgently at floorlevel needed product
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

%% @doc inform the requesttracker of a product needed at floorlevel
handle_cast({in, {Quantity, Product}}, State) ->
    %% check if we have an open movement before adding
    case mypl_db_query:open_movements_for_product(Product) of
        [] ->
            % no open movement, so we can add
            case ets:lookup(State#state.table, Product) of
                [] ->
                    ets:insert(State#state.table, {Product, Quantity, mypl_util:timestamp()});
                [_] ->
                    ets:update_counter(State#state.table, Product, {2, Quantity})
            end;
        Movements ->
            erlang:display({not_adding, {Quantity, Product}, Movements}),
            ok % we add nothing
    end,
    {noreply, State};

handle_cast({movement_done, {_, Product}}, State) ->
    ets:delete(State#state.table, Product),
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
