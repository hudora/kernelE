%% @version 0.2
%% Author  : Maximillian Dornseif
%% Created :  Created by Maximillian Dornseif on 2007-10-08.
%% @doc this server keeps track of the requests for movements (e.g. "Fixplatznachschub")

-module(mypl_requesttracker).

-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").
-include("mypl.hrl").

-define(SERVER, mypl_requesttracker).

%% API
-export([start_link/0, start/0, stop/0, in/3, out/0, movement_done/2, dump_requests/0, flush/0]).

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

%% @doc inform the requesttrackerquesttracker than an specific quantity of a product is needed for picking
-spec in(Quantity::integer(), Product::string(), term()) -> 'ok'.
in(Quantity, Product, Priority) when is_integer(Quantity) ->
    gen_server:cast(?SERVER, {in, {Quantity, Product, Priority}}).

%% @doc get the next product which should be moved to floorlevel for picking
-spec out() -> {ok, {Quantity::integer(), Product::string()}}|{empty}.
out() -> 
    gen_server:call(?SERVER, {out}). 

%% @doc inform the requesttracker that a unit of a specific floorlevel has been moved to floorlevel
-spec movement_done(Quantity::integer(), Product::string()) -> 'ok'.
movement_done(Quantity, Product) ->
    gen_server:cast(?SERVER, {movement_done, {Quantity, Product}}). 

%% @doc get a list of all data inside the requesttracker
-spec dump_requests() ->  []|[{Product::string(),Quantity::integer(),Timestamp::term(),Priority::term()}, ...].
dump_requests() ->
    gen_server:call(?SERVER, {dump_requests}). 

%% @doc delete qll requesttrackerentries
-spec flush() -> ok.
flush() ->
    gen_server:call(?SERVER, {flush}). 

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
    % sort by timestamp
    case sort_entries(qlc:e(qlc:q([Y || Y <- ets:table(State#state.table)]))) of
        [] ->
            {reply, {empty}, State};
        [H|_] ->
            {Product, Quantity, _Timestamp, _Prio} = H,
            ets:delete(State#state.table, Product),
            {reply, {ok, {Quantity, Product}}, State}
    end;
handle_call({flush}, _From, State) ->
   Ret = ets:delete_all_objects(State#state.table),
   {reply, Ret, State};
handle_call({dump_requests}, _From, State) ->
   Ret = sort_entries(ets:tab2list(State#state.table)),
   {reply, Ret, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

%% @doc inform the requesttracker of a product needed at floorlevel
handle_cast({in, {Quantity, Product, Priority}}, State) ->
    
    %% check if we have an open movement before adding
    %% ignore movements not bound to floorlevel.
    MovLoc = [{X, mypl_db_util:read_location(X#movement.to_location) } 
              || X <- mypl_db_query:open_movements_for_product(Product),
                 X#movement.to_location /= "AUSLAG" ],
    Movements = [Mov || {Mov, Loc} <- MovLoc, Loc#location.floorlevel =:= true],
    % Movements is now a list of all Movements for this Article which are bound to floor locations
    case Movements of
        [] ->
            % no open movement, so we can add
            case ets:lookup(State#state.table, Product) of
                [] ->
                    ets:insert(State#state.table, {Product, Quantity, mypl_util:timestamp(), Priority});
                [{_OProduct, OQuantity, OTimestamp, OPriority}] ->
                    ets:insert(State#state.table,
                               {Product, Quantity + OQuantity, OTimestamp, lists:min([Priority, OPriority])}
                              )
            end;
        _Movements ->
            % since there are open movements we can't be sure that the open movements
            % would't be sufficient to fullfill the request so ...
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

sort_entries(L) ->
    lists:sort(fun({_, _, TSa, Prioa}, {_, _, TSb, Priob}) -> {Prioa, TSa} < {Priob, TSb} end, L).
