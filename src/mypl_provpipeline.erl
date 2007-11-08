%%% File    : mypl_provpipeline
%%% Author  : Maximillian Dornseif
%%% Created :  Created by Maximillian Dornseif on 2007-11-07.
-module(mypl_provpipeline).
-define(SERVER, mypl_provpipeline).

% orders to be provisioned
-record(provpipeline, {id,
                       priority,
                       orderlines,
                       weigth,
                       volume,
                       attributes
                }).
                
%    init_table_info(mnesia:create_table(provpipeline,     [{disc_copies, [node()]}, {attributes, record_info(fields, provpipeline)}]), provpipeline),
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec add(string(), Orderlines, integer(), string(), integer(), float(), Attributes) -> ok
%%           Orderlines = [{Quanity::integer(), Product::string(), Attributes}]
%%           Attributes = [{name, value}]
%% @doc adds an order to the provisioningpipeline.
%%
%% `Id' is a unique Id used by the client to refer to this order. `Orderlines' is a list of Articles to 
%% provision. The List elements are tuples `{Quanity, Product, Attributes}' where Attributes contains
%% arbitrary data for use at tha client side.
%% The higher the `priority' the more likely it is, that the Order is processed early. If you want the
%% scheduler to also consider day to deliver you have to encode that into priority. E.g.
%% E.g. `NewPriority = Priority + 10 * max([(now() + 5 - order.day_to_deliver), 0])'.
%% 'Customer' is to aggregate shippments to the same customer. 'Weigth' and 'Volume' are the calculated
%% total Weigth and Volume of the shippment and are used to make scheduling descisions.
%%
%% Example:
%% ```add(Id, [{20, 10106, [{"auftragsposition", "1"}, {"gewicht", "34567"}]},
%%          {70, 14650, [{"auftragsposition", "2"}, {"gewicht", "35667"}]},
%%          {30, 76500, [{"auftragsposition", "3"}, {"gewicht", "12367"}]}],
%%          28, "34566",
%%          345000, 581.34,
%%          [{"auftragsnumer", "123432", "liefertermin", "2007-12-23"}]).'''
add(Id, Orderlines, Priority, Customer, Weigth, Volume, Attributes) -> ok.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
