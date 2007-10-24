%% Author  : Maximillian Dornseif
%% Created :  Created by Maximillian Dornseif on 2007-10-21.
%% @doc ABC-analysis
%%
%% This module is for keeping track of how frequent different products are picked. Possibly later we
%% start also to check for retrievals bt so far this module only is concerned about picks.
%% The public API only consists of {@link feed/3} and {@link get_abc/0}.

-module(mypl_abcserver).

-behaviour(gen_server).

-define(SERVER, mypl_abcserver).
-include_lib("stdlib/include/qlc.hrl").
-include("include/mypl.hrl").

%% API
-export([start_link/0, feed/3, get_abc/0]).

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

%% @spec feed(pick, Pick::mypl_db:pickRecord(), Locationname::string()) -> term()
%% @doc notifies the abcserver about a pick that happened.
%% 
%% to be called from {@link mypl_db:commit_pick/1}.
feed(pick, Pick, Locationname) ->
    gen_server:cast(?SERVER, {feed, {pick, Pick, Locationname}}).

%% @spec get_abc() -> {[{Picks::integer(), Product::string()}],[{Picks::integer(), Product::string()}],[{Picks::integer(), Product::string()}]}
%% @doc get abc classification for last 45 days
%%
%% This returns three lists of [{Picks, Product}] representing the A, B and C product classes.
%% E.g.
%% ```
%% {[{15,"14801"},{14,"10202"},{12,"71537"},{11,"14555"},{10,"01105"},{9,"92700/01"}],
%%  [{8,"74206"},{7,"14630/01"},{6,"76003"},{5,"65192"},{4,"14695"},{3,"12730"},{2,[...]},{2,...},{...}|...],
%%  [{2,"66702"},{2,"10195"},{1,"92720"},{1,"85020"},{1,[...]},{1,...},{...}|...]}'''

get_abc() ->
    gen_server:call(?SERVER, {get_abc}).



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
handle_call({get_abc}, _From, State) ->
    Reply = abc(),
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({feed, {pick, Pick, Locationname}}, State) ->
    {Date, Time, _} = Pick#pick.created_at,
    Duration = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(now())) 
               - calendar:datetime_to_gregorian_seconds({Date, Time}),
    Fun = fun() ->
            mnesia:write(#abc_pick_detail{id=mypl_util:oid(),
                                          quantity=Pick#pick.quantity,
                                          product=Pick#pick.product,
                                          duration=Duration,
                                          location=Locationname,
                                          created_at=calendar:universal_time()})
    end,
    {atomic, _ } = mnesia:transaction(Fun),
    {noreply, State}.
%handle_cast(_Msg, State) ->
%    {noreply, State}.

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


aggregate_helper([], Dict) -> Dict;
aggregate_helper([H|T], Dict) ->
    aggregate_helper(T, dict:update_counter(H#abc_pick_summary.product, H#abc_pick_summary.picks, Dict)).
    

aggregate(Start, End) ->
    update_summary(),
    Records = mypl_db_util:do(qlc:q([X || X <- mnesia:table(abc_pick_summary),
                                          X#abc_pick_summary.date > Start,
                                          X#abc_pick_summary.date =< End])),
    lists:reverse(lists:sort(lists:map(fun({Product, NumPicks}) -> 
                             {NumPicks, Product} end, 
                         dict:to_list(aggregate_helper(Records, dict:new()))))).
    
    
aggregate(Start) ->
    aggregate(Start, {9999,1,1}).
    

abc() ->
    abc_spit(aggregate()).
    

abc_split_helper(_, [], Acc) -> {lists:reverse(Acc), []};
abc_split_helper(Count, RestL, Acc) when Count < 0 -> {lists:reverse(Acc), RestL};
abc_split_helper(Count, RestL, Acc) ->
    [H|T] = RestL,
    {Quantity, _Product} = H,
    NewCount = Count - Quantity,
    abc_split_helper(NewCount, T, [H|Acc]).
abc_split_helper(Count, L) ->
    abc_split_helper(Count, L, []).
    

abc_spit(L) ->
    Splitpos = lists:sum([A || {A, _} <- L])/3,
    {A, Rest} = abc_split_helper(Splitpos, L),
    {B, C} = abc_split_helper(Splitpos, Rest),
    {A, B, C}.
    

%% @doc Give a Orderring of the products most often picked in the last 45 days.
aggregate() ->
  {Date, _} = calendar:now_to_datetime(erlang:now()),
  Start = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) - 45),
  aggregate(Start).
  

update_summary(Record) ->
    Date = element(1, Record#abc_pick_detail.created_at),
    Product = Record#abc_pick_detail.product,
    % read summary for product or create one
    case mnesia:read({abc_pick_summary, {Date, Product}}) of
        [] ->
            Summary = #abc_pick_summary{id={Date, Product},
                                        date=Date, picks=0, quantity=0, product=Product,
                                        avg_picksize=0, picksizes=[],
                                        avg_duration=0, durations=[], locations=[]};
        [Summary] ->
            updated
    end,
    Summary1 = Summary#abc_pick_summary{
                          picks=Summary#abc_pick_summary.picks + 1,
                          quantity=Summary#abc_pick_summary.quantity + Record#abc_pick_detail.quantity,
                          picksizes=[Record#abc_pick_detail.quantity|Summary#abc_pick_summary.picksizes],
                          durations=[Record#abc_pick_detail.duration|Summary#abc_pick_summary.durations],
                          locations=[Record#abc_pick_detail.location|Summary#abc_pick_summary.locations]
                          },
    Summary2 = Summary1#abc_pick_summary{avg_duration=average(Summary#abc_pick_summary.durations),
                                         avg_picksize=average(Summary#abc_pick_summary.picksizes)},
    ok = mnesia:write(Summary2),
    ok = mnesia:delete({abc_pick_detail, Record#abc_pick_detail.id}).

%% @doc aggregate records in abc_pick_detail into abc_pick_summary
update_summary() ->
    Fun = fun() ->
        lists:map(fun(X) -> update_summary(X) end, qlc:e(qlc:q([X || X <- mnesia:table(abc_pick_detail)]))),
        ok
    end,
    {atomic, _} = mnesia:transaction(Fun).


average([]) -> 0.0;
average(L) ->
    lists:sum(L) / length(L).

% ~~ Unit tests
-ifdef(EUNIT).
-compile(export_all).

average_test() ->
    15.0 = average([15]),
    15.0 = average([10,20]),
    15.0 = average([10,15,20]),
    15.0 = average([5,10,20,25]),
    15.0 = average([5,10,15,20,25]),
    ok.

abc_split_helper_test() ->
    {[{3,a},{3,b}],[{3,c},{3,d},{3,e}]} = abc_split_helper(5, [{3, a}, {3, b}, {3, c}, {3, d}, {3, e}]),
    ok.

abc_split_test() ->
    {[{3,a},{3,b}],[{3,c},{3,d}],[{3,e}]} = abc_spit([{3, a}, {3, b}, {3, c}, {3, d}, {3, e}]),
    ok.

%%% @hidden
testrunner() ->
    average_test(),
    abc_split_helper_test(),
    ok.
    

-endif.

