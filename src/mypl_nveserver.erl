%%%-------------------------------------------------------------------
%%% File    : mypl_nveserver
%%% Author  : Maximillian Dornseif
%%% Description : 
%%%
%%% Created :  Created by Maximillian Dornseif on 2007-10-25.
%%%-------------------------------------------------------------------
-module(mypl_nveserver).

-behaviour(gen_server).
-define(SERVER, mypl_nveserver).

%% API
-export([start_link/0, make_nve/0, make_oid/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {nvepos, oidpos, generated_count}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

make_nve() ->
    gen_server:call(?SERVER, {make_nve}).

make_oid() ->
    gen_server:call(?SERVER, {make_oid}).

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
    State = init_state(),
    % checkpoint at least every 60 seconds
    timer:apply_interval(60000,  gen_server, cast, [?SERVER, {write_checkpoint}]),
    {ok, State}.
    

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({make_nve}, _From, State) ->
    Reply = make_nve(State#state.nvepos),
    NewState = check_checkpoint(State),
    {reply, Reply, NewState#state{nvepos=State#state.nvepos+1, generated_count=State#state.generated_count+1}};
handle_call({make_oid}, _From, State) ->
    Reply = make_oid(State#state.oidpos),
    NewState = check_checkpoint(State),
    {reply, Reply, NewState#state{oidpos=State#state.oidpos+1, generated_count=State#state.generated_count+1}}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({write_checkpoint}, State) ->
    write_checkpoint(State),
    {noreply, write_checkpoint(State)}.

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

ean_digit(Nums) ->
    {_, Summe} = 
    lists:foldr(fun(X, {Factor, Summe}) -> 
                    {4-Factor, Summe + ((X-$0) * Factor)}
                end, {3, 0}, Nums),
    [$0 + ((10-(Summe rem 10)) rem 10)].

pad_helper(Howmuch, L) when Howmuch > 0 ->
    pad_helper(Howmuch - 1, [$0|L]);
pad_helper(_Howmuch, L) -> L.

pad(Howmuch, L) ->
    pad_helper(Howmuch-length(L), L).

make_nve(Num) ->
    Prefix = "340059981",
    NveRaw = Prefix ++ pad(8, erlang:integer_to_list(Num)),
    NveRaw ++ ean_digit(NveRaw).
    
make_oid(Num) ->
    "Z" ++ pad(7, erlang:integer_to_list(Num)).
    
    
checkpoint_file() -> mnesia:system_info(directory) ++ "/nveserver_checkpoint".

% reads state file from disk
init_state() ->
    FileName = checkpoint_file(),
    case file:consult(FileName) of
        {ok, [{version1, State}]} -> 
            % we restart 1000 after the last checkpoint to ensure nothing is lost between checkpoints
            State#state{nvepos=State#state.nvepos+1000, oidpos=State#state.oidpos+1000, generated_count=0};
        Data ->
            erlang:display({cannot_read_nveserver_checkpoint, FileName, Data}),
            #state{nvepos=0, oidpos=0, generated_count=0}
    end.

%% @doc write current state to disk
write_checkpoint(State) ->
    if  % only save if we have generated new numbers sice last call
        State#state.generated_count > 0 ->
            FileName = checkpoint_file(),
            erlang:display({write_cp2, FileName}),
            Handle = case file:open(FileName, [write]) of
                         {ok, Device} -> Device;
                         {error, Reason} ->
                             throw({error, {cannot_create_nveserver_checkpoint,
                                            FileName, Reason}})
                     end,
            try
                ok = io:write(Handle, {version1, State}),
                ok = io:put_chars(Handle, [$.])
            after
                ok = file:close(Handle)
            end,
            State#state{generated_count=0};
        true ->
            State
    end.

% @doc forces checkpoint to be written to disk at least once every 990 sec
check_checkpoint(State) ->
    erlang:display({write_cp, State}),
    % if more than 990 records have been created, save checkpoint to disk
    if
        State#state.generated_count > 990 ->
            write_checkpoint(State);
        true ->
            State
    end.
    

% ~~ Unit tests
-ifdef(EUNIT).
-compile(export_all).
    
testrunner() ->
    [$0] = ean_digit(""),
    [$4] = ean_digit("2"), 
    [$2] = ean_digit("22"), 
    [$6] = ean_digit("222"), 
    [$0] = ean_digit("1234567"), 
    [$2] = ean_digit("12345678901"), 
    [$4] = ean_digit("062235652032"), 
    [$1] = ean_digit("978014200057"), 
    [$2] = ean_digit("400599871650"), 
    [$8] = ean_digit("400599899379"), 
    [$1] = ean_digit("1234567890123"), 
    [$0] = ean_digit("0000001234567"), 
    [$2] = ean_digit("0012345678901"), 
    [$8] = ean_digit("0123456789012"), 
    [$0] = ean_digit("00000000000000000"), 
    [$7] = ean_digit("00000000000000001"), 
    [$9] = ean_digit("00000000000000010"), 
    [$7] = ean_digit("00000000000000100"), 
    [$5] = ean_digit("12345678901234567"), 
    [$0] = ean_digit("22222222222222222"), 
    [$5] = ean_digit("99999999999999999"), 
    [$8] = ean_digit("34005998000000026"), 
    [$5] = ean_digit("34005998000000027"), 
    [$2] = ean_digit("34005998000000028"), 
    [$5] = ean_digit("99999999999999999"), 
    [$6] = ean_digit("999999999999999999"), 
    [$9] = ean_digit("9999999999999999999"), 
    [$0] = ean_digit("99999999999999999999"), 
    [$3] = ean_digit("999999999999999999999"), 
    [$4] = ean_digit("9999999999999999999999"), 
    [$7] = ean_digit("99999999999999999999999"),
    [$8] = ean_digit("999999999999999999999999"), 
    [$1] = ean_digit("9999999999999999999999999"), 
    [$2] = ean_digit("99999999999999999999999999"), 
    [$5] = ean_digit("999999999999999999999999999"), 
    [$6] = ean_digit("9999999999999999999999999999"),
    [$9] = ean_digit("99999999999999999999999999999"), 
    [$0] = ean_digit("999999999999999999999999999999"), 
    [$3] = ean_digit("9999999999999999999999999999999"), 
    [$4] = ean_digit("99999999999999999999999999999999"), 
    [$7] = ean_digit("999999999999999999999999999999999"), 
    [$8] = ean_digit("9999999999999999999999999999999999"), 
    [$1] = ean_digit("99999999999999999999999999999999999"), 
    [$2] = ean_digit("999999999999999999999999999999999999"), 
    [$5] = ean_digit("9999999999999999999999999999999999999"), 
    [$6] = ean_digit("99999999999999999999999999999999999999"), 
    [$9] = ean_digit("999999999999999999999999999999999999999"), 
    [$0] = ean_digit("9999999999999999999999999999999999999999"), 
    [$3] = ean_digit("99999999999999999999999999999999999999999"), 
    [$4] = ean_digit("999999999999999999999999999999999999999999"), 
    [$7] = ean_digit("9999999999999999999999999999999999999999999"), 
    [$8] = ean_digit("99999999999999999999999999999999999999999999"), 
    [$1] = ean_digit("999999999999999999999999999999999999999999999"), 
    [$2] = ean_digit("9999999999999999999999999999999999999999999999"), 
    [$5] = ean_digit("99999999999999999999999999999999999999999999999"), 
    [$6] = ean_digit("999999999999999999999999999999999999999999999999"), 
    [$9] = ean_digit("9999999999999999999999999999999999999999999999999"), 
    [$0] = ean_digit("99999999999999999999999999999999999999999999999999"), 
    [$3] = ean_digit("999999999999999999999999999999999999999999999999999"), 
    [$4] = ean_digit("9999999999999999999999999999999999999999999999999999"), 
    [$7] = ean_digit("99999999999999999999999999999999999999999999999999999"), 
    [$8] = ean_digit("999999999999999999999999999999999999999999999999999999"), 
    [$1] = ean_digit("9999999999999999999999999999999999999999999999999999999"), 
    [$2] = ean_digit("99999999999999999999999999999999999999999999999999999999"), 
    [$5] = ean_digit("999999999999999999999999999999999999999999999999999999999"), 
    [$6] = ean_digit("9999999999999999999999999999999999999999999999999999999999"),
    [$9] = ean_digit("99999999999999999999999999999999999999999999999999999999999"),
    ok.

-endif.
