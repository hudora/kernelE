%% @version 0.2
%%% Created :  Created by Maximillian Dornseif on 2007-10-25.
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

%% @doc return a NVE/SSCC out of the hudora number range
%% http://de.wikipedia.org/wiki/Nummer_der_Versandeinheit
-spec make_nve() -> string().
make_nve() ->
    % start the server if it is not already running
    case whereis(?SERVER) of
        undefined -> start_link();
        _ -> ok
    end,
    gen_server:call(?SERVER, {make_nve}).

%% @doc return a unique Identifier.
-spec make_oid() -> string().
make_oid() ->
    % start the server if it is not already running
    case whereis(?SERVER) of
        undefined -> start_link();
        _ -> ok
    end,
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
    % checkpoint at least every 17 seconds
    timer:apply_interval(17000,  gen_server, cast, [?SERVER, {write_checkpoint}]),
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
    {reply, Reply, NewState#state{nvepos=State#state.nvepos+1, generated_count=NewState#state.generated_count+1}};
handle_call({make_oid}, _From, State) ->
    Reply = make_oid(State#state.oidpos),
    NewState = check_checkpoint(State),
    {reply, Reply, NewState#state{oidpos=State#state.oidpos+1, generated_count=NewState#state.generated_count+1}}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({write_checkpoint}, State) ->
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
-spec terminate(_,#state{}) -> #state{}.
terminate(_Reason, State) ->
    write_checkpoint(State).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @doc calculate a EAN check digit
%% input is a string containing only digits, output is a single digit.
% verify at http://www.gs1.org/productssolutions/barcodes/support/check_digit_calculator.html
-spec ean_digit(nonempty_string()) -> nonempty_string().
ean_digit(Nums) ->
    {_, Summe} = 
    lists:foldr(fun(X, {Factor, Summe}) -> 
                    {4-Factor, Summe + ((X-$0) * Factor)}
                end, {3, 0}, Nums),
    [$0 + ((10-(Summe rem 10)) rem 10)].


% dihedral addition matrix A+B = a[A][B] used by verhoeff_digit()
-define(DIHEDRAL_A, {{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 },
                     { 1, 2, 3, 4, 0, 6, 7, 8, 9, 5 },
                     { 2, 3, 4, 0, 1, 7, 8, 9, 5, 6 },
                     { 3, 4, 0, 1, 2, 8, 9, 5, 6, 7 },
                     { 4, 0, 1, 2, 3, 9, 5, 6, 7, 8 },
                     { 5, 9, 8, 7, 6, 0, 4, 3, 2, 1 },
                     { 6, 5, 9, 8, 7, 1, 0, 4, 3, 2 },
                     { 7, 6, 5, 9, 8, 2, 1, 0, 4, 3 },
                     { 8, 7, 6, 5, 9, 3, 2, 1, 0, 4 },
                     { 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 }}).
-define(DIHEDRAL_INVERSE, {0, 4, 3, 2, 1, 5, 6, 7, 8, 9}).
-define(DIHEDRAL_P, {{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 },
                     { 1, 5, 7, 6, 2, 8, 3, 0, 9, 4 },
                     { 5, 8, 0, 3, 7, 9, 6, 1, 4, 2 },
                     { 8, 9, 1, 6, 0, 4, 3, 5, 2, 7 },
                     { 9, 4, 5, 3, 1, 2, 6, 8, 7, 0 },
                     { 4, 2, 8, 6, 5, 7, 3, 9, 0, 1 },
                     { 2, 7, 9, 3, 8, 0, 6, 4, 1, 5 },
                     { 7, 0, 4, 6, 9, 1, 3, 2, 5, 8 }}).

-spec verhoeff_digit_helper(pos_integer(),{pos_integer(),non_neg_integer()}) -> {pos_integer(),0..9}.
verhoeff_digit_helper(Char, {Pos, X}) ->
    Pval = element((Char-$0)+1, element(((Pos+1) rem 8)+1, ?DIHEDRAL_P)),
    XNew = element(Pval+1, element(X+1, ?DIHEDRAL_A)),
    {Pos + 1, XNew}.
    

%% @doc generate a Verhoeff Check Digit
%% input is a string containing only digits, output is a single digit.
%% See http://en.wikipedia.org/wiki/Verhoeff_algorithm
-spec verhoeff_digit(nonempty_string()) -> nonempty_string().
verhoeff_digit(NumStr) ->
    % There are various (contradicting) implementation guidelines for the Verhoeff algorithm:
    % http://www.cs.utsa.edu/~wagner/pubs/DecimalDigits.pdf
    % http://static.23.nu/md/Pictures/SNOMEDCT_Core_Technical+Reference+Manual_US.pdf
    % http://static.23.nu/md/Pictures/adfa.pdf
    % http://www.cs.utsa.edu/~wagner/laws/verhoeff.html
    % http://en.wikipedia.org/wiki/Verhoeff_algorithm
    % http://www.vuemetrix.com/vueseries/comm_protocols.html
    % http://en.dahnielson.com/2006/09/verhoeff.html
    % http://www.ams.org/featurecolumn/archive/barcodes6.html
    % see voerhoeff_test() for some examples of contradicting results.
    {_, X} = lists:foldl(fun(Char, Acc) -> verhoeff_digit_helper(Char, Acc) end,
                        {0, 0}, lists:reverse(NumStr)),
    [$0 + element(X+1, ?DIHEDRAL_INVERSE)].


-spec pad_helper(pos_integer(),string()) -> string().
pad_helper(Howmuch, L) when Howmuch > 0 ->
    pad_helper(Howmuch - 1, [$0|L]);
pad_helper(_Howmuch, L) -> L.

%% @doc 0-pad a string
%-spec pad(pos_integer(),string()) -> string().
% currently we only pad to 7 or 8 characters
-spec pad(7|8,string()) -> string().
pad(Howmuch, L) ->
    pad_helper(Howmuch-length(L), L).

%% @doc generate a NVE/SSCC from
%%
%% This are generated from HUDORA's pool of NVEs keeping internal state to ensure no duplicate NVEs
%% are generated.
%% 
%% ```1> mypl_nveserver:make_nve().
%% "340059981000000005"
%% 2> mypl_nveserver:make_nve().
%% "340059981000000012"
%% 3> mypl_nveserver:make_nve().
%% "340059981000000029"'''
-spec make_nve(pos_integer()) -> nonempty_string().
make_nve(Num) ->
    Prefix = "340059981",
    NveRaw = Prefix ++ pad(8, erlang:integer_to_list(Num)),
    NveRaw ++ ean_digit(NveRaw).
    

%% @doc generate a unique number
%%
%% This numbers have the propertiey that they sort according to sequence of creation and they are
%% ending with a Verhoeff Check Digit. You can simply ignore that fact but you also can use it
%% to verify it during data entry.
%%
%% ```1> mypl_nveserver:make_oid()
%% "00000001"
%% 2> mypl_nveserver:make_oid().
%% "00000017"
%% 3> mypl_nveserver:make_oid().
%% "00000029"
%% 4> mypl_nveserver:make_oid().
%% "00000038"'''
-spec make_oid(pos_integer()) -> nonempty_string().
make_oid(Num) ->
    NumStr = pad(7, erlang:integer_to_list(Num)),
    NumStr ++ verhoeff_digit(NumStr).
    

%% @doc get filename for checkpoint file
-spec checkpoint_file() -> nonempty_string().
checkpoint_file() -> mnesia:system_info(directory) ++ "/nveserver_checkpoint".


% reads state file from disk
init_state() ->
    FileName = checkpoint_file(),
    case file:consult(FileName) of
        {ok, [{version1, State}]} -> 
            % we restart about 200 after the last checkpoint to ensure nothing is lost between checkpoints
            State#state{nvepos=State#state.nvepos+201, oidpos=State#state.oidpos+203, generated_count=0};
        _Data ->
            error_logger:warning_msg("cannot read nveserver checkpoint file ~s during startup, starting at 2000000.",
                                     [FileName]),
            mypl_zwitscherserver:zwitscher("!!! ERROR: cannot read nveserver checkpoint file ~s during startup, starting at 2000000.",
                                     [FileName]),
            #state{nvepos=2000000, oidpos=2000000, generated_count=2000000}
    end.

%% @doc write current state to disk
-spec write_checkpoint(#state{}) -> #state{}.
write_checkpoint(State) when State#state.generated_count > 0 ->
    FileName = checkpoint_file() ++ ".new",
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
        ok = file:close(Handle),
        file:rename(FileName, checkpoint_file())
    end,
    State#state{generated_count=0};
write_checkpoint(State) ->
    State.


%% @doc forces checkpoint to be written to disk at least once every 41 numbers
%% if more than 41 records have been created, save checkpoint to disk
-spec check_checkpoint(#state{}) -> #state{}.
check_checkpoint(State) when State#state.generated_count > 41 ->
    write_checkpoint(State);
check_checkpoint(State) ->
    State.
    

% ~~ Unit tests
-ifdef(EUNIT).
-compile(export_all).
    
verhoeff_test() ->
    % Different Implementations of Verhoeff's Dihedral Check Digit seem to produce different results.
    % We stick to the way of calculating implemented by Perl's Algorithm::Verhoeff,
    % http://www.augustana.ab.ca/~mohrj/algorithms/checkdigit.html and 
    % http://en.wikipedia.org/wiki/Verhoeff_algorithm
    % The Implementation of http://code.google.com/p/checkdigits/ sems not to reverse the input before
    % calculation which seems to be the only difference.
    
    % http://www.esb.ie/esbnetworks/mrso/mprn.jsp
    % [$9] = verhoeff_digit("10003729"),
    
    % http://web.mit.edu/kenta/www/three/verhoeff-checksum.html.gz
    % [$8] = verhoeff_digit("1125"),
    % [$5] = verhoeff_digit("16412"),
    
    % http://www.vuemetrix.com/vueseries/comm_protocols.html
    % [$2] = verhoeff_digit("0000168"),
    % [$1] = verhoeff_digit("04052"),
    % [$9] = verhoeff_digit("1"),
    
    % Perl's Algorithm::Verhoeff test code
    [$9] = verhoeff_digit("123456654321"),
    [$3] = verhoeff_digit("5743839105748193475681981039847561718657489228374"),
    
    % http://en.wikipedia.org/wiki/Verhoeff_algorithm
    [$0] = verhoeff_digit("142857"),
    
    % http://www.augustana.ab.ca/~mohrj/algorithms/checkdigit.html
    [$5] = verhoeff_digit("1"),
    [$3] = verhoeff_digit("11"),
    [$3] = verhoeff_digit("050"),
    [$3] = verhoeff_digit("505"),
    [$1] = verhoeff_digit("494"),
    [$1] = verhoeff_digit("949"),
    [$5] = verhoeff_digit("272"),
    [$5] = verhoeff_digit("727"),
    [$8] = verhoeff_digit("161"),
    [$8] = verhoeff_digit("616"),
    [$4] = verhoeff_digit("383"),
    [$9] = verhoeff_digit("838"),
    [$2] = verhoeff_digit("1125"),
    [$6] = verhoeff_digit("04052"),
    [$8] = verhoeff_digit("16412"),
    [$2] = verhoeff_digit("505505"),
    [$4] = verhoeff_digit("050050"),
    [$0] = verhoeff_digit("142857"),
    [$4] = verhoeff_digit("1456789"),
    [$6] = verhoeff_digit("0000168"),
    [$1] = verhoeff_digit("10003729"),
    [$9] = verhoeff_digit("123456654321"),
    [$3] = verhoeff_digit("5743839105748193475681981039847561718657489228374"),
    ok.
    
ean_test() ->
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

testrunner() ->
    verhoeff_test(),
    ean_test(),
    ok.

-endif.
