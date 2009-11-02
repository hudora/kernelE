%% @version 0.2
%%% File    : mypl_util
%%% Author  : Maximillian Dornseif
%%% Created :  Created by Maximillian Dornseif on 2007-10-03.
-module(mypl_util).

%% API
-export([get_config/2, serial/0, oid/0, generate_mui/0,
         timestamp/0, timestamp2binary/0, timestamp2binary/1, ensure_binary/1,
         proplist_cleanup/1, proplist_cleanup_binary/1,
         proplist_cleanup_binary2/1,
         combine_until_fit/2, choose/2, choose/3, nearest/2, nearest/3, spawn_and_register/2, log/5]).


%% @doc Get some configuration value from the applications environment.
-spec get_config(atom(),_) -> any().
get_config(Name, Default) ->
    case application:get_env(Name) of
        undefined ->
            Default;
        Value ->
            Value
    end.

%% @doc generate unique object ID
%% this ID should be unique across all processes
%% it also should sort in ascending order
%% @end
-spec oid() -> nonempty_string().
oid() ->
    {MS,S,US} = erlang:now(),
    % add , "-", atom_to_list(node()) for a distributed environment
    % to have it unique across all erlang nodes
    lists:flatten([integer_to_list(MS),integer_to_list(S),".",integer_to_list(US)]). 


%% @doc generate a nice object ID
%% this ID should be unique across all processes and across all erlang nodes
%% it also should sort in ascending order
-spec serial() -> nonempty_string().
serial() ->
    mypl_nveserver:make_oid().
    

%% @spec generate_mui() -> string()
%% @doc generate a UUID for use with MUIs
-spec generate_mui() -> nonempty_string().
generate_mui() ->
    mypl_nveserver:make_nve().
    

%% @spec micro_now() -> integer()
%% @doc returns Microseconds since 1.1.1970
%% @see erlang:now/0
micro_now() ->
    {MS, S, M} = erlang:now(),
    SecSinceEpoch = (MS * 1000000) + S,
    ((SecSinceEpoch * 1000000) + M).


%% @spec timestamp() -> term()
%% @doc generate a human readable timestamp including microseconds
%% 
%% '''
%% > timestamp().
%% {{2007,10,14},{11,58,39},691267}}'''
-spec timestamp() -> {{pos_integer(),1..12,1..31},{byte(),byte(),byte()},non_neg_integer()}.
timestamp() ->
    {Date, Time} = calendar:universal_time(),
    {_, _, MS} = erlang:now(),
    {Date, Time, MS}.
% TODO: replace all formating code scattered arround kernel with this function
-spec timestamp2binary() -> binary().
timestamp2binary() ->
    timestamp2binary(timestamp()).

-spec timestamp2binary({{_,_,_},{_,_,_}} | {{_,_,_},{_,_,_},_}) -> binary().
timestamp2binary({{Year,Month,Day},{Hour,Minute,Second}}) ->
    timestamp2binary({{Year,Month,Day},{Hour,Minute,Second},0});
timestamp2binary({{Year,Month,Day},{Hour,Minute,Second},Ms}) ->
    list_to_binary(lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0BZ",
                                               [Year, Month, Day, Hour, Minute, Second, Ms]))).

%% @doc converts a list (or a timestamp) to binary.
-spec ensure_binary(atom() | binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | byte(),binary() | []) | {maybe_improper_list(any(),[] | {_})} | {{_,_,_},{_,_,_}} | {{_,_,_},{_,_,_},_}) -> binary() | maybe_improper_list(any(),[] | {maybe_improper_list(any(),[] | {_}) | {_}}) | {maybe_improper_list(any(),[] | {_}) | {maybe_improper_list(any(),[] | {_}) | {_}}}.
ensure_binary({{Year,Month,Day},{Hour,Minute,Second}}) ->
    timestamp2binary({{Year,Month,Day},{Hour,Minute,Second},0});
ensure_binary({{Year,Month,Day},{Hour,Minute,Second},Ms}) ->
    timestamp2binary({{Year,Month,Day},{Hour,Minute,Second},Ms});
ensure_binary({List}) when is_list(List) ->
    proplist_cleanup_binary({List});
ensure_binary(Atom) when is_atom(Atom) ->
    ensure_binary(atom_to_list(Atom));
ensure_binary(Bin) when is_list(Bin) ->
    list_to_binary(Bin);
ensure_binary(Str) when is_binary(Str)->
    Str.


%% @doc converts
%% [{tries,0},
%%  {kernel_customer," "14529"},
%%  ["auftragsnummer", 647105],
%%  ["liefertermin", "2007-12-03"]]
%% to
%% [{tries,0},
%%  {kernel_customer, &lt;&lt;"14529">>},
%%  {auftragsnummer, 647105},
%%  {liefertermin, &lt;&lt;"2007-12-03">>}]
%% mainly for fixing data gotten via json
-spec proplist_cleanup([any()]) -> [{_,_}].
proplist_cleanup(L) ->
    lists:map(fun([Name, Value]) when is_atom(Name) -> 
                   proplist_cleanup_helper({Name, Value});
                 ([Name, Value]) when is_list(Name) -> 
                   proplist_cleanup_helper({erlang:list_to_atom(Name), Value});
                 ([Name, Value]) when is_binary(Name) -> 
                   proplist_cleanup_helper({erlang:list_to_atom(erlang:binary_to_list(Name)), Value});
                 ({Name, Value}) -> 
                   proplist_cleanup_helper({Name, Value}) end, L).

proplist_cleanup_helper({Name, Value}) when is_list(Value) ->
    {Name, list_to_binary(Value)};
proplist_cleanup_helper({Name, Value}) ->    
    {Name, Value}.


% changes all string values in a proplist to binary
proplist_cleanup_binary({L}) ->
    {proplist_cleanup_binary(L)};
proplist_cleanup_binary([]) ->
    [];
proplist_cleanup_binary([{K, V}|T]) when is_number(V) ->
    [{mypl_util:ensure_binary(K), V}|proplist_cleanup_binary(T)];
proplist_cleanup_binary([[K, V]|T]) when is_number(V) ->
    [{mypl_util:ensure_binary(K), V}|proplist_cleanup_binary(T)];
proplist_cleanup_binary([{K, V}|T]) ->
    [{mypl_util:ensure_binary(K), mypl_util:ensure_binary(V)}|proplist_cleanup_binary(T)];
proplist_cleanup_binary([[K, V]|T]) ->
    [{mypl_util:ensure_binary(K), mypl_util:ensure_binary(V)}|proplist_cleanup_binary(T)];
proplist_cleanup_binary([[K]|T]) ->
    [{mypl_util:ensure_binary(K), true}|proplist_cleanup_binary(T)];
proplist_cleanup_binary([{K}|T]) ->
    [{mypl_util:ensure_binary(K), true}|proplist_cleanup_binary(T)];
proplist_cleanup_binary([K|T]) when is_atom(K) ->
    [{mypl_util:ensure_binary(K), true}|proplist_cleanup_binary(T)];
proplist_cleanup_binary([H|T]) ->
    [H|proplist_cleanup_binary(T)].


proplist_cleanup_binary2({[H|T]}) ->
    {proplist_cleanup_binary2_helper([H|T])}.

proplist_cleanup_binary2_helper([]) ->
    [];
proplist_cleanup_binary2_helper([{K, V}|T]) when is_atom(K) and is_number(V) ->
    [{K, V}|proplist_cleanup_binary2_helper(T)];
proplist_cleanup_binary2_helper([{K, {{Y,M,D},{H,Min,S},Mico}}|T]) ->
    [{K, timestamp2binary({{Y,M,D},{H,Min,S},Mico})}|proplist_cleanup_binary2_helper(T)];
proplist_cleanup_binary2_helper([{K, {{Y,M,D},{H,Min,S}}}|T]) ->
    [{K, timestamp2binary({{Y,M,D},{H,Min,S}})}|proplist_cleanup_binary2_helper(T)];
proplist_cleanup_binary2_helper([{K, {[VH|VT]}}|T]) when is_atom(K) ->
    [{K, proplist_cleanup_binary2({[VH|VT]})}|proplist_cleanup_binary2_helper(T)];
proplist_cleanup_binary2_helper([{K, V}|T]) when is_atom(K) ->
    [{K, mypl_util:ensure_binary(V)}|proplist_cleanup_binary2_helper(T)];
proplist_cleanup_binary2_helper([{K}|T]) when is_atom(K) ->
    [{K, true}|proplist_cleanup_binary2_helper(T)].


%% @spec combine_until_fit(Quantity::integer(), [Value::integer()]) -> [{Takefrom::integer(), Value::integer()}]
%% @doc Takes from Values until Quantity is reached.
%%
%% Hopefully the following example explains what is actually happening:
%%
%% ```
%% > combine_until_fit(15,[5,6,7,8]).
%% [{5,5},{6,6},{4,7}]'''
%% > combine_until_fit(12,[6,6]).
%% [{6,6},{6,6}]'''
-spec combine_until_fit(_,maybe_improper_list()) -> [{pos_integer(),pos_integer()}].
combine_until_fit(_, []) -> [];
combine_until_fit(Quantity, [H|_]) when H >= Quantity ->
    [{Quantity, H}];
combine_until_fit(Quantity, [H|T]) when H < Quantity ->
    [{H, H}| combine_until_fit(Quantity-H, T)].
    

%% @private
%% @spec perms2([integer()], integer(), integer(), term()) -> [[integer()]]
%% @doc internal helper for caclulating permutations
perms2([], _, _, _) -> [];
perms2(_, Quantity, _, _) when Quantity =< 0 -> [];
perms2([X], Quantity, _, _) when X < Quantity -> [[X]];
perms2(L, Quantity, Endtime, State) -> % when is_list(L), is_integer(Quantity), is_integer(Endtime) -> 
    case ets:lookup(State, {L, Quantity}) of
        [{{L, Quantity}, Ret}] ->
            Ret;
        _ ->
            Ret = case lists:sum(L) of
                X when X =:= Quantity ->
                    [L]; % direct hit!
                _ ->
                    Now = micro_now(),
                    if
                        Now > Endtime ->
                            % we are already late
                            [];
                        true ->
                            Lsmall = [X || X <- L, X < Quantity], % remove elements beeing to big                    
                            
                            Tmp = 
                            % permutations of H in front with the tail beeing shuffled
                            [lists:sort([H|T]) || H <- Lsmall, T <- perms2(Lsmall--[H], Quantity-H, Endtime, State)]
                            ++
                            % permutations with one element (H) missing 
                            [T || H <- Lsmall, T <- perms2(Lsmall--[H], Quantity, Endtime, State)],
                            
                            lists:usort(Tmp)
                    end
            end,
            ets:insert(State, {{L, Quantity}, Ret}),
            Ret
    end.

%% @doc implement an etc cache for perms2
perms(L, Quantity, Endtime) ->
    State = ets:new(perms, [set]),
    Ret = perms2(L, Quantity, Endtime, State),
    ets:delete(State),
    Ret.
    

%% @spec permutator([integer()], integer(), integer()) -> [[integer()]]
%% @see nearest/2
%% @doc Generic permutation engine
%%
%% If it takes more than Maxtime seconds, then the computation is stopped.
permutator(L, Quantity, Maxtime) when is_list(L), is_integer(Quantity), is_integer(Maxtime) ->
    Endtime = micro_now() + (Maxtime * 1000000),
    % TODO: possible optimisation: shorten series of identical integers so SUM(series) =< Quantity
    % INFO: with the current implementation of perms2 the sort isn't actually needed
    Permuted = perms(lists:sort(L), Quantity, Endtime),
    Correct = lists:filter(fun(X) -> lists:sum(X) =< Quantity end,
                           Permuted ++ lists:map(fun(X) -> [X] end, L)),
    % remove duplicates and prefer longer lists
    CorrectSorted = lists:sort(fun (A, B) -> lists:sum(A) >= lists:sum(B) end, lists:usort(Correct)),
    case CorrectSorted of
        [] -> [];
        [H|_] ->
            % return all permutations with match the best permutation
            BestQuantity = lists:sum(H),
            lists:filter(fun(X) -> lists:sum(X) =:= BestQuantity end, CorrectSorted)
    end.

%% @see nearest/2
%% @doc get the permutation which ist nearest to Quantity.
%%
%% Like {@link choose/2} but without the requrement of an exact match.
%% If it takes more than Maxtime seconds, then the computation is stopped.
-spec nearest([any()],integer(),integer()) -> [[integer()]].
nearest(L, Quantity, Maxtime) when is_list(L), is_integer(Quantity), is_integer(Maxtime) ->
    case permutator(L, Quantity, Maxtime) of
        [] -> [];
        [H|_] -> H
    end.

%% @doc get the permutation which ist nearest to Quantity.
%%
%% Like {@link choose/2} but without the requrement of an exact match.
%% ```
%% > nearest([2,2,4,6], 6),
%% [2,4]
%% > nearest([2,4,6,16], 15)
%% [2,4,6]
%% > nearest([2,4,6], 7)
%% [2,4]'''
%% Computation aborts after 2 seconds
-spec nearest([any()],integer()) -> [[integer()]].
nearest(L, Quantity) when is_list(L), is_integer(Quantity) ->
    nearest(L, Quantity, 2).

%% @spec choose([integer()], integer(), integer()) -> [[integer()]]
%% @see choose/2
%% @doc choose with explicit timeout.
%%
%% If it takes more than Maxtime seconds, then the computation is stopped.
-spec choose([number()],integer(),pos_integer()) -> [any()].
choose(L, Quantity, Maxtime) when is_list(L), is_integer(Quantity), is_integer(Maxtime) ->
    case lists:sum(L) of
        X when X < Quantity ->
            []; % no chance of ever reaching our Quantity
        _ ->
            Permuted = permutator(L, Quantity, Maxtime),
            Correct = lists:filter(fun(X) -> lists:sum(X) =:= Quantity end, Permuted),
            lists:sort(fun (A, B) -> length(A) >= length(B) end, lists:usort(Correct))
    end.

%% @spec choose([integer()], integer()) -> [[integer()]]
%% @doc suggests how to combine goods to get to a desired quantity.
%%
%% choose/2 tries to get all permutations of how to select goods from the warehouse.
%% suppose you have 4 crates of 4kg toothpaste, 2 crates of 2kg toothpase, 1 crate of 3 and 4kg.
%% choose/2 gives you possibilities what to ship to the customer if he wants 4kg toothpaste:
%%
%% ```
%% > choose([1,1,1,1,2,2,3,4], 4).
%% [[1,1,1,1],[1,1,2],[2,2],[1,3],[4]]'''
%%
%% The whole thing very easyly hits a wall preformance wise, not finding obvious solutions like [1,2,2]:
%% ```
%% > choose([1,1,1,1,2,2,2,3,3,3,4,5,6,7], 5).
%% [[5]]'''
%%
%% use choose/3 to speciy how many seconds it should take to find solutions. E.g. the example from above
%% yields reasonable solutions on my macheine when given 10 Seconds:
%% ```
%% > choose([1,1,1,1,2,2,3,3,3,4,5,6,7], 5, 10).
%% [[1,1,1,2],[1,2,2],[1,1,3],[2,3],[1,4],[5]]'''
%% With choose/2 the computation is aborted after 2 seconds.
-spec choose([number()],integer()) -> [any()].
choose(L, Quantity) when is_list(L), is_integer(Quantity) ->
    choose(L, Quantity, 2).


% This is based on http://www.nabble.com/Programming-Erlang-Exercise-8.11-t4485540.html
% Re: Programming Erlang Exercise 8.11 by Ladislav Lenart Sep 20, 2007; 09:47am
-spec spawn_and_register(atom(),fun(() -> any())) -> 'already_running' | {'ok',pid()}.
spawn_and_register(Atom, Fun) when is_atom(Atom), is_function(Fun, 0) -> 
    Sender = self(), 
    Fun2 = fun() -> 
        case catch register(Atom, self()) of 
            true -> 
                Sender ! {started, self()}, 
                Fun(); 
            _ -> 
                Sender ! {already_running, self()} 
        end 
    end, 
    Pid = spawn(Fun2), 
    receive 
        {started, Pid} -> 
            {ok, Pid}; 
        {already_running, Pid} -> 
            already_running 
    end.


% from adviserl
%%% @doc  Add information in the log streams.
%%% If Level is <em>dbg</em>, print message on <em>stdout</em>; else use the standard application <em>error_logger</em> (levels stands for info, warning and error).<br/>
%%% This function is used through applications macros (<em>mypl.hrl</em>) which automaticaly capture <em>Level</em>, <em>Module</em> and <em>Line</em>.
%%% @spec (Module::atom(), Line::integer(), Level, Msg::string(), Params) -> integer()
%%%   Level = debug|dbg | normal|inf | warn|wrn | error|err
%%%   Params = [term()]
%%% @end
-spec log(_,_,'dbg' | 'debug' | 'err' | 'error' | 'inf' | 'normal' | 'warn' | 'wrn',[any()],[any()]) -> 'ok'.
log(Module, Line, debug, Msg, Params) ->
    io:format(
        "Debug:~p:~p: " ++ Msg ++ "~n",
        [Module, Line] ++ Params
    );
log(Module, Line, dbg, Msg, Params) ->
    io:format(
        "Debug:~p:~p: " ++ Msg ++ "~n",
        [Module, Line] ++ Params
    );
log(Module, Line, normal, Msg, Params) ->
    error_logger:info_msg(
        "~p:~p: " ++ Msg ++ "~n",
        [Module, Line] ++ Params
    );
log(Module, Line, inf, Msg, Params) ->
    error_logger:info_msg(
        "~p:~p: " ++ Msg ++ "~n",
        [Module, Line] ++ Params
    );
log(Module, Line, warn, Msg, Params) ->
    error_logger:warning_msg(
        "~p:~p: " ++ Msg ++ "~n",
        [Module, Line] ++ Params
    );
log(Module, Line, wrn, Msg, Params) ->
    error_logger:warning_msg(
        "~p:~p: " ++ Msg ++ "~n",
        [Module, Line] ++ Params
    );
log(Module, Line, error, Msg, Params) ->
    error_logger:error_msg(
        "~p:~p: " ++ Msg ++ "~n",
        [Module, Line] ++ Params
    );
log(Module, Line, err, Msg, Params) ->
    error_logger:error_msg(
        "~p:~p: " ++ Msg ++ "~n",
        [Module, Line] ++ Params
    ).
    

% ~~ Unit tests
-ifdef(EUNIT).
-compile(export_all).

timing_helper() ->
    choose([1,2,3,4,5,6,7,8,9,15,15,15,15], 60, 999).

timing() ->
    {Time , Ret} =  timer:tc(?MODULE, timing_helper, []),
    Secs = Time div 1000,
    io:format("~n~p ms~n~w~n",[Secs, Ret]).
    

%%% @hidden

get_config_test() ->
    123 = get_config(foobarda, 1234).

proplist_cleanup_test() ->
    [{tries,0}, {kernel_customer, <<"14529">>}, {auftragsnummer, 647105},
        {liefertermin, <<"2007-12-03">>}] = proplist_cleanup([{tries,0}, {kernel_customer, "14529"},
        ["auftragsnummer", 647105], ["liefertermin", "2007-12-03"]]).

permutator_test() ->
    [[1,1,1,1],[1,1,2],[1,3],[2,2],[4]] = choose([1,1,1,1,2,2,3,4], 4),
    [[2,4],[6]] = permutator([2,2,4,6], 6, 1),
    [[2,4],[3,3],[6]] = permutator([2,3,3,4,6], 6, 1),
    ok.
    

nearest_test() ->
    [2,4] = nearest([2,2,4,6], 6),
    [2,4,6] = nearest([2,4,6,16], 15),
    [2,4,6] = nearest([2,4,6], 15),
    [2,4] = nearest([2,4,6], 7),
    [2,4] = nearest([2,2,4,6], 7),
    ok.
    
%%% @hidden
choose_test() ->
    [[1,1,2],[1,3],[2,2],[4]] = choose([1,1,2,2,3,4], 4, 1),
    [2,4] = nearest([2,2,4,6], 6),
    [2,4,6] = nearest([2,4,6,16], 15),
    [2,4,6] = nearest([2,4,6], 15),
    [2,4] = nearest([2,4,6], 7),
    [2,4] = nearest([2,2,4,6], 7),
    [2, 48, 48] = nearest([48,2,48,48,48], 100),
    ok.
    

%%% @hidden
combine_until_fit_test() ->
    [{5,5},{6,6},{4,7}] = combine_until_fit(15,[5,6,7,8]),
    [{6,6},{6,6}] = combine_until_fit(12,[6,6]),
    ok.
    

%%% @hidden
testrunner() ->
    get_config_test(),
    proplist_cleanup_test(),
    permutator_test(),
    nearest_test(),
    choose_test(),
    ok.
    

-endif.

