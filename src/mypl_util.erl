%%%-------------------------------------------------------------------
%%% File    : mypl_util
%%% Author  : Maximillian Dornseif
%%% Description : 
%%%
%%% Created :  Created by Maximillian Dornseif on 2007-10-03.
%%%-------------------------------------------------------------------
-module(mypl_util).

%% API
-export([oid/0, generate_mui/0, timestamp/0, combine_until_fit/2,
         choose/2, choose/3, nearest/2, nearest/3, spawn_and_register/2, log/5]).

%% @spec oid() -> string()
%% @doc generate unique object ID
%% this ID should be unique across all processes and across all erlang nodes
%% it also should sort in ascending order
oid() -> 
    {MS,S,US} = erlang:now(),
    lists:flatten([integer_to_list(MS),"-",integer_to_list(S),"-",integer_to_list(US), "-", atom_to_list(node())]).
    

%% @spec generate_mui() -> string()
%% @doc generate a UUID for use with MUIs
%%
%% Might be someday extended into "real" NVE/SSCC generation.
generate_mui() ->
    oid().
    

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
timestamp() ->
    {Date, Time} = calendar:universal_time(),
    {_, _, MS} = erlang:now(),
    {Date, Time, MS}.
    

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
combine_until_fit(_, []) -> [];
combine_until_fit(Quantity, [H|_]) when H >= Quantity ->
    [{Quantity, H}];
combine_until_fit(Quantity, [H|T]) when H < Quantity ->
    [{H, H}| combine_until_fit(Quantity-H, T)].
    

%% @private
%% @spec perms2([integer()], integer(), integer()) -> [[integer()]]
%% @doc internal helper for caclulating permutations
perms2([], _, _) -> [];
perms2(_, Quantity, _) when Quantity =< 0 -> [];
perms2([X], Quantity, _) when X < Quantity -> [[X]];
perms2(L, Quantity, Endtime) -> % when is_list(L), is_integer(Quantity), is_integer(Endtime) -> 
    case lists:sum(L) of
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
                    
                    Ret = 
                    % permutations of H in front with the tail beeing shuffled
                    [lists:sort([H|T]) || H <- Lsmall, T <- perms2(Lsmall--[H], Quantity-H, Endtime)]
                    ++
                    % permutations with one element (H) missing 
                    [T || H <- Lsmall, T <- perms2(Lsmall--[H], Quantity, Endtime)],
                    
                    lists:usort(Ret)
            end
    end.

%% @spec permutator([integer()], integer(), integer()) -> [[integer()]]
%% @see nearest/2
%% @doc Generic permutation engine
%%
%% If it takes more than Maxtime seconds, then the computation is stopped.
permutator(L, Quantity, Maxtime) when is_list(L), is_integer(Quantity), is_integer(Maxtime) ->
    Endtime = micro_now() + (Maxtime * 1000000),
    % TOTO: possible optimisation: shorten series of identical integers so SUM(series) =< Quantity
    % with the current implementation of perms2 the sort isn't actually needed
    Permuted = perms2(lists:sort(L), Quantity, Endtime),
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

%% @spec nearest([integer()], integer(), integer()) -> [[integer()]]
%% @see nearest/2
%% @doc get the permutation which ist nearest to Quantity.
%%
%% Like {@link choose/2} but without the requrement of an exact match.
%% If it takes more than Maxtime seconds, then the computation is stopped.
nearest(L, Quantity, Maxtime) when is_list(L), is_integer(Quantity), is_integer(Maxtime) ->
    case permutator(L, Quantity, Maxtime) of
        [] -> [];
        [H|_] -> H
    end.

%% @spec nearest([integer()], integer()) -> [[integer()]]
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
%% Computation aborts after 3 seconds
nearest(L, Quantity) when is_list(L), is_integer(Quantity) ->
    nearest(L, Quantity, 3).

%% @spec choose([integer()], integer(), integer()) -> [[integer()]]
%% @see choose/2
%% @doc choose with explicit timeout.
%%
%% If it takes more than Maxtime seconds, then the computation is stopped.
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
%% With choose/2 the computation is aborted after 3 seconds.
choose(L, Quantity) when is_list(L), is_integer(Quantity) ->
    choose(L, Quantity, 3).


% This is based on http://www.nabble.com/Programming-Erlang-Exercise-8.11-t4485540.html
% Re: Programming Erlang Exercise 8.11 by Ladislav Lenart Sep 20, 2007; 09:47am

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


%%% @hidden
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
    permutator_test(),
    nearest_test(),
    choose_test(),
    ok.
    

-endif.

