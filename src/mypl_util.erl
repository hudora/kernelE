%%%-------------------------------------------------------------------
%%% File    : mypl_util
%%% Author  : Maximillian Dornseif
%%% Description : 
%%%
%%% Created :  Created by Maximillian Dornseif on 2007-10-03.
%%%-------------------------------------------------------------------
-module(mypl_util).

%% API
-export([oid/0, generate_mui/0, choose/2, choose/3, spawn_and_register/2, log/5]).

% generate unique object ID
oid() -> 
    {MS,S,US} = erlang:now(),
    lists:flatten([integer_to_list(MS-1190),"-",integer_to_list(S),"-",integer_to_list(US), "-", atom_to_list(node())]).

generate_mui() ->
    oid().

%% a bunch of functions ot get all permutations of how to select boods from the warehouse.
%% suppose you have 4 crates of 4kg toothpaste, 2 crates of 2kg toothpase, 1 crate of 3 and 4kg.
%% choose/2 gives you possibilities what to ship to the customer if he wants 4kg toothpaste:
%% > choose([1,1,1,1,2,2,3,4], 4).
%%  [[1,1,1,1],[1,1,2],[2,2],[1,3],[4]]

%% The whole thing very easyly hits a wall preformance wise:
%% > choose([1,1,1,1,2,2,2,3,3,3,4,5,6,7], 5).
%% [[5]]
%% use choose/3 to say how many seconds it should take to find solutions
%% > choose([1,1,1,1,2,2,3,3,3,4,5,6,7], 5, 10).
%% [[1,1,1,2],[1,2,2],[1,1,3],[2,3],[1,4],[5]]

%%
listsum(L) when is_list(L) ->
    lists:foldl(fun(X, Sum) -> X + Sum end, 0, L).

perms2([], _, _) -> [];
perms2(_, Quantity, _) when Quantity =< 0 -> [];
perms2(L, Quantity, Endtime) when is_list(L), is_integer(Quantity), is_integer(Endtime) -> 
    case listsum(L) of
        X when X < Quantity ->
            []; % no chance of ever reaching our Quantity
        X when X =:= Quantity ->
            [L]; % direct hit!
        _ ->
            {MS, S, _} = erlang:now(),
            if
                MS * 1000000 + S > Endtime ->
                    % we are already late
                    [];
                true ->
                    Lsmall = [X || X <- L, X < Quantity], % remove elements beeing to big
                    Ret = [lists:usort([H|T]) || H <- Lsmall, T <- perms2(Lsmall--[H], Quantity-H, Endtime)]
                    ++
                    % permutations with one element missing
                    [T || H <- Lsmall, T <- perms2(Lsmall--[H], Quantity, Endtime)],
                    lists:usort(Ret)
            end
    end.

choose(L, Quantity, Maxtime) when is_list(L), is_integer(Quantity), is_integer(Maxtime) ->
    {MS, S, _} = erlang:now(),
    Endtime = (MS * 1000000 + S) + Maxtime,
    % TOTO: possible optimisation: shorten series of identical integers so SUM(series) =< Quantity
    % with the current implementation of perms2 the sort isn't actually needed
    lists:usort(fun (A, B) -> length(A) >= length(B) end,
                lists:filter(fun(X) -> listsum(X) =:= Quantity end,
                             perms2(lists:sort(fun(A, B) -> A < B end, L), Quantity, Endtime) 
                             ++ lists:map(fun(X) -> [X] end, L))).
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
%%% This function is used through applications macros (<em>adviserl.hrl</em>) which automaticaly capture <em>Level</em>, <em>Module</em> and <em>Line</em>.
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