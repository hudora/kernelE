%%%-------------------------------------------------------------------
%%% File    : mypl_util
%%% Author  : Maximillian Dornseif
%%% Description : 
%%%
%%% Created :  Created by Maximillian Dornseif on 2007-10-03.
%%%-------------------------------------------------------------------
-module(mypl_util).

%% API
-export([oid/0, choose/2, choose/3]).

% generate unique object ID
oid() -> 
    {MS,S,US} = erlang:now(),
    lists:flatten([integer_to_list(MS-1190),"-",integer_to_list(S),"-",integer_to_list(US), "-", atom_to_list(node())]).


%% a bunch of functions ot get all permutations of how to select boods from the warehouse.
%% suppose you have 4 crates of 4kg toothpaste, 2 crates of 2kg toothpase, 1 crate of 3 and 4kg.
%% choose/2 gives you possibilities what to ship to the customer if he wants 4kg toothpaste:
%% > test:choose([1,1,1,1,2,2,3,4], 4).
%%  [[1,1,1,1],[1,1,2],[2,2],[1,3],[4]]

%% The whole thing very easyly hits a wall preformance wise:
%% > test:choose([1,1,1,1,2,2,2,3,3,3,4,5,6,7], 5).
%% [[5]]
%% use choose/3 to say how many seconds it should take to find solutions
%% > test:choose([1,1,1,1,2,2,3,3,3,4,5,6,7], 5, 10).
%% [[1,1,1,2],[1,2,2],[1,1,3],[2,3],[1,4],[5]]

%%
listsum(L) ->
    lists:foldl(fun(X, Sum) -> X + Sum end, 0, L).

perms2([], _, _) -> [];
perms2(_, Quantity, _) when Quantity =< 0 -> [];
perms2(L, Quantity, Endtime) -> 
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

choose(L, Quantity, Maxtime) ->
    {MS, S, _} = erlang:now(),
    Endtime = (MS * 1000000 + S) + Maxtime,
    % with the current implementation of perms2 the sort isn't actually needed
    lists:usort(fun (A, B) -> length(A) >= length(B) end,
                lists:filter(fun(X) -> listsum(X) =:= Quantity end,
                             perms2(lists:sort(fun(A, B) -> A < B end, L), Quantity, Endtime) 
                             ++ lists:map(fun(X) -> [X] end, L))).
choose(L, Quantity) ->
    choose(L, Quantity, 3).
