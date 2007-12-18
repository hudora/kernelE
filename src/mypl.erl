%% @version 0.2
%%%-------------------------------------------------------------------
%%% File    : mypl
%%% Author  : Maximillian Dornseif
%%% Description : 
%%%
%%% Created :  Created by Maximillian Dornseif on 2007-10-09.
%%%-------------------------------------------------------------------
-module(mypl).

-define(APPLICATION, mypl_kernel).

%% API
-export([start/0]).

start() ->
    Res = application:start(?APPLICATION, temporary),
    case Res of 
        ok ->
            io:format("~nmyPL/kernel-E started in on ~w with PID ~s.~n", [{os:type(), os:version()}, os:getpid()]),
            ok;
        {error, {already_started, ?APPLICATION}} ->
            io:format("~nmyPL/kernel-E already started.~n"),
            ok;
        {error, R} ->
            io:format("~nmyPL/kernel-E failed to start: ~p~n",[R]),
            {error, R}
    end.