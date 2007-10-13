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
    {Time , Res} =  timer:tc(application, start, [?APPLICATION, temporary]),
    
    Secs = Time div 1000,
    case Res of 
        ok ->
            io:format("myPL/kernel-E started in ~p ms on ~w with PID ~w~n.",[Secs, 
                      {os:type(), os:version()}, os:getpid()]),
            ok;
        {error, {already_started, ?APPLICATION}} ->
            io:format("myPL/kernel-E already started, ~p ms~n",[ Secs]),
            ok;
        {error, R} ->
            io:format("myPL/kernel-E failed to start, ~p ms: ~p~n",[ Secs, R]),
            {error, R}
    end.