%% @version 0.2
%% @author author <author@example.com>
%% @copyright YYYY author.
%% Created :  Created by Maximillian Dornseif on 2007-10-09.

%% @doc TEMPLATE.
-module(mypl).
-author('author <author@example.com>').

%% API
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
    

start() ->
    mypl_deps:ensure(),
    mnesia:create_schema([node()]),
    % try to make mnesia start faster
    application:set_env (mnesia, no_table_loaders, 20),
    ensure_started(crypto),
    ensure_started(psql),
    ensure_started(mnesia),
    Res = application:start(mypl),
    case Res of 
        ok ->
            io:format("~nmyPL/kernel-E started in on ~w with PID ~s.~n", [{os:type(), os:version()}, os:getpid()]),
            ok;
        {error, {already_started, mypl}} ->
            io:format("~nmyPL/kernel-E already started.~n"),
            ok;
        {error, R} ->
            io:format("~nmyPL/kernel-E failed to start: ~p~n",[R]),
            {error, R}
    end.
    

%% @spec stop() -> ok
%% @doc Stop the mypl server.
stop() ->
    Res = application:stop(mypl),
    application:stop(crypto),
    Res.
