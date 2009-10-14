%% @version 0.2
%% @author author <author@example.com>
%% @copyright YYYY author.
%% Created :  Created by Maximillian Dornseif on 2007-10-09.

%% @doc TEMPLATE.
-module(mypl).
-author('author <author@example.com>').

%% API
-export([start/0, start_periodic/0, stop/0]).

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
    ensure_started(mnesia),
    mnesia:create_schema([node()]), % TODO may fail, don't care (already exists)?
    Res = application:start(mypl),
    case Res of 
        ok ->
            io:format("~nmyPL/kernel-E started in on ~w with PID ~s.~n", [{os:type(), os:version()}, os:getpid()]),
            io:format("mnesia boot might take a few more minutes.~n"),
            io:format("you might want to call mypl:start_periodic() on a production server~n"),
            io:format("to initiate automatic data archival.~n"),
            ok;
        {error, {already_started, mypl}} ->
            io:format("~nmyPL/kernel-E already started.~n"),
            ok;
        {error, R} ->
            io:format("~nmyPL/kernel-E failed to start: ~p~n",[R]),
            {error, R}
    end.
    

start_periodic() ->
    % every 7 seconds try to transfer audit data from temporary tables to their final destination
    timer:apply_interval(7000,  mypl_audit_transfer, spawn_audit_transfer, []),
    % dump database once a day
    timer:apply_interval(1000*60*60*24,  mypl_db, backup, []),
    % move abc_summary to CouchDB once a day by giving it once a hour the chance to run
    timer:apply_interval(1000*60*59,  mypl_abcserver, spawn_abc_transfer, []),
    io:format("~nPeriodic jobs initialized.~n").
    

%% @spec stop() -> ok
%% @doc Stop the mypl server.
stop() ->
    Res = application:stop(mypl),
    application:stop(crypto),
    Res.
