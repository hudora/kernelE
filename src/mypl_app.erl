%% @version 0.2

-module(mypl_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, Args) ->
    % try to make mnesia start faster
    application:set_env (mnesia, no_table_loaders, 20),
    % init database tables
    mypl_db:run_me_once(),
    % check database integrity - also warms up caches
    % this gets to slow
    % mypl_integrity:selftest(),
    application:start(psql),
    % start supervisor
    mypl_sup:start_link(Args).
    % fill requestracker at startup
    % mypl_provpipeline:flood_requestracker().

stop(_State) ->
    ok.
