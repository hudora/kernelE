%% @version 0.2

-module(mypl_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, Args) ->
    % init database tables
    mypl_db:run_me_once(),
    % check database integrity - also warms up caches
    mypl_integrity:selftest(),
    % start supervisor
    mypl_sup:start_link(Args).
    
stop(_State) ->
    ok.
