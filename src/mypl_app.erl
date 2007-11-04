-module(mypl_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, Args) ->
    mypl_db:run_me_once(),
    mypl_integrity:selftest(),
    mypl_sup:start_link(Args).
    
stop(_State) ->
    ok.
