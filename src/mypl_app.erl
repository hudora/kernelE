%% @version 0.2
%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the mypl application.

-module(mypl_app).
-author('author <author@example.com>').
-behaviour(application).

-export([start/2, stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for mypl.
start(_Type, _StartArgs) ->
    mypl_deps:ensure(),
    % init database tables
    mypl_db:run_me_once(),
    % check database integrity - also warms up caches
    % this gets to slow
    % mypl_integrity:selftest(),
    % start supervisor
    mypl_sup:start_link().
    % fill requestracker at startup
    % mypl_provpipeline:flood_requestracker().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for mypl.
stop(_State) ->
    ok.
