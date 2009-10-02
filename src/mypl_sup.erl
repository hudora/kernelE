%% @version 0.2
%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Supervisor for the mypl application.
%% This module supervises the system, checks if all processes are running and restarts them
%% if needed. Refer to OTP Design Principles / Supervisor Behaviour for further enlightenment.

-module(mypl_sup).
-author('author <author@example.com>').
-behaviour(supervisor).             % see erl -man supervisor

%% External exports
-export([start/0, start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

start() ->
    spawn(fun() ->
            supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = [])
        end).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.
    
    
%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    % which way for default environment is preferrable?
    Ip = case os:getenv("MOCHIWEB_IP") of false -> "0.0.0.0"; Any -> Any end,
    {ok, Host} = application:get_env(listen_host),
    {ok, Port} = application:get_env(listen_port),
    
    WebConfig = [
     {ip, Ip},
             {port, 8000},
             {docroot, mypl_deps:local_path(["priv", "www"])}],
    
    Web = {mypl_web,
           {mypl_web, start, [WebConfig]},
           permanent, 10000, worker, dynamic},
    Api = {tag2, 
           {mypl_server, start_link, []},
           permanent, 10000, worker, [mypl_server]},
    Abc = {tag3, 
           {mypl_abcserver, start_link, []},
           permanent, 10000, worker, [mypl_abcserver]},
    Nve = {tag4, 
           {mypl_nveserver, start_link, []},
           permanent, 10000, worker, [mypl_nveserver]},
    Net = {tag5,
           {generic_tcp_server, start_link, 
            [mypl_tcp_session, Host, Port, [list, {active, false}, {packet, line}, {reuseaddr, true}], []]
           },
           permanent, 10000, worker, [generic_tcp_server]},
    Trk = {tag1, 
            {mypl_requesttracker, start_link, []},
            permanent, 10000, worker, [mypl_requesttracker]},
    Processes = [Web, Api, Abc, Nve, Net, Trk],
    {ok, {{one_for_one, 30, 5}, Processes}}.
