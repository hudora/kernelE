{application, mypl,
 [{description,"myPL storage and processing backend"},
  {vsn, "0.01"},
  {modules, [
    mypl,
    mypl_app,
    mypl_sup,
    mypl_web,
    mypl_deps,
    mypl_audit,
    mypl_db,
    mypl_db_util,
    mypl_movements,
    mypl_server,
    mypl_util,
    mypl_requesttracker,
    mypl_abcserver,
    mypl_nveserver,
    mypl_tcp_session,
    generic_tcp_server
    % provpipeline is missing
  ]},
  {registered,[mypl_requesttracker]}, % what's about nveserver?
  {mod, {mypl_app, []}},
  {env, [{listen_host, "0.0.0.0"},
         {listen_port, 1919},
         {minimum_free_floor, 5}]},
  {applications, [kernel, stdlib, crypto, mnesia]}]}.
