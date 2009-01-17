{application, psql, [{description, "psql $Rev$"},
  {vsn, "0.0.2"},
	{modules, [psql, psql_app, psql_con_sup, psql_connection, psql_lib, psql_logic, psql_pool, psql_protocol, psql_sup ]},
	{registered, [psql_sup]},
	{applications, [kernel, stdlib]},
	{mod, {psql, []}},
	{env, [{erlydb_psql, {"postgresql.local.hudora.biz", 5432, "kernele",
                              "neiMei6cieshah8J", "mypl_archive"}},
    		 {pools, [{erlydb_psql, 1}]}]}]}.
