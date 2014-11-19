{application, emysql,
	[
		{description, "emysql"},
		{vsn, "1.0"},
		{modules, [db]},
		{registered, [emysql_sup, emysql_conn_mgr, db]},
		{applications, [kernel, stdlib, crypto]},
		{mod, {emysql_app, []}},
		{env, [
			{default_timeout, 5000},
			{conn_test_period, 28000}]}
	]
}.