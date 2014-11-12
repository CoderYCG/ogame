{
	application, log,
	[
		{description, "log"},
		{vsn, "1.0"},
		{modules, [log]},
		{registered, [log_app]},
		{applications, [kernel, stdlib, sasl]},
		{mod, {log_app, []}},
		{start_phases, []}
	]
}.