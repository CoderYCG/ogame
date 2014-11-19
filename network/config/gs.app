{
	application, gs,
	[
		{description, "server"},
		{vsn, "1.0"},
		{modules, [gs]},
		{registered, [gs_app]},
		{applications, [kernel, stdlib, sasl]},
		{mod, {gs_app, []}},
		{start_phases, []}
	]
}.