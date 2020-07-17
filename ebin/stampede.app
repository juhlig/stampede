{application, 'stampede', [
	{description, "Application resilience testing"},
	{vsn, "0.6.0"},
	{modules, ['stampede','stampede_app','stampede_callbacks','stampede_herd','stampede_herd_sup','stampede_sup','stampede_tracer']},
	{registered, [stampede_sup]},
	{applications, [kernel,stdlib]},
	{mod, {stampede_app, []}},
	{env, []}
]}.