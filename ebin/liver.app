{application, 'liver', [
	{description, "Lightweight Erlang validator based on LIVR specification"},
	{vsn, "1.0.0"},
	{modules, ['liver','liver_bstring','liver_float','liver_livr_rules','liver_maps','liver_rules','liver_strict_rules']},
	{registered, []},
	{applications, [kernel,stdlib]},
	{env, []}
]}.