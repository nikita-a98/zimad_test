{application, 'zimad_test', [
	{description, "Test task for zimad company"},
	{vsn, "0.0.1"},
	{modules, ['zimad','zimad_app','zimad_auth_token','zimad_sup']},
	{registered, [zimad_test_sup,zimad_sup]},
	{applications, [kernel,stdlib,kernel,stdlib,crypto,public_key,inets,ssl,asn1,mnesia,os_mon,runtime_tools,observer,wx,sasl,tools,common_test,debugger,et,cowboy,jiffy,uuid]},
	{mod, {zimad_app, []}},
	{env, []}
]}.