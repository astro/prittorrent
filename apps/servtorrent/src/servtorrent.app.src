{application, servtorrent,
 [{description, "servtorrent"},
  {vsn, "0.0.0"},
  {modules, [backend, peer_sup, servtorrent_sup,
	     wire_listener, backend_file, piecesdb,
	     torrentdb, seedlist, tracker_web,
	     tracker_client, benc, servtorrent,
	     wire_connection
	    ]},
  {registered, [wire_listener, servtorrent_sup]},
  {mod, {servtorrent, []}},
  {env, [{wire_port, 6881},
	 {seedlist, "seeds.xml"},
	 %% LogOptions = [LogOption]
	 %% LogOption = {Target, [Filter]}
	 %% Filter = {Level, Destination}
	 %% Level = debug | info | warn | error | fatal
	 %% Destination = stdout | sasl | Filename
	 {logging, [{control, [{warn, sasl},
			       {info, "control.log"}]},
		    {wire, [{info, sasl}]}
		   ]}
	]},
  {applications, [kernel, stdlib, crypto,
		  sasl, mnesia, inets]}]}.
