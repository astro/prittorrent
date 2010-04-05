{application, servtorrent,
 [{description, "servtorrent"},
  {vsn, "0.0.0"},
  {modules, [backend, peer_sup, servtorrent_sup,
	     wire_listener, backend_file, piecesdb,
	     torrentdb, backend_http, seedlist,
	     tracker, benc, servtorrent,
	     wire_connection
	    ]},
  {registered, [wire_listener, servtorrent_sup]},
  {mod, {servtorrent, []}},
  {env, [{wire_port, 6881},
	 {seedlist, "seeds.xml"}]
  },
  {applications, [kernel, stdlib, crypto,
		  sasl, mnesia, inets, ibrowse]}]}.
