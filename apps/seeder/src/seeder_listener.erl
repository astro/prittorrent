-module(seeder_listener).

-export([start_link/0]).

start_link() ->
    cowboy:start_listener(
      seeder_wire_listener, 32,
      cowboy_tcp_transport, [{ip, {0, 0, 0, 0, 0, 0, 0, 0}},
			     {port, 6881},
			     {max_connections, 768},
			     %% Not so many but long-lived connections
			     {backlog, 16}
			    ],
      seeder_wire_protocol, []
     ).
