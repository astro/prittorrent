-module(seeder_listener).

-export([start_link/0, stop/0]).

start_link() ->
    IP = case os:getenv("BIND_IP") of
	     false ->
		 {0, 0, 0, 0, 0, 0, 0, 0};
	     IP1 ->
		 {ok, IP2} = inet_parse:address(IP1),
		 IP2
	 end,
    ranch:start_listener(
      seeder_wire_listener, 32,
      ranch_tcp, [{ip, IP},
                  {port, 6881},
                  {max_connections, 768},
                  %% Not so many but long-lived connections
                  {backlog, 16}
                 ],
      seeder_wire_protocol, []
     ).

stop() ->
    ranch:stop_listener(seeder_wire_listener).
