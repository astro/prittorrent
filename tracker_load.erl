-module(tracker_load).

-export([run/2]).


worker(Tracker, InfoHash) ->
    {MS, S, SS} = erlang:now(),
    random:seed(MS, S, SS),
    PeerId = <<"-<30000-",
	       (<< << (random:uniform(255)):8 >> || _ <- lists:seq(1, 12) >>)/binary
	     >>,
    worker(Tracker, InfoHash, PeerId, 0, 0).

worker(Tracker, InfoHash, PeerId, Up, Down) ->
    URL = [Tracker,
	   "?info_hash=", cowboy_http:urlencode(InfoHash),
	   "&peer_id=", cowboy_http:urlencode(PeerId),
	   "&port=6881&left=0&uploaded=", integer_to_list(Up),
	   "&downloaded=", integer_to_list(Down)
	  ],

    case lhttpc:request(binary_to_list(list_to_binary(URL)), get, [], 1000) of
	{ok, {{200, _}, _Headers, _Body}} ->
	    ok
    end,
    worker(Tracker, InfoHash, PeerId, Up + 1, Down + 10).

run(Tracker, InfoHash) ->
    run(Tracker, InfoHash, 1).

run(Tracker, InfoHash, N) ->
    spawn_link(fun() ->
		       worker(Tracker, InfoHash)
	       end),
    io:format("Running ~B clients~n", [N]),
    receive
    after 1000 ->
	    run(Tracker, InfoHash, N + 1)
    end.
