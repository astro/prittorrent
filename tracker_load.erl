-module(tracker_load).

-export([run/2]).


worker(Parent, Tracker, InfoHash) ->
    {MS, S, SS} = erlang:now(),
    random:seed(MS, S, SS),
    PeerId = <<"-<30000-",
	       (<< << (random:uniform(255)):8 >> || _ <- lists:seq(1, 12) >>)/binary
	     >>,
    worker(Parent, Tracker, InfoHash, PeerId, 0, 0).

worker(Parent, Tracker, InfoHash, PeerId, Up, Down) ->
    URL = [Tracker,
	   "?info_hash=", cowboy_http:urlencode(InfoHash),
	   "&peer_id=", cowboy_http:urlencode(PeerId),
	   "&port=6881&left=0&uploaded=", integer_to_list(Up),
	   "&downloaded=", integer_to_list(Down)
	  ],

    T1 = util:get_now_us(),
    case lhttpc:request(binary_to_list(list_to_binary(URL)), get, [], 1000) of
	{ok, {{200, _}, _Headers, _Body}} ->
	    T2 = util:get_now_us(),
	    Parent ! {req_done, T2 - T1},
	    ok
    end,
    worker(Parent, Tracker, InfoHash, PeerId, Up + 1, Down + 10).

run(Tracker, InfoHash) ->
    run(Tracker, InfoHash, 1).

run(Tracker, InfoHash, N) ->
    I = self(),
    spawn_link(fun() ->
		       worker(I, Tracker, InfoHash)
	       end),
    io:format("Running ~B clients~n", [N]),
    receive
    after 200 ->
	    {ok, Reqs, Time} = recv_all(),
	    io:format("~B req/s, avg. ~B us~n", [Reqs, trunc(Time)]),
	    run(Tracker, InfoHash, N + 4)
    end.


recv_all() ->
    recv_all(0, 0).

recv_all(Reqs, Time) ->
    receive
	{req_done, T} ->
	    recv_all(Reqs + 1, Time + T)
    after 0 ->
	    {ok, Reqs, Time / Reqs}
    end.
