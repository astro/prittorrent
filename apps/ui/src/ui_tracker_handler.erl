-module(ui_tracker_handler).
-export([init/3, handle/2, terminate/2]).

-behaviour(cowboy_http_handler).

tracker_interval() ->
    540 + random:uniform(60).

init({tcp, http}, Req, _Opts) ->
    {MS, S, SS} = erlang:now(),
    PS = lists:sum(pid_to_list(self())),
    random:seed(MS + PS, S, SS),

    {ok, Req, undefined_state}.

handle(Req, _State) ->
    T1 = util:get_now_us(),
    %%io:format("Tracker: ~p~n", [Req]),

    %% HTTP parameters
    %% FIXME: use cowboy_http_req:peer_addr/1 when using a frontend proxy
    {{Host, _}, _} = cowboy_http_req:peer(Req),
    {Method, _} = cowboy_http_req:method(Req),
    {Path, _} = cowboy_http_req:path(Req),

    Reply =
	case (catch handle1(Req, Host, Method, Path)) of
	    {ok, Dict} ->
		Dict;
	    {'EXIT', Reason} ->
		io:format("Error handling ~s ~p:~n~p~n", [Method, Path, Reason]),
		[{<<"failure">>, <<"Internal server error">>}]
	end,
    %%io:format("Tracker Reply: ~p~n", [Reply]),
    
    Body = benc:to_binary(Reply),
    {ok, Req2} =
	cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"application/x-bittorrent">>}],
			      Body, Req),

    T2 = util:get_now_us(),
    io:format("[~.1fms] ui_tracker_handler ~s ~p~n", [(T2 - T1) / 1000, Method, Path]),
    
    {ok, Req2, undefined_state}.

handle1(Req, {0, 0, 0, 0, 0, 16#ffff, AB, CD}, Method, Path) ->
    %% Handle bindv6only=0:
    A = AB bsr 8,
    B = AB band 16#ff,
    C = CD bsr 8,
    D = CD band 16#ff,
    handle1(Req, {A, B, C, D}, Method, Path);

handle1(Req, Host, Method, Path) ->
    %% Tracker parameters
    {InfoHash, _} = cowboy_http_req:qs_val(<<"info_hash">>, Req),
    {PeerId, _} = cowboy_http_req:qs_val(<<"peer_id">>, Req),
    {Port, _} = cowboy_http_req:qs_val(<<"port">>, Req),
    {Uploaded, _} = cowboy_http_req:qs_val(<<"uploaded">>, Req),
    {Downloaded, _} = cowboy_http_req:qs_val(<<"downloaded">>, Req),
    {Left, _} = cowboy_http_req:qs_val(<<"left">>, Req),
    {Event, _} = cowboy_http_req:qs_val(<<"event">>, Req),
    {Compact, _} = cowboy_http_req:qs_val(<<"compact">>, Req),
    %% TODO: numwant w/ checks
    io:format("Tracker request: info_hash=~p~n\thost=~p port=~p peer_id=~p~n\tevent=~p uploaded=~p downloaded=~p left=~p~n", [InfoHash, Host, Port, PeerId, Event, Uploaded, Downloaded, Left]),

    handle2(Method, Path, InfoHash,
	    host_to_binary(Host), binary_to_integer_or(Port, undefined), PeerId,
	    Event,
	    binary_to_integer_or(Uploaded, 0), binary_to_integer_or(Downloaded, 0),
	    binary_to_integer_or(Left, 0), Compact).

handle2('GET', [<<"announce">>], <<InfoHash:20/binary>>, 
	Host, Port, <<PeerId:20/binary>>, 
	Event, Uploaded, Downloaded, Left, Compact)
  when is_integer(Port),
       is_integer(Uploaded),
       is_integer(Downloaded),
       is_integer(Left) ->
    IsSeeder = case Left of
		   0 -> true;
		   _ -> false
	       end,
    {ok, MySeeders} = application:get_env(ui, seeders),
    {ok, TrackerPeers} = model_tracker:get_peers(InfoHash, PeerId, IsSeeder),
    Peers =
	if
	    IsSeeder ->
		[];
	    true ->
		io:format("Prepending MySeeders=~p~n", [MySeeders]),
		[{peer_id:generate(), host_to_binary(PeerHost), PeerPort}
		 || {PeerHost, PeerPort} <- MySeeders]
	end ++ TrackerPeers,
    %% scrape info:
    %% We return the numbers without the client added for the first time.
    {ok, Leechers, Seeders, _Downspeed, _Downloaded} =
	model_tracker:scrape(InfoHash),

    %% Continue write part in background:
    spawn_set_peer(InfoHash, 
	Host, Port, PeerId,
	Event, Uploaded, Downloaded, Left),

    io:format("Tracker returns ~B+~B peers, leechers=~B seeders=~B~n",
	      [length(TrackerPeers),
	       if IsSeeder -> 0; true -> length(MySeeders) end,
	       Leechers, Seeders]),
    {PeersValue, Peers6Value} =
	case Compact of
	    <<"1">> ->
		{<< <<PeerHost/binary, PeerPort:16>>
		    || {_, <<PeerHost:4/binary>>, PeerPort} <- Peers >>,
		 << <<PeerHost/binary, PeerPort:16>>
		    || {_, <<PeerHost:16/binary>>, PeerPort} <- Peers >>};
	    _ ->
		{[[{<<"id">>, PeerPeerId},
		   {<<"ip">>, PeerHost},
		   {<<"port">>, PeerPort}]
		  || {PeerPeerId, <<PeerHost:4/binary>>, PeerPort} <- Peers],
		 [[{<<"id">>, PeerPeerId},
		   {<<"ip">>, PeerHost},
		   {<<"port">>, PeerPort}]
		  || {PeerPeerId, <<PeerHost:16/binary>>, PeerPort} <- Peers]}
	end,

    {ok, [{<<"interval">>, tracker_interval()},
	  {<<"peers">>, PeersValue},
	  {<<"peers6">>, Peers6Value},
	  {<<"incomplete">>, Leechers},
	  {<<"complete">>, Seeders + 1}
	 ]};

handle2('GET', [<<"scrape">>], <<InfoHash:20/binary>>,
	_Host, _port, _PeerId,
	_Event, _Uploaded, _Downloaded, _Left, _Compact) ->
    {ok, Leechers, Seeders, _Downspeed, Downloaded} =
	model_tracker:scrape(InfoHash),
    %% TODO: implement `name' if available from query
    {ok, [{<<"files">>,
	   [{InfoHash, [{<<"interval">>, tracker_interval()},
			{<<"incomplete">>, Leechers},
			{<<"complete">>, Seeders + 1},
			{<<"downloaded">>, Downloaded}
		       ]}
	   ]}
	 ]};

handle2(_Method, _Path, _InfoHash,
	_Host, _Port, _PeerId,
	_Event, _Uploaded, _Downloaded, _Left, _Compact) ->
    exit(invalid_request).

terminate(_Req, _State) ->
    ok.



spawn_set_peer(InfoHash, 
	       Host, Port, PeerId,
	       Event, Uploaded, Downloaded, Left) ->
    spawn(
      fun() ->
	      T1 = util:get_now_us(),
	      case (catch set_peer(InfoHash, 
				   Host, Port, PeerId,
				   Event, Uploaded, Downloaded, Left)) of
		  {'EXIT', Reason} ->
		      io:format("Failed set_peer: ~p ~p ~p ~p ~p ~p ~p ~p ~n~p~n",
				[InfoHash, 
				 Host, Port, PeerId,
				 Event, Uploaded, Downloaded, Left,
				 Reason]);
		  _ ->
		      T2 = util:get_now_us(),
		      io:format("[~.1fms] set_peer~n", [(T2 - T1) / 1000]),
		      ok
	      end
      end).

set_peer(InfoHash, 
	 Host, Port, PeerId,
	 Event, Uploaded, Downloaded, Left) ->
    %% TODO: count "completed" for stats
    case Event of
	<<"stopped">> ->
	    model_tracker:rm_peer(InfoHash, PeerId, Uploaded, Downloaded);
	_ ->
	    case Event of
		<<"completed">> ->
		    model_stats:add_counter(complete, InfoHash, 1);
		_ ->
		    ok
	    end,

	    model_tracker:set_peer(InfoHash, Host, Port, PeerId,
				   Uploaded, Downloaded, Left)
    end.


binary_to_integer_or(<<Bin/binary>>, _) ->
    list_to_integer(binary_to_list(Bin));
binary_to_integer_or(_, Default) ->
    Default.


host_to_binary({A, B, C, D}) ->
    <<A:8, B:8, C:8, D:8>>;
host_to_binary({A, B, C, D, E, F, G, H}) ->
    <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>.
