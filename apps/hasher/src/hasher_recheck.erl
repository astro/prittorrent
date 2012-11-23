-module(hasher_recheck).

-export([start_link/0, loop/0]).


start_link() ->
    {ok, spawn_link(fun loop/0)}.

loop() ->
    case model:to_recheck() of
	nothing ->
	    %% Idle 5..10s
	    Delay = 5000 + random:uniform(5000),
	    receive
	    after Delay ->
		    ok
	    end;
	{ok, URL, Length, ETag, LastModified} ->
