-module(hasher_recheck).

-export([start_link/0, loop/0]).


start_link() ->
    {ok, spawn_link(fun loop/0)}.

%% Perform just HEAD requests, and if needed recheck async.
loop() ->
    case model_enclosures:to_recheck() of
	nothing ->
	    %% Idle 5..10s
	    Delay = 5000 + random:uniform(5000),
	    receive
	    after Delay ->
		    ok
	    end;
	{ok, URL, Length, ETag, LastModified} ->
	    io:format("Recheck ~s~n", [URL]),
	    ETagList = if
			   is_binary(ETag) -> binary_to_list(ETag);
			   true -> ETag
		       end,
	    LastModifiedList = if
				   is_binary(LastModified) -> binary_to_list(LastModified);
				   true -> LastModified
			       end,
	    case storage:resource_info(URL) of
		{ok, _Type, ContentLength, ContentETag, ContentLastModified}
		  when (Length == ContentLength andalso
                        (ContentETag == ETagList orelse
			 ContentETag == undefined orelse
			 ETagList == null) andalso
                        (ContentLastModified == LastModifiedList orelse
			 ContentLastModified == undefined orelse
			 LastModifiedList == null)
		       ) ->
		    nothing_changed;

		{ok, _Type, ContentLength, ContentETag, ContentLastModified} ->
		    io:format("Need recheck: ~s~n", [URL]),
		    io:format("\tLength: ~p /= ~p~n", [Length, ContentLength]),
		    io:format("\tETag: ~p /= ~p~n", [ETagList, ContentETag]),
		    io:format("\tLast-Modified: ~p /= ~p~n", [LastModifiedList, ContentLastModified]),
		    hasher_sup:recheck(URL);

		{error, {http, HttpStatus}} ->
		    io:format("Recheck ~s resulted in HTTP ~p~n", [URL, HttpStatus]),

                    %% We've got an HTTP code, meaning the network is
                    %% up but the URL has disappeared!
                    model_enclosures:set_torrent(
                      URL, list_to_binary(io_lib:format("HTTP ~B", [HttpStatus])), <<"">>, null, null, null);

		{error, {nxdomain, _Reason}} ->
		    io:format("Recheck ~s failed on DNS~n", [URL]),

                    %% We've got an HTTP code, meaning the network is
                    %% up but the URL has disappeared!
                    model_enclosures:set_torrent(
                      URL, <<"NXDOMAIN">>, <<"">>, null, null, null);

		{error, E} ->
		    io:format("Recheck ~s failed:~n~p~n", [URL, E])

	    end
    end,
    
    ?MODULE:loop().
	  
