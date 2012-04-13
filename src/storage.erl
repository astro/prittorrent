-module(storage).

%% TODO: relative redirects

-export([make/1, size/1, fold/5]).

-record(storage, {urls :: [{binary(), integer()}]}).

%% URLs for multi-file torrents, not fallback
make(URLs) ->
    set_urls(#storage{urls = []}, URLs).

set_urls(Storage, URLs1) ->
    URLs2 = [case URL of
		 {_, Size} when is_integer(Size) ->
		     URL;
		 _ when is_binary(URL) ->
		     {ok, Size} = resource_size(URL),
		     {URL, Size}
	     end || URL <- URLs1],
    Storage#storage{urls = URLs2}.

size(#storage{urls = URLs}) ->
    lists:foldl(fun({_, Size}, Total) ->
			Total + Size
		end, 0, URLs).

resource_size(URL) when is_binary(URL) ->
    resource_size(binary_to_list(URL));
resource_size(URL) ->
    case ibrowse:send_req(URL, [], head) of
	{ok, "200", Headers, _} ->
	    case extract_header("content-length", Headers) of
		undefined ->
		    undefined;
		SizeS ->
		    Size = list_to_integer(SizeS),
		    {ok, Size}
	    end;
	{ok, [$3, _, _] = StatusS, Headers, _} ->
	    case extract_header("location", Headers) of
		undefined ->
		    exit({http, list_to_integer(StatusS)});
		Location ->
		    %% FIXME: infinite redirects?
		    io:format("HTTP ~s: ~s redirects to ~s~n", [StatusS, URL, Location]),
		    resource_size(Location)
	    end;
	{ok, StatusS, _, _} ->
	    exit({http, list_to_integer(StatusS)});
	{error, Reason} ->
	    exit(Reason)
    end.

fold(_, _, Length, _, AccOut) when Length =< 0 ->
    AccOut;
fold(#storage{urls = URLs} = Storage,
     Offset, Length, F, AccIn) ->
    {URL, Offset1, Length1} =
	lists:foldl(
	  fun({URL, Size}, {look, Offset1}) ->
		  if
		      Offset1 < Size ->
			  {URL, Offset1, min(Length, Size)};
		      true ->
			  {look, Offset1 - Size}
		  end;
	     (_, {URL, Offset1, Length1}) ->
		  {URL, Offset1, Length1}
	  end, {look, Offset}, URLs),

    AccOut = fold_resource(URL, Offset1, Length1, F, AccIn),
    
    fold(Storage, Offset + Length1, Length - Length1, F, AccOut).

fold_resource(URL, Offset, Length, F, AccIn) when is_binary(URL) ->
    fold_resource(binary_to_list(URL), Offset, Length, F, AccIn);
fold_resource(URL, Offset, Length, F, AccIn) ->
    %% Compose request
    logger:log(backend_http, debug,
               "GET ~s (~p+~p)~n", [URL, Offset, Length]),
    io:format("GET ~s (~p+~p)~n", [URL, Offset, Length]),
    Headers =
        if
            is_integer(Offset),
            is_integer(Length) ->
                [{"Range",
                  io_lib:format("bytes=~B-~B",
                                [Offset,
                                 Offset + Length - 1])
                 }];
            true ->
                []
        end ++
        [{"User-Agent", "PritTorrent/0.1"}],
    {ibrowse_req_id, ReqId} =
        ibrowse:send_req(URL, Headers, get, [],
                         [{stream_to, {self(), once}},
			  {response_format, binary}
			 ], 10000),

    %% Await response
    ok = ibrowse:stream_next(ReqId),
    receive
        {ibrowse_async_headers, ReqId, "206", _Headers} ->
	    %% Strrream...
            fold_resource1(ReqId, F, AccIn);
	{ok, [$3, _, _] = StatusS, Headers, _} ->
	    case extract_header("location", Headers) of
		undefined ->
		    exit({http, list_to_integer(StatusS)});
		Location ->
		    io:format("HTTP ~s: ~s redirects to ~s~n", [StatusS, URL, Location]),
		    %% FIXME: infinite redirects?
		    %% FIXME: this breaks Offset & Length for multi-file torrents
		    fold_resource(Location, Offset, Length, F, AccIn)
	    end;
        {ibrowse_async_headers, ReqId, StatusS, _Headers} ->
            {Status, _} = string:to_integer(StatusS),
            exit({http, Status});
        {ibrowse_async_response, ReqId, {error, Err}} ->
            logger:log(backend_http, warn,
                       "HTTP request error: ~p", [Err]),
            exit(Err)
    end.

fold_resource1(ReqId, F, AccIn) ->
    ok = ibrowse:stream_next(ReqId),
    receive
        {ibrowse_async_response, ReqId, {error, Err}} ->
            logger:log(backend_http, warn,
                       "HTTP request error: ~p", [Err]),
            exit(Err);
        {ibrowse_async_response, ReqId, Data} ->
	    AccOut = F(AccIn, Data),
	    fold_resource1(ReqId, F, AccOut);
        {ibrowse_async_response_end, ReqId} ->
	    AccIn
	%% M ->
	%%     io:format("!!! fold_resource1 got ~p~n", [M])
    end.

extract_header(Name1, Headers) ->
    Name2 = string:to_lower(Name1),
    lists:foldl(
      fun({Header, Value}, undefined) ->
	      case string:to_lower(Header) of
		  Name3 when Name2 == Name3 ->
		      Value;
		  _ ->
		      undefined
	      end;
	 (_, Value) ->
	      Value
      end, undefined, Headers).
    
