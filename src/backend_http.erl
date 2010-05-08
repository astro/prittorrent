%% Stupid simple synchronous HTTP client
-module(backend_http).

-export([fold_file/5]).

-behaviour(backend).

fold_file(URL, Offset, Length, Fold, AccIn) ->
    logger:log(backend_http, debug,
	       "GET ~s (~p+~p)~n", [URL, Offset, Length]),
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
			 [{stream_to, {self(), once}}], 10000),
    fold_file1(ReqId, Fold, AccIn).

fold_file1(ReqId, Fold, AccIn) ->
    ok = ibrowse:stream_next(ReqId),
    receive
	{ibrowse_async_headers, ReqId, [$2, _, _], _Headers} ->
	    fold_file1(ReqId, Fold, AccIn);
	{ibrowse_async_headers, ReqId, StatusS, _Headers} ->
	    {Status, _} = string:to_integer(StatusS),
	    exit({http, Status});
	{ibrowse_async_response, ReqId, {error, Err}} ->
	    logger:log(backend_http, warn,
		       "HTTP request error: ~p", [Err]),
	    exit(Err);
	{ibrowse_async_response, ReqId, Data} ->
	    AccOut = Fold(list_to_binary(Data), AccIn),
	    fold_file1(ReqId, Fold, AccOut);
	{ibrowse_async_response_end, ReqId} ->
	    AccIn
    end.
