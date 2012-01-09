-module(feeds_fetch).

-export([fetch/1]).


fetch(Url) ->
    Parser = exmpp_xml:start_parser([{max_size, 10 * 1024 * 1024},
					{names_as_atom, false},
					{engine, expat}]),
    http_fold(Url, fun(Chunk, _) ->
			   R1 = exmpp_xml:parse(Parser, Chunk),
			   io:format("Parsed: ~p~n",[R1])
		   end, undefined),
    exmpp_xml:parse_final(Parser, <<"">>),
    R = exmpp_xml:stop_parser(Parser),
    io:format("Downloaded ~s - ~p~n",[Url,R]),
    receive
	M -> io:format("M=~p~n",[M])
    after 1000 ->
	    ignore
    end,
    ok.

http_fold(Url, Fold, AccIn) ->
    Headers =
	[{"User-Agent", "PritTorrent/0.1"}],
    {ibrowse_req_id, ReqId} =
	ibrowse:send_req(Url, Headers, get, [],
			 [{stream_to, {self(), once}}], 10000),
    http_fold1(ReqId, Fold, AccIn).

http_fold1(ReqId, Fold, AccIn) ->
    ok = ibrowse:stream_next(ReqId),
    receive
	{ibrowse_async_headers, ReqId, [$2, _, _], _Headers} ->
	    http_fold1(ReqId, Fold, AccIn);
	{ibrowse_async_headers, ReqId, StatusS, _Headers} ->
	    {Status, _} = string:to_integer(StatusS),
	    exit({http, Status});
	{ibrowse_async_response, ReqId, {error, Err}} ->
	    exit(Err);
	{ibrowse_async_response, ReqId, Data} ->
	    AccOut = Fold(list_to_binary(Data), AccIn),
	    http_fold1(ReqId, Fold, AccOut);
	{ibrowse_async_response_end, ReqId} ->
	    AccIn
    end.
