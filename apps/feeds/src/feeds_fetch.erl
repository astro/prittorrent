-module(feeds_fetch).

-export([fetch/1]).


fetch(Url) ->
    Headers =
	[{"User-Agent", "PritTorrent/0.1"}],
    {ibrowse_req_id, ReqId} =
	ibrowse:send_req(Url, Headers, get, [],
			 [{stream_to, {self(), once}}], 10000),

    ok = ibrowse:stream_next(ReqId),
    receive
	{ibrowse_async_headers, ReqId, [$2, _, _], _Headers} ->
	    ok;
	{ibrowse_async_headers, ReqId, StatusS, _Headers} ->
	    {Status, _} = string:to_integer(StatusS),
	    exit({http, Status});
	{ibrowse_async_response, ReqId, {error, Err1}} ->
	    exit(Err1)
    end,

    Continuation =
	fun(Tail, _) ->
		ok = ibrowse:stream_next(ReqId),
		receive
		    {ibrowse_async_response, ReqId, Data} ->
			Bin = list_to_binary(Data),
			{<<Tail/binary, Bin/binary>>, ok};
		    {ibrowse_async_response_end, ReqId} ->
			{Tail, eof};
		    {ibrowse_async_response, ReqId, {error, Err2}} ->
			exit(Err2)
		end
	end,

    {ok, Result, _TrailingBytes} = 
	erlsom:parse_sax(<<>>, state, fun sax_callback/2, 
			 [{continuation_function, Continuation, state}]),
    {ok, Result}.

sax_callback(startDocument, _) ->
    {false, []};
sax_callback({startElement, _, "title", _, _}, {false, Titles}) ->
    {"", Titles};
sax_callback({characters, C}, {Title, Titles}) when is_list(Title) ->
    {Title ++ C, Titles};
sax_callback({endElement, _, "title", _}, {Title, Titles}) when is_list(Title) ->
    {false, [Title | Titles]};
sax_callback(endDocument, State) ->
    case State of
	{false, Titles} ->
	    lists:reverse(Titles);
	_ ->
	    []
    end;
sax_callback(_Event, State) ->
    State.
