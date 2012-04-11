%% TODO: etags/last-modified support
-module(feeds_fetch).

-export([fetch/3]).

-include_lib("exmpp/include/exmpp_xml.hrl").


-spec(fetch/3 :: (string(), string() | undefined, string() | undefined) -> {ok, {string(), string()}, xmlel()}).
fetch(Url, Etag1, LastModified1) ->
    Headers =
	if
	    is_binary(Etag1) ->
		[{"If-None-Match", binary_to_list(Etag1)}];
	    true ->
		[]
	end ++
	if
	    is_binary(LastModified1) ->
		[{"If-Modified-Since", binary_to_list(LastModified1)}];
	    true ->
		[]
	end,
    io:format("EL: ~p~nH: ~p~n",[{Etag1, LastModified1}, Headers]),

	    
    Parser = exmpp_xml:start_parser([{max_size, 30 * 1024 * 1024},
					{names_as_atom, false},
					{engine, expat}]),
    HttpRes =
	http_fold(Url, Headers,
		  fun(Chunk, Els) ->
			  case exmpp_xml:parse(Parser, Chunk) of
			      continue ->
				  Els;
			      Els1 when is_list(Els1) ->
				  Els1 ++ Els
			  end
		  end, []),

    Result =
	case HttpRes of
	    {ok, {Etag2, LastModified2}, Els1} ->
		Els2 =
		    case exmpp_xml:parse_final(Parser, <<"">>) of
			done ->
			    [];
			Els3 when is_list(Els3) ->
			    Els3
		    end,
		%% At least one:
		[RootEl | _] = Els1 ++ Els2,
		{ok, {Etag2, LastModified2}, RootEl};
	    not_modified ->
		not_modified;
	    E ->
		E
	end,

    ok = exmpp_xml:stop_parser(Parser),
    Result.

%% TODO: use storage, handle pcast://
http_fold(Url, Headers, Fold, AccIn) ->
    Headers1 =
	[{"User-Agent", "PritTorrent/0.1"}
	 | Headers],
    {ibrowse_req_id, ReqId} =
	ibrowse:send_req(Url, Headers1, get, [],
			 [{stream_to, {self(), once}}], 10000),
    http_fold1(ReqId, Fold, AccIn).

http_fold1(ReqId, Fold, AccIn) ->
    ok = ibrowse:stream_next(ReqId),
    receive
	{ibrowse_async_headers, ReqId, [$2, _, _], Headers} ->
	    {ok, Etag, LastModified} =
		get_etag_last_modified_from_headers(Headers),
	    case http_fold2(ReqId, Fold, AccIn) of
		{ok, AccOut} ->
		    {ok, {Etag, LastModified}, AccOut};
		{error, Reason} ->
		    {error, Reason}
	    end;
	{ibrowse_async_headers, ReqId, "304", _Headers} ->
	    not_modified;
	{ibrowse_async_headers, ReqId, StatusS, _Headers} ->
	    {Status, _} = string:to_integer(StatusS),
	    {error, {http, Status}};
	{ibrowse_async_response, ReqId, {error, Err}} ->
	    {error, Err}
    end.

http_fold2(ReqId, Fold, AccIn) ->
    ok = ibrowse:stream_next(ReqId),
    receive
	{ibrowse_async_response, ReqId, Data} ->
	    AccOut = Fold(list_to_binary(Data), AccIn),
	    http_fold2(ReqId, Fold, AccOut);
	{ibrowse_async_response_end, ReqId} ->
	    {ok, AccIn};
	{ibrowse_async_response, ReqId, {error, Err}} ->
	    {error, Err}
    end.

get_etag_last_modified_from_headers(Headers) ->
    {ok,
     proplists:get_value("ETag", Headers, undefined),
     proplists:get_value("Last-Modified", Headers, undefined)}.
