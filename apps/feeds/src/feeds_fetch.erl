%% TODO: etags/last-modified support
-module(feeds_fetch).

-export([fetch/3]).

-include_lib("exmpp/include/exmpp_xml.hrl").

-define(TIMEOUT, 30 * 1000).


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
	    
    Parser = exmpp_xml:start_parser([{max_size, 30 * 1024 * 1024},
					{names_as_atom, false},
					{engine, libxml2}]),
    HttpRes =
	http_fold(Url, Headers,
		  fun(Els, Chunk) ->
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

%% TODO: handle pcast://
http_fold(URL, ReqHeaders1, F, AccIn) ->
    %% Compose request
    ReqHeaders2 =
        [{"User-Agent", "PritTorrent/0.1"}
	 | ReqHeaders1],
    ReqOptions =
	[{partial_download,
	  [
	   %% specifies how many part will be sent to the calling
	   %% process before waiting for an acknowledgement
	   {window_size, 4},
	   %% specifies the size the body parts should come in
	   {part_size, 4096}
	  ]}
	],
    case lhttpc:request(URL, get, ReqHeaders2,
			[], ?TIMEOUT, ReqOptions) of
	%% Ok
	{ok, {{200, _}, Headers, Pid}} ->
            {ok, Etag, LastModified} =
                get_etag_last_modified_from_headers(Headers),
	    %% Strrream:
	    {ok, AccOut } = http_fold1(Pid, F, AccIn),
	    {ok, {Etag, LastModified}, AccOut};
	{ok, {{Status, _}, _Headers, Pid}} ->
	    %% Finalize this response:
	    http_fold1(Pid, F, AccIn),

	    exit({http, Status});

	{error, Reason} ->
	    exit(Reason)
    end.

http_fold1(undefined, _, AccIn) ->
    %% No body, no fold.
    AccIn;
http_fold1(Pid, F, AccIn) ->
    case lhttpc:get_body_part(Pid, ?TIMEOUT) of
	{ok, Data} when is_binary(Data) ->
	    AccOut = F(AccIn, Data),
	    http_fold1(Pid, F, AccOut);
	{ok, {http_eob, _Trailers}} ->
	    {ok, AccIn}
    end.


get_etag_last_modified_from_headers(Headers) ->
    {ok,
     proplists:get_value("ETag", Headers, undefined),
     proplists:get_value("Last-Modified", Headers, undefined)}.
