%% TODO: etags/last-modified support
-module(feeds_fetch).

-export([fetch/3]).

-include_lib("exmpp/include/exmpp_xml.hrl").

-define(TIMEOUT, 30 * 1000).
-define(MAX_REDIRECTS, 3).


-spec fetch(string(), string() | undefined, string() | undefined)
       -> {ok, {string(), string()}, xmlel()}.
fetch(Url, Etag1, LastModified1) ->
    Headers =
	if
	    is_binary(Etag1), size(Etag1) > 0 ->
		[{"If-None-Match", binary_to_list(Etag1)}];
	    true ->
		[]
	end ++
	if
	    is_binary(LastModified1), size(LastModified1) > 0 ->
		[{"If-Modified-Since", binary_to_list(LastModified1)}];
	    true ->
		[]
	end,
	    
    Parser = exmpp_xml:start_parser([{max_size, 30 * 1024 * 1024},
					{names_as_atom, false},
					{engine, libxml2}]),
    HttpRes = (catch
		  http_fold(
		    Url, Headers,
		    fun(Els, Chunk) ->
			    case exmpp_xml:parse(Parser, Chunk) of
				continue ->
				    Els;
				Els1 when is_list(Els1) ->
				    Els1 ++ Els
			    end
		    end, [])),
    
    Result =
	case HttpRes of
	    {ok, {Etag2, LastModified2}, Els1} ->
		{Els2, Error} =
		    case (catch exmpp_xml:parse_final(Parser, <<"">>)) of
			done ->
			    {[], <<"Empty document">>};
			Els3 when is_list(Els3) ->
			    {Els3, <<"Empty document">>};
			{xml_parser, _, _Reason, _Details} = Error1 ->
			    {[], Error1}
		    end,
		%% At least one:
		case Els1 ++ Els2 of
		    [#xmlel{} = RootEl | _] ->
			{ok, {Etag2, LastModified2}, RootEl};
		    _ ->
			{error, Error}
		end;
	    not_modified ->
		not_modified;
	    {'EXIT', Reason} ->
		{error, Reason};
	    {xml_parser, _, _Reason, _Details} = Error1 ->
		{error, Error1}
	end,

    ok = exmpp_xml:stop_parser(Parser),
    Result.

%% TODO: handle pcast://
http_fold(URL, ReqHeaders1, F, AccIn) ->
    http_fold(URL, ReqHeaders1, F, AccIn, 0).

http_fold(_, _, _, _, Redirects) when Redirects >= ?MAX_REDIRECTS ->
    exit({http, too_many_redirects});
http_fold(URL, ReqHeaders1, F, AccIn, Redirects) ->
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
	{ok, {{Status, _}, Headers, Pid}} ->
	    %% Finalize this response:
	    http_fold1(Pid, fun(_, _) ->
				    ok
			    end, undefined),

	    case get_header("location", Headers) of
		undefined ->
		    exit({http, Status});
		Location ->
		    io:format("HTTP ~B: ~s redirects to ~s~n", [Status, URL, Location]),
		    http_fold(Location, ReqHeaders1, F, AccIn, Redirects + 1)
	    end;

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
     get_header("etag", Headers),
     get_header("last-modified", Headers)}.

%% expects lower-case Name
get_header(_Name, []) ->
    undefined;
get_header(Name, [{HName, HValue} | Headers]) ->
    case string:to_lower(HName) == Name of
	true ->
	    HValue;
	false ->
	    get_header(Name, Headers)
    end.
