-module(storage).

%% TODO: relative redirects

-export([make/1, size/1, fold/5, resource_size/1]).

-define(TIMEOUT, 30 * 1000).
-define(PART_SIZE, 32768).

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
    case lhttpc:request(URL, head, [], ?TIMEOUT) of
	{ok, {{200, _}, Headers, _}} ->
	    case extract_header("content-length", Headers) of
		undefined ->
		    undefined;
		SizeS ->
		    Size = list_to_integer(SizeS),
		    {ok, Size}
	    end;
	{ok, {{Status, _}, Headers, _}}
	 when Status >= 300, Status < 400 ->
	    case extract_header("location", Headers) of
		undefined ->
		    exit({http, Status});
		Location ->
		    %% FIXME: infinite redirects?
		    io:format("HTTP ~B: ~s redirects to ~s~n", [Status, URL, Location]),
		    resource_size(Location)
	    end;
	{ok, {{Status, _}, _, _}} ->
	    io:format("~B ~s~n", [Status, URL]),
	    exit({http, Status});
	{error, Reason} ->
	    io:format("~s ~p~n", [URL, Reason]),
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

%% FIXME: what if response chunk is smaller than requested? retry in
%% case it's still uploading?
fold_resource(URL, Offset, Length, F, AccIn) when is_binary(URL) ->
    fold_resource(binary_to_list(URL), Offset, Length, F, AccIn);
fold_resource(URL, Offset, Length, F, AccIn) ->
    %% Compose request
    ReqHeaders =
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
    ReqOptions =
	[{partial_download,
	  [
	   %% specifies how many part will be sent to the calling
	   %% process before waiting for an acknowledgement
	   {window_size, 4},
	   %% specifies the size the body parts should come in
	   {part_size, ?PART_SIZE}
	  ]}
	],
    case lhttpc:request(URL, get, ReqHeaders,
			[], ?TIMEOUT, ReqOptions) of
	%% Partial Content
	{ok, {{206, _}, _Headers, Pid}} ->
	    %% Strrream:
	    fold_resource1(Pid, F, AccIn);
	{ok, {{Status, _}, Headers, Pid}}
	  when Status >= 300, Status < 400 ->
	    %% Finalize this response:
	    fold_resource1(Pid, fun(_, _) ->
					ok
				end, undefined),

	    case extract_header("location", Headers) of
		undefined ->
		    exit({http, Status});
		Location ->
		    io:format("HTTP ~B: ~s redirects to ~s~n", [Status, URL, Location]),
		    %% FIXME: infinite redirects?
		    %% FIXME: this breaks Offset & Length for multi-file torrents
		    fold_resource(Location, Offset, Length, F, AccIn)
	    end;
	{ok, {{Status, _}, _Headers, Pid}} ->
	    %% Finalize this response:
	    fold_resource1(Pid, fun(_, _) ->
					ok
				end, undefined),

	    exit({http, Status});

	{error, Reason} ->
	    exit(Reason)
    end.

fold_resource1(undefined, _, AccIn) ->
    %% No body, no fold.
    AccIn;
fold_resource1(Pid, F, AccIn) ->
    case (catch lhttpc:get_body_part(Pid, ?TIMEOUT)) of
	{ok, Data} when is_binary(Data) ->
	    AccOut = F(AccIn, Data),
	    fold_resource1(Pid, F, AccOut);
	{ok, {http_eob, _Trailers}} ->
	    AccIn;
	{'EXIT', Reason} ->
	    io:format("storage fold interrupted: ~p~n", [Reason]),
	    AccIn
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
    
