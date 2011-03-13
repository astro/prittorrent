-module(tracker_client).
-export([request/8, peer_list_from_info/1]).

-import(lists, [reverse/1, map/2, keysearch/3, flatten/1]).
-import(regexp, [gsub/3]).

% Normal mode
peer_list_from_info(InfoDict) ->
    Peers4 =
	case lists:keysearch(<<"peers">>, 1, InfoDict) of
	    {value, {_, P4}} when is_list(P4) ->
		lists:map(
		  fun(Peer) ->
			  {value, {"ip", Host, _}} = keysearch("ip", 1, Peer),
			  {ok, IP} = inet_parse:address(Host),
			  {value, {"port", Port, _}} = keysearch("port", 1, Peer),
			  {IP, Port}
		  end, P4);
	    {value, {_, P4}} when is_binary(P4) ->
		peer_list_from_compact4(P4);
	    _ ->
		[]
	end,
    Peers6 =
	case lists:keysearch(<<"peers6">>, 1, InfoDict) of
	    {value, {_, P6}} when is_binary(P6) ->
		peer_list_from_compact6(P6);
	    _ ->
		[]
	end,
    Peers4 ++ Peers6.

peer_list_from_compact4(<<H1:8, H2:8, H3:8, H4:8, Port:16/big,
			  Peers/binary>>) ->
    Host = {H1, H2, H3, H4},
    [{Host, Port} | peer_list_from_compact4(Peers)];
peer_list_from_compact4(<<>>) ->
    [].

peer_list_from_compact6(<<H1:16/big, H2:16/big, H3:16/big, H4:16/big,
			  H5:16/big, H6:16/big, H7:16/big, H8:16/big,
			  Port:16/big,
			  Peers/binary>>) ->
    Host = {H1, H2, H3, H4, H5, H6, H7, H8},
    [{Host, Port} | peer_list_from_compact6(Peers)];
peer_list_from_compact6(<<>>) ->
    [].

% This is one of the announcements done at regular intervals.
is_event(empty)     -> ok;
% When a download first begins.
is_event(started)   -> ok;
% Downloaders send an announcement using stopped when they cease
% downloading.
is_event(stopped)   -> ok;
% Is sent when the download is complete. No completed is sent if the
% file was complete when started.
is_event(completed) -> ok.

request(Url, InfoHash, PeerId, Port, Uploaded, Downloaded, Left, Event) ->
    is_event(Event),

    Param = [
	     {info_hash, InfoHash},
	     {peer_id, PeerId},
	     {port, Port},
	     {uploaded, Uploaded},
	     {downloaded, Downloaded},
	     {left, Left},
	     {compact, 1}
	     | case Event of
		   empty -> [];
		   _ -> [{event, atom_to_list(Event)}]
	       end
    ],
    Url2 = flatten(io_lib:format("~s?~s", [Url, get_param(Param)])),
    logger:log(control, debug,
	       "Tracker request to: ~s", [Url2]),

    {ok,{{_,200,_},_,Body}} = http:request(Url2),
    benc:parse(list_to_binary(Body)).

get_param([]) ->
    "";

get_param([{K,V}|H]) ->
    flatten(get_param(io_lib:format("~s=~s", [K, url_escape(V)]), H)).

get_param(Result, []) ->
    Result;

get_param(Result, [{K,V}|H]) ->
    get_param(Result ++ io_lib:format("&~s=~s", [K, url_escape(V)]), H).

url_escape(S) ->
    url_escape("", S).

url_escape(R, []) ->
    R;
url_escape(R, [C | S]) when C >= $0, C =< $9 ->
    url_escape(R ++ [C], S);
url_escape(R, [C | S]) when C >= $A, C =< $Z ->
    url_escape(R ++ [C], S);
url_escape(R, [C | S]) when C >= $a, C =< $z ->
    url_escape(R ++ [C], S);
url_escape(R, [C | S]) ->
    Escaped = flatten(io_lib:format("%~2.16.0B", [C])),
    url_escape(R ++ Escaped, S);
url_escape(R, Number) when is_integer(Number) ->
    R ++ io_lib:format("~B", [Number]);
url_escape(R, Bin) when is_binary(Bin) ->
    url_escape(R, binary_to_list(Bin)).


