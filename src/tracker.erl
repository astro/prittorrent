-module(tracker).
-export([request/8, peer_list_from_info/1]).

-import(lists, [reverse/1, map/2, keysearch/3, flatten/1]).
-import(regexp, [gsub/3]).

% Normal mode
peer_list_from_info([{"peers", [Peers_1 | _]=Peers, _} | _])
when is_list(Peers_1) ->
    map(
      fun(Peer) ->
	      {value, {"ip", Ip, _}} = keysearch("ip", 1, Peer),
	      {value, {"port", Port, _}} = keysearch("port", 1, Peer),
	      {Ip, Port}
      end,
      Peers);
% Compact mode
peer_list_from_info([{"peers", Peers, _} | _]) ->
    peers_list_from_compact(list_to_binary(Peers), []);
peer_list_from_info([_ | Rest]) ->
    peer_list_from_info(Rest).

peers_list_from_compact(<<>>, Result) ->
    reverse(Result);
peers_list_from_compact(<<H1:8, H2:8, H3:8, H4:8, Port:16/big, Peers/binary>>, Result) ->
    Host = {H1, H2, H3, H4},
    peers_list_from_compact(Peers, [{Host, Port} | Result]).

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
    ],
    Url2 = flatten(io_lib:format("~s?~s", [Url, get_param(Param)])),
    io:format("URL: ~p~n", [Url2]),

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
    {ok, Escaped, _} = regexp:gsub(flatten(io_lib:format("%~2.16B", [C])), " ", "0"),
    url_escape(R ++ Escaped, S);
url_escape(R, Number) when is_integer(Number) ->
    R ++ io_lib:format("~B", [Number]);
url_escape(R, Bin) when is_binary(Bin) ->
    url_escape(R, binary_to_list(Bin)).


