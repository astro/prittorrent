%% @author {{author}}
%% @copyright {{year}} {{author}}

%% @doc Web server for tracker.

-module(tracker_web).
-author("The PritTorrent Committee").

-export([start/1, stop/0, loop/1]).

%% External API

start(Options) ->
    mochiweb_http:start([{name, ?MODULE}, {loop, {?MODULE, loop}} | Options]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req) ->
	case {Req:get(method), Req:get(path)} of
	    { 'GET', "/announce" } ->
			Query = Req:parse_qs(),
			io:format("~p~n", [Query]), 
			{ok, Ip} = inet_parse:address(Req:get(peer)),
			InfoHash = list_to_binary(proplists:get_value("info_hash", Query)),
			PeerId = list_to_binary(proplists:get_value("peer_id", Query)),
			Port = list_to_integer(proplists:get_value("port", Query)),
			Uploaded = list_to_integer(proplists:get_value("uploaded", Query, "0")),
			Downloaded = list_to_integer(proplists:get_value("downloaded", Query, "0")),
			Left = list_to_integer(proplists:get_value("left", Query, "0")),
			_Compact = list_to_integer(proplists:get_value("compact", Query, "0")),
			_Crypto = list_to_integer(proplists:get_value("supportcrypto", Query, "0")),
			_Key = list_to_binary(proplists:get_value("key", Query, "")),

			{ ok, AvailablePeers, Complete, Incomplete } = trackerdb:announce(InfoHash, Ip, Port, PeerId, Uploaded, Downloaded, Left),

			Payload = << <<A,B,C,D,P:16/big>> || { _, {A,B,C,D}, P } <- AvailablePeers >>,
			Response = benc:to_binary([{<<"peers">>, Payload}]),

			io:format("~s~n", [Response]),
	    	Req:ok({"text/plain", Response});
	    _ ->
	        Req:respond({501, [{"Content-Type", "text/plain"}], "Malformed request."})
	end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.


get_remote_addr_(Addr) ->
%%io:format("get_remote_addr_(~p)~n", [Addr]),
    case inet_parse:ipv6_address(Addr) of
	{ok, {0, 0, 0, 0, 0, 16#ffff, AB, CD}} ->
	    {AB bsr 8, AB band 16#ff, CD bsr 8, CD band 16#ff};
	{ok, Addr6} ->
	    Addr6
    end.
-define(GET_REMOTE_ADDR, get_remote_addr_(Req:get(peer))).



%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
