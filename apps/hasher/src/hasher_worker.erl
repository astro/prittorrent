-module(hasher_worker).

-export([start_link/0, loop/0]).

start_link() ->
    {ok, spawn_link(fun loop/0)}.

loop() ->
    case model_enclosures:to_hash(1) of
	{ok, URL} ->
	    io:format("To hash: ~p~n", [URL]),
	    hash(URL);
	nothing ->
	    SleepTime = 30 + random:uniform(30),
	    receive
	    after SleepTime * 1000 ->
		    ok
	    end
    end,

    ?MODULE:loop().

hash(URL) ->
    try
	{ok, InfoHash, TorrentFile} =
	    hasher_hash:make_torrent([URL]),
	model_torrents:add_torrent(InfoHash, TorrentFile),
	model_enclosures:set_torrent(URL, <<"">>, InfoHash)
    catch _:Reason ->
	    io:format("Failed hashing ~s~n~p~n", [URL, Reason]),
	    model_enclosures:set_torrent(
	      URL, list_to_binary(io_lib:format("~p", [Reason])), <<"">>)
    end.
