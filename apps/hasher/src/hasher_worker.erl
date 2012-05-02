-module(hasher_worker).

-export([start_link/0, loop/0, hash/1]).

start_link() ->
    {ok, spawn_link(fun loop/0)}.

loop() ->
    case model_enclosures:to_hash() of
	{ok, URL} ->
	    io:format("To hash: ~p~n", [URL]),
	    hash(URL);
	nothing ->
	    SleepTime = 30 + random:uniform(30),
	    io:format("Nothing to hash, sleeping ~Bs~n", [SleepTime]),
	    receive
	    after SleepTime * 1000 ->
		    ok
	    end
    end,

    ?MODULE:loop().

hash(URL) ->
    case (catch hash1(URL)) of
	{'EXIT', Reason} ->
	    io:format("Failed hashing ~s~n~p~n", [URL, Reason]),
	    model_enclosures:set_torrent(
	      URL, list_to_binary(io_lib:format("~p", [Reason])), <<"">>);
	_ ->
	    ok
    end.

hash1(URL) ->
    {ok, InfoHash, Name, Size, TorrentFile} =
	hasher_hash:make_torrent([URL]),
    model_torrents:add_torrent(InfoHash, Name, Size, TorrentFile),
    model_enclosures:set_torrent(URL, <<"">>, InfoHash).
