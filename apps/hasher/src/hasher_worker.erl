-module(hasher_worker).

-export([start_link/0, loop/0, hash/1]).

start_link() ->
    {ok, spawn_link(fun init/0)}.


init() ->
    util:seed_random(),
    process_flag(trap_exit, true),
    loop().

loop() ->
    case model_enclosures:to_hash() of
	{ok, URL} ->
	    io:format("To hash: ~p~n", [URL]),
	    hash(URL);
	nothing ->
	    SleepTime = 30 + random:uniform(30),
	    io:format("Nothing to hash, sleeping ~Bs~n", [SleepTime]),
	    receive
		{'EXIT', Pid, Reason} ->
		    error_logger:error_msg("Hasher ~p exited:~n~p~n", [Pid, Reason]);
		{recheck, URL, From} ->
		    io:format("To recheck: ~p~n", [URL]),
		    hash(URL),
		    From ! recheck_done
	    after SleepTime * 1000 ->
		    ok
	    end
    end,

    ?MODULE:loop().

hash(URL) ->
    case (catch hash1(URL)) of
	{'EXIT', Reason} ->
	    error_logger:error_msg("Failed hashing ~s~n~p~n", [URL, Reason]),
	    model_enclosures:set_torrent(
	      URL, list_to_binary(io_lib:format("~p", [Reason])), <<"">>, null, null, null);
	_ ->
	    ok
    end.

hash1(URL) ->
    {ok, InfoHash, Name, Size, TorrentFile, ETag, LastModified} =
	hasher_hash:make_torrent(URL),
    Updater = fun(OldTorrentFile) ->
		      hasher_hash:update_torrent(URL, OldTorrentFile)
	      end,
    model_torrents:add_torrent(InfoHash, Name, Size, TorrentFile, Updater),
    model_enclosures:set_torrent(URL, <<"">>, InfoHash, Size, ETag, LastModified).
