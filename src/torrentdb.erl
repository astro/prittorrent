-module(torrentdb).

-export([init/0, seedlist/0, add_torrent/2, rm_torrent/1, register_peer/1, inc_uploaded/2, peer_id/0, tracker_loop/2]).

-record(torrent, {info_hash,
		  torrent_file,
		  tl_pid,  %% tracker loop
		  uploaded = 0
		 }).

init() ->
    mnesia:create_table(torrent,
			[{attributes, record_info(fields, torrent)}]),
    case application:get_env(servtorrent, peer_id) of
	{ok, _} -> fine;
	undefined ->
	    application:set_env(servtorrent, peer_id, generate_peer_id())
    end.

register_peer(_InfoHash) ->
    _I = self(),
    %% TODO: chk InfoHash existence, add to supervisor
    ok.

seedlist() ->
    {atomic, L} =
	mnesia:transaction(
	  fun() ->
		  mnesia:select(torrent,
				[{#torrent{torrent_file = '$1',
					   _ = '_'},
				  [],
				  ['$1']
				 }])
	  end),
    {ok, L}.

add_torrent(TorrentFile, Dir) ->
    Parsed = benc:parse_file(TorrentFile),
    {value, {_, InfoDict}} =
	lists:keysearch(<<"info">>, 1, Parsed),
    InfoHash = benc:hash(InfoDict),
    {value, {_, PieceLength}} =
	lists:keysearch(<<"piece length">>, 1, InfoDict),
    Files = info_files(InfoDict),
    {value, {_, AnnounceUrl}} =
	lists:keysearch(<<"announce">>, 1, Parsed),
    TLPid = spawn_link(
	      fun() ->
		      receive
			  go ->
			      tracker_loop(AnnounceUrl, InfoHash);
			  _ ->
			      ok
		      end
	      end),
    {atomic, _} =
	mnesia:transaction(
	  fun() ->
		  mnesia:write(#torrent{info_hash = InfoHash,
					torrent_file = TorrentFile,
					tl_pid = TLPid
				       }),
		  piecesdb:add_t(InfoHash, Dir,
				 PieceLength, Files)
	  end),
    TLPid ! go,
    {ok, InfoHash, TLPid}.

info_files(InfoDict) ->
    case {lists:keysearch(<<"name">>, 1, InfoDict),
	  lists:keysearch(<<"length">>, 1, InfoDict),
	  lists:keysearch(<<"files">>, 1, InfoDict)} of
	{{value, {_, Name}},
	 {value, {_, Length}},
	 _} ->
	    [{Name, Length}];
	{_, _, {value, {_, FileList}}} ->
	    lists:map(
	      fun(FileDict) ->
		      {value, {_, PathList}} =
			  lists:keysearch(<<"path">>, 1, FileDict),
		      {value, {_, Length}} =
			  lists:keysearch(<<"length">>, 1, FileDict),
		      Path = string:join([binary_to_list(P)
					  || P <- PathList], "/"),
		      {Path, Length}
	      end, FileList)
    end.

rm_torrent(TorrentFile) ->
    {atomic, TLPids} =
	mnesia:transaction(
	  fun() ->
		  Torrents =
		      mnesia:select(torrent,
				    [{#torrent{torrent_file = '$1',
					       _ = '_'},
				      [{'=:=', '$1',
					{const, TorrentFile}}],
				      ['$_']}]),
		  lists:map(
		    fun(#torrent{tl_pid = TLPid} = Torrent) ->
			    mnesia:delete_object(Torrent),
			    TLPid
		    end, Torrents)
	  end),
    lists:foreach(
      fun(TLPid) ->
	      exit(TLPid, normal)
      end, TLPids).

inc_uploaded(InfoHash, Delta) ->
    {atomic, _} =
	mnesia:transaction(
	  fun() ->
		  case mnesia:read(torrent, InfoHash) of
		      [#torrent{uploaded = Uploaded} = Torrent] ->
			  mnesia:write(Torrent#torrent{uploaded = Uploaded + Delta});
		      _ ->
			  ignore
		  end
	  end).

peer_id() ->
    {ok, MyPeerId} = application:get_env(servtorrent, peer_id),
    {ok, MyPeerId}.

%% http://www.bittorrent.org/beps/bep_0020.html
generate_peer_id() ->
    list_to_binary("-CR0000-" ++
		       [random:uniform(256) - 1
			|| _ <- lists:seq(1, 12)]).

tracker_loop(AnnounceUrl, InfoHash) ->
    Interval =
	case (catch tracker_request(AnnounceUrl, InfoHash)) of
	    {'EXIT', Reason} ->
		io:format("Tracker request to ~s crashed: ~p~n",
			  [AnnounceUrl, Reason]),
		600 + random:uniform(300);
	    {ok, Interval1} ->
		Interval1
	end,
    receive
    after Interval * 1000 ->
	    ?MODULE:tracker_loop(AnnounceUrl, InfoHash)
    end.

tracker_request(AnnounceUrl, InfoHash) ->
    {ok, Port} = wire_listener:get_port(),
    Uploaded = case mnesia:dirty_read(torrent, InfoHash) of
		   [#torrent{uploaded = U}] -> U;
		   _ -> 0
	       end,
    {ok, PeerId} = peer_id(),
    Response = tracker:request(AnnounceUrl, InfoHash, PeerId, Port,
			       Uploaded, 0, 0, empty),
    {value, {_, Interval}} = lists:keysearch(<<"interval">>, 1, Response),
    {ok, Interval}.
