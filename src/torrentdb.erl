-module(torrentdb).

-export([init/0, apply_seedlist/1, add_torrent/2, rm_torrent/1, inc_uploaded/2, get_torrent_file/1, peer_id/0, tracker_loop/3]).

-record(torrent, {info_hash,
		  torrent_file,
		  tl_pid,  %% tracker loop
		  uploaded = 0
		 }).

init() ->
    mnesia:create_table(torrent,
			[{attributes, record_info(fields, torrent)}]),
    mnesia:add_table_index(torrent, torrent_file),
    case application:get_env(servtorrent, peer_id) of
	{ok, _} -> fine;
	undefined ->
	    application:set_env(servtorrent, peer_id, generate_peer_id())
    end.

apply_seedlist(NewSeedList) ->
    {atomic, {ToAdd, Removed}} =
	mnesia:transaction(
	  fun() ->
		  IsIn = fun(TorrentFile1, SeedList) ->
				 lists:any(
				   fun({TorrentFile2, _}) ->
					   TorrentFile1 == TorrentFile2
				   end, SeedList)
			 end,
		  OldSeedList = seedlist_t(),
		  {ToUpdate1, ToRemove} =
		      lists:partition(
			fun({TorrentFileOld, _}) ->
				IsIn(TorrentFileOld, NewSeedList)
			end, OldSeedList),
		  {ToUpdate2, ToAdd} =
		      lists:partition(
			fun({TorrentFileNew, _}) ->
				IsIn(TorrentFileNew, OldSeedList)
			end, NewSeedList),
		  ToUpdate = uniq_list(ToUpdate1 ++ ToUpdate2),
		  lists:foreach(
		    fun({TorrentFile, Dir}) ->
			    case mnesia:index_read(torrent, TorrentFile, #torrent.torrent_file) of
				[#torrent{info_hash = InfoHash}] ->
				    piecesdb:set_dir_t(InfoHash, Dir);
				_ ->
				    ignore
			    end
		    end, ToUpdate),
		  Removed =
		      lists:map(
			fun({TorrentFile, _}) ->
				%% TODO: doesn't work
				case mnesia:index_read(torrent, TorrentFile, #torrent.torrent_file) of
				    [Torrent] ->
					mnesia:delete_object(Torrent),
					[Torrent];
				    _ ->
					[]
				end
			end, ToRemove),
		  {ToAdd, lists:append(Removed)}
	  end),
    lists:foreach(
      fun(#torrent{tl_pid = TLPid}) ->
	      %% TODO: disconnect leechers, don't let them just die
	      exit(TLPid, kill)
      end, Removed),
    
    if
	length(Removed) > 0 ->
	    logger:log(control, warn,
		       "Removed ~B torrents", [length(Removed)]);
	true -> quiet
    end,
    lists:foreach(
      fun({TorrentFile, Dir}) ->
	      logger:log(control, debug,
			 "Adding torrent ~s (~s)",
			 [TorrentFile, Dir]),
	      case (catch add_torrent(TorrentFile, Dir)) of
		  {'EXIT', Reason} ->
		      logger:log(control, error,
				 "Cannot start seeding ~s~: ~p",
				 [TorrentFile, Reason]);
		  _ ->
		      logger:log(control, info,
				 "Started seeding ~s", [TorrentFile])
		  end
      end, ToAdd),
    ok.

seedlist_t() ->
    L1 =
	mnesia:select(torrent,
		      [{#torrent{info_hash = '$1',
				 torrent_file = '$2',
				 _ = '_'},
			[],
			[['$1', '$2']]
		       }]),
    lists:foldl(
      fun([InfoHash, TorrentFile], R) ->
	      case piecesdb:get_dir_t(InfoHash) of
		  {ok, Dir} ->
		      [{TorrentFile, Dir} | R];
		  _ ->
		      R
	      end
      end, [], L1).

-define(MAX_TORRENT_SIZE, 1024 * 1024).

add_torrent(TorrentFile, Dir) ->
    Torrent = backend:read_file(TorrentFile),
    if size(Torrent) > ?MAX_TORRENT_SIZE -> exit(torrent_file_too_big);
       true -> ok
    end,
    Parsed = benc:parse(Torrent),
    {value, {_, InfoDict}} =
	lists:keysearch(<<"info">>, 1, Parsed),
    InfoHash = benc:hash(InfoDict),
    {value, {_, PieceLength}} =
	lists:keysearch(<<"piece length">>, 1, InfoDict),
    Files = info_files(InfoDict),
    {value, {_, AnnounceUrl}} =
	lists:keysearch(<<"announce">>, 1, Parsed),
    %% TODO: make supervised
    TLPid = spawn_link(
	      fun() ->
		      receive
			  go ->
			      tracker_loop(AnnounceUrl, InfoHash, 0);
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
	    PathList = string:tokens(binary_to_list(Name), "/"),
	    Path = string:join(normalize_path(PathList), "/"),
	    [{Path, Length}];
	{_, _, {value, {_, FileList}}} ->
	    lists:map(
	      fun(FileDict) ->
		      {value, {_, PathList}} =
			  lists:keysearch(<<"path">>, 1, FileDict),
		      {value, {_, Length}} =
			  lists:keysearch(<<"length">>, 1, FileDict),
		      Path = string:join([binary_to_list(P)
					  || P <- PathList], "/"),
		      {normalize_path(Path), Length}
	      end, FileList)
    end.

normalize_path([]) ->
    exit(empty_path);
normalize_path(PathList) ->
    normalize_path(PathList, []).

normalize_path([<<".">> | PathList], R) ->
    normalize_path(PathList, R);
normalize_path([<<"..">> | PathList], [_Parent | R]) ->
    normalize_path(PathList, R);
normalize_path([<<"..">> | PathList], []) ->
    normalize_path(PathList, []);
normalize_path([Dir | PathList], R) ->
    normalize_path(PathList, [Dir | R]);
normalize_path([], R) ->
    lists:reverse(R).

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
		    fun(#torrent{info_hash = InfoHash,
				 tl_pid = TLPid} = Torrent) ->
			    mnesia:delete_object(Torrent),
			    piecesdb:rm_t(InfoHash),
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

get_torrent_file(InfoHash) ->
    {atomic, R} =
	mnesia:transaction(
	  fun() ->
		  case mnesia:read(torrent, InfoHash) of
		      [#torrent{torrent_file = TorrentFile}] ->
			  {ok, TorrentFile};
		      _ ->
			  not_found
		  end
	  end),
    R.
    

peer_id() ->
    {ok, MyPeerId} = application:get_env(servtorrent, peer_id),
    {ok, MyPeerId}.

%% http://www.bittorrent.org/beps/bep_0020.html
generate_peer_id() ->
    list_to_binary("-PR0001-" ++
		       [random:uniform(256) - 1
			|| _ <- lists:seq(1, 12)]).

tracker_loop(AnnounceUrl, InfoHash, Counter) ->
    Interval =
	try
	    Event = case Counter of
			0 -> started;
			_ -> empty
		    end,
	    {ok, Interval1} = tracker_request(AnnounceUrl, InfoHash, Event),
	    Interval1
	catch
	    exit:Reason ->
		logger:log(control, error,
			   "Tracker request to ~s crashed: ~p",
			   [AnnounceUrl, Reason]),
		%% Backup interval:
		600 + random:uniform(300)
	end,
    %% Wait the interval before next tracker request
    receive
    after Interval * 1000 ->
	    ?MODULE:tracker_loop(AnnounceUrl, InfoHash)
    end.

tracker_request(AnnounceUrl, InfoHash, Event) ->
    {ok, Port} = wire_listener:get_port(),
    Uploaded = case mnesia:dirty_read(torrent, InfoHash) of
		   [#torrent{uploaded = U}] -> U;
		   _ -> 0
	       end,
    {ok, PeerId} = peer_id(),
    Response = tracker_client:request(AnnounceUrl, InfoHash, PeerId, Port,
			       Uploaded, 0, 0, Event),
    {value, {_, Interval}} = lists:keysearch(<<"interval">>, 1, Response),
    case (catch connect_by_tracker_response(InfoHash, Response)) of
	{'EXIT', Reason} ->
	    logger:log(wire, fatal,
		       "Cannot connect_by_tracker_response(~p, ~p): ~p",
		       [InfoHash, Response, Reason]);
	_ -> ok
    end,
    {ok, Interval}.

connect_by_tracker_response(InfoHash, Response) ->
    Peers = tracker_client:peer_list_from_info(Response),
    peerdb:add_peers(InfoHash, Peers).

uniq_list([]) ->
    [];
uniq_list([E | L]) ->
    case lists:member(E, L) of
	false -> [E | uniq_list(L)];
	true -> uniq_list(L)
    end.
