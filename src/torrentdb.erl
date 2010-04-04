-module(torrentdb).

-export([init/0, apply_seedlist/1, add_torrent/2, rm_torrent/1, register_peer/1, inc_uploaded/2, peer_id/0, tracker_loop/2]).

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

register_peer(_InfoHash) ->
    _I = self(),
    %% TODO: chk InfoHash existence, add to supervisor
    ok.

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
		  {_ToUpdate1, ToRemove} =
		      lists:partition(
			fun({TorrentFileOld, _}) ->
				IsIn(TorrentFileOld, NewSeedList)
			end, OldSeedList),
		  {_ToUpdate2, ToAdd} =
		      lists:partition(
			fun({TorrentFileNew, _}) ->
				IsIn(TorrentFileNew, OldSeedList)
			end, NewSeedList),
		  %% ToUpdate = lists:uniq(ToUpdate1 ++ ToUpdate2),
		  %% lists:foreach(fun({TorrentFile, Dir}) ->
		  %% 			InfoHash = case mnesia:read(torrent, 
		  %% 			piecesdb:update_dir_t(InfoHash, Dir)
		  %% 		end, ToUpdate)
		  Removed =
		      lists:map(
			fun({TorrentFile, _}) ->
				%% TODO: doesn't work
				case mnesia:index_read(torrent, TorrentFile, #torrent.torrent_file) of
				    [Torrent] ->
					mnesia:delete_object(Torrent);
				    _ ->
					ignore
				end
			end, ToRemove),
		  {ToAdd, Removed}
	  end),
    lists:foreach(
      fun(#torrent{tl_pid = TLPid}) ->
	      %% TODO: disconnect leechers, don't let them just die
	      exit(TLPid, kill)
      end, Removed),
    
    if
	length(Removed) > 0 ->
	    io:format("Removed ~B torrents~n", [length(Removed)]);
	true -> quiet
    end,
    lists:foreach(fun({TorrentFile, Dir}) ->
			  add_torrent(TorrentFile, Dir),
			  io:format("Started seeding ~s~n", [TorrentFile])
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
    io:format("L1: ~p~n", [L1]),
    lists:foldl(
      fun([InfoHash, TorrentFile], R) ->
	      case piecesdb:get_dir_t(InfoHash) of
		  {ok, Dir} ->
		      [{TorrentFile, Dir} | R];
		  _ ->
		      R
	      end
      end, [], L1).

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
