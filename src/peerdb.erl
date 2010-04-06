-module(peerdb).

-export([init/0, add_peers/2, register_peer/4, peer_died/1]).

-record(peer, {info_hash, peer_id, ip, port, pid, seeder = false}).


init() ->
    mnesia:create_table(peer,
			[{attributes, record_info(fields, peer)},
			 {type, bag}]).

%% From tracker response
add_peers(InfoHash, Peers) ->
    {atomic, NewPeers} =
	mnesia:transaction(
	  fun() ->
		  lists:append(
		    [case mnesia:select(peer,
					[{#peer{info_hash = '$1',
						ip = '$2',
						_ = '_'},
					  [{'==', '$1', {const, InfoHash}},
					   {'==', '$2', {const, IP}}],
					  ['$$']}]) of
			 [] ->
			     mnesia:write(#peer{info_hash = InfoHash,
						ip = IP,
						port = Port}),
			     [{IP, Port}];
			 _ ->
			     []
		     end
		     || {IP, Port} <- Peers])
	  end),
    lists:foreach(
      fun({IP, Port}) ->
	      peer_sup:start_peer({InfoHash, IP, Port})
      end, NewPeers).

register_peer(InfoHash, PeerId,
	      IP, Port) ->
    Pid = self(),
    mnesia:transaction(
      fun() ->
	      Peer1 =
		  case mnesia:select(peer,
				     [{#peer{info_hash = '$1',
					     ip = '$2',
					     port = '$3',
					     _ = '_'},
				       [{'==', '$1', {const, InfoHash}},
					{'==', '$2', {const, IP}},
					{'==', '$3', {const, Port}}],
				       ['$$']}]) of
		      [Peer] ->
			  mnesia:delete_object(Peer),
			  Peer;
		      [] ->
			  #peer{info_hash = InfoHash,
				ip = IP,
				port = Port}
		  end,
	      Peer2 =
		  Peer1#peer{peer_id = PeerId,
			     pid = Pid},
	      mnesia:write(Peer2)
      end).

peer_died(InfoHash) when is_binary(InfoHash) ->
    Pid = self(),
    mnesia:transaction(
      fun() ->
	      lists:foreach(
		fun mnesia:delete_object/1,
		mnesia:select(peer,
			      [{#peer{info_hash = '$1',
				      pid = '$2',
				      _ = '_'},
				[{'==', '$1', {const, InfoHash}},
				 {'==', '$2', {const, Pid}}],
				['$$']}]))
      end);
peer_died(_) ->
    ok.
