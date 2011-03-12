-module(trackerdb).

-export([init/0, announce/7]).

-record(pirate, { id, info_hash, ip, port, peer_id, 
					uploaded, downloaded, left,
					first_seen, last_seen}).


init() ->
    mnesia:create_table(pirate,
			[{attributes, record_info(fields, pirate)}, {type, set}, {index, [info_hash]}]).

announce(InfoHash, Ip, Port, PeerId, Uploaded, Downloaded, Left) ->
	{atomic, Result} = mnesia:transaction(fun() -> 
		AllPeers = mnesia:index_read(pirate, InfoHash, #pirate.info_hash),
		Now = make_timestamp(now),
		PeerUpdate = case mnesia:read(pirate, { InfoHash, Ip, Port }) of
			[Peer = #pirate{ }] -> Peer#pirate { peer_id = PeerId, uploaded = Uploaded,
												 	downloaded = Downloaded, left = Left, last_seen = Now };
			[] -> #pirate{ id = {InfoHash, Ip, Port},
							info_hash = InfoHash, ip = Ip, port = Port, peer_id = PeerId,
							uploaded = Uploaded, downloaded = Downloaded, left = Left,
							first_seen = Now, last_seen = Now }
		end,
		mnesia:write(PeerUpdate),
		
		AvailablePeers = [ { TmpPeerId, TmpIp, TmpPort } ||
							#pirate{ peer_id = TmpPeerId, ip = TmpIp, port = TmpPort } <- AllPeers],
		Complete = 0, Incomplete = 0,
		{ ok, AvailablePeers, Complete, Incomplete }
		end),
	Result.


make_timestamp(now) ->
	{ MegaSeconds, Seconds, MicroSeconds} = erlang:now(),
	(MegaSeconds * 1000000 + Seconds) * 1000000 + MicroSeconds.