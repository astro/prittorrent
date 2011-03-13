-module(trackerdb).

-export([init/0, announce/7, unix_seconds_since_epoch/0, remove_peers_with_timeout_in_seconds/1]).

-include_lib("stdlib/include/qlc.hrl").

-record(pirate, { id, info_hash, ip, port, peer_id, 
					uploaded, downloaded, left,
					first_seen, last_seen}).


init() ->
    mnesia:create_table(pirate,
			[{attributes, record_info(fields, pirate)}, {type, set}, {index, [info_hash]}]).

announce(InfoHash, Ip, Port, PeerId, Uploaded, Downloaded, Left) ->
	PrimaryPeerKey = { InfoHash, Ip, Port },
	{atomic, Result} = mnesia:transaction(fun() -> 
		AllPeers = mnesia:index_read(pirate, InfoHash, #pirate.info_hash),
		Now = unix_seconds_since_epoch(),
		PeerUpdate = case mnesia:read(pirate, PrimaryPeerKey) of
			[Peer = #pirate{ }] -> Peer#pirate { peer_id = PeerId, uploaded = Uploaded,
												 	downloaded = Downloaded, left = Left, last_seen = Now };
			[] -> #pirate{ id = PrimaryPeerKey,
							info_hash = InfoHash, ip = Ip, port = Port, peer_id = PeerId,
							uploaded = Uploaded, downloaded = Downloaded, left = Left,
							first_seen = Now, last_seen = Now }
		end,
		mnesia:write(PeerUpdate),
		
		AvailablePeers = [ { TmpPeerId, TmpIp, TmpPort } ||
							Peer = #pirate{ peer_id = TmpPeerId, ip = TmpIp, port = TmpPort } <- AllPeers, Peer#pirate.id =/= PrimaryPeerKey],
		Complete = 0, Incomplete = 0,
		{ ok, AvailablePeers, Complete, Incomplete }
		end),
	Result.

remove_peers_with_timeout_in_seconds(Seconds) ->
	KillTime = unix_seconds_since_epoch() - Seconds, % all pirates iwth an update date of this and below need to go away
	Q = qlc:q([Pirate || Pirate <- mnesia:table(pirate), Pirate#pirate.last_seen < KillTime]),
	F = fun () ->
		PiratesToKill = qlc:e(Q),
		lists:foreach( fun(Pirate) -> mnesia:delete_object(Pirate) end, PiratesToKill),
		io:format("Killing Pirates: ~p \n",[PiratesToKill]) % should not do this in a transaction…
	end,
	graceful_transaction(F).

graceful_transaction(F) ->
	case mnesia:transaction(F) of
		{atomic, Result} ->
			Result;
		{aborted, Reason} ->
			io:format("transaction abort: ~p~n",[Reason]),
			[]
	end.	


% find(Q) ->
% 	F = fun() ->
% 			qlc:e(Q)
% 	end,
% 	graceful_transaction(F).
% 
% read_all(Table) ->
% 	Q = qlc:q([X || X <- mnesia:table(Table)]),
% 	graceful_transaction(Q). 


unix_seconds_since_epoch() ->
    LocalDateTime = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    UnixEpoch = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    LocalDateTime - UnixEpoch.


% make_timestamp(now) ->
% 	{ MegaSeconds, Seconds, MicroSeconds} = erlang:now(),
% 	(MegaSeconds * 1000000 + Seconds) * 1000000 + MicroSeconds.