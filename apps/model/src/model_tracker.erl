-module(model_tracker).

-export([get_peers/3, add_peer/7]).

-include("../include/model.hrl").

-define(POOL, pool_users).
-define(Q(Stmt, Params), model_sup:equery(?POOL, Stmt, Params)).
-define(T(Fun), model_sup:transaction(?POOL, Fun)).


get_peers(InfoHash, ReqPeerId, LeechersOnly) ->
    SQL1 = "SELECT \"peer_id\", \"host\", \"port\" FROM tracked WHERE \"info_hash\"=$1 AND \"peer_id\"!=$2",
    SQL2 = case LeechersOnly of
	       true ->
		   SQL1 ++ " AND \"left\">0";
	       false ->
		   SQL1
	   end,
    SQL3 = SQL2 ++ " ORDER BY \"left\" ASC, \"last_request\" DESC LIMIT 60",
    case ?Q(SQL3,
	    [InfoHash, ReqPeerId]) of
	{ok, _, Rows} ->
	    {ok, Rows}
    end.

add_peer(InfoHash, Host, Port, PeerId, Uploaded, Downloaded, Left) ->
    

    %% Report back deltas:
    {ok}.
