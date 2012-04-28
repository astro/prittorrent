-module(model_tracker).

-export([scrape/1, get_peers/3, set_peer/7, rm_peer/2]).

-include("../include/model.hrl").

-define(POOL, pool_tracker).
-define(Q(Stmt, Params), model_sup:equery(?POOL, Stmt, Params)).
-define(T(Fun), model_sup:transaction(?POOL, Fun)).


scrape(InfoHash) ->
    {ok, _, [{Leechers, Seeders, Downspeed, Downloaded}]} =
	?Q("SELECT \"t_leechers\", \"t_seeders\", \"t_downspeed\", \"t_downloaded\" FROM scrape_tracker($1)",
	   [InfoHash]),
    {ok, Leechers, Seeders, Downspeed, Downloaded}.

%% List
-spec(get_peers/3 :: (binary(), binary(), true | false)
		     -> {ok, [{binary(), binary(), integer()}]}).
get_peers(InfoHash, ReqPeerId, LeechersOnly) ->
    View = if
	       LeechersOnly -> "tracker_leechers";
	       true -> "tracker"
	   end,
    case ?Q("SELECT \"peer_id\", \"host\", \"port\" FROM " ++
		View ++
		" WHERE \"info_hash\"=$1 AND \"peer_id\"!=$2",
	    [InfoHash, ReqPeerId]) of
	{ok, _, Rows} ->
	    {ok, Rows}
    end.

%% Add/Update
set_peer(InfoHash, Host, Port, PeerId, Uploaded, Downloaded, Left) ->
    %% FIXME: up/down not going out
    {ok, _, [{_Up, _Down}]} = 
	?Q("SELECT \"up\", \"down\" FROM set_peer($1, $2, $3, $4, $5, $6, $7)",
	   [InfoHash, Host, Port, PeerId, Uploaded, Downloaded, Left]),
    %%io:format("set_peer: ~p~n", [{_Up, _Down}]),

    %% TODO: Report back deltas:
    {ok}.

%% Remove
rm_peer(InfoHash, PeerId) ->
    {ok, _} =
	?Q("DELETE FROM tracked WHERE \"info_hash\"=$1 AND \"peer_id\"=$2",
	   [InfoHash, PeerId]),

    %% TODO: Report back deltas:
    {ok}.
