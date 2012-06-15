-module(model_tracker).

-export([scrape/1, get_peers/3, set_peer/7, rm_peer/4]).

-include("../include/model.hrl").

-define(POOL, pool_tracker).
-define(Q(Stmt, Params), model_sup:equery(?POOL, Stmt, Params)).
-define(T(Fun), model_sup:transaction(?POOL, Fun)).


scrape(InfoHash) ->
    case ?Q("SELECT \"leechers\", \"seeders\", \"downspeed\", COALESCE(\"downloaded\", 0) FROM scraped LEFT JOIN downloaded_stats USING (info_hash) WHERE \"info_hash\"=$1",
	    [InfoHash]) of
	{ok, _, [{Leechers, Seeders, Downspeed, Downloaded}]} ->
	    {ok, Leechers, Seeders, Downspeed, Downloaded};
	{ok, _, []} ->
	    {ok, 0, 0, 0, 0}
    end.

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
		" WHERE \"info_hash\"=$1 AND \"peer_id\"!=$2 LIMIT 40",
	    [InfoHash, ReqPeerId]) of
	{ok, _, Rows} ->
	    {ok, Rows}
    end.

%% Add/Update
set_peer(InfoHash, Host, Port, PeerId, Uploaded, Downloaded, Left) ->
    case ?Q("SELECT \"up\", \"down\" FROM set_peer($1, $2, $3, $4, $5, $6, $7)",
	    [InfoHash, Host, Port, PeerId, Uploaded, Downloaded, Left]) of
	%% FIXME: up/down not going out
	{ok, _, [{_Up, _Down}]} ->
	    %% Success, schedule a scraped update:
	    model_scrape_queue:update_scraped(InfoHash),
	    %% TODO: Report back deltas:
	    ok;
	{error, {error, _, _, Message, _Details}} ->
	    case (catch split_binary(Message, size(Message) - 24)) of
		{_, <<"\"tracked_info_hash_fkey\"">>} ->
		    {error, not_tracked};
		_ ->
		    {error, Message}
	    end
    end.

%% Remove
rm_peer(InfoHash, PeerId, Uploaded, Downloaded) ->
    case ?Q("DELETE FROM tracked WHERE \"info_hash\"=$1 AND \"peer_id\"=$2 RETURNING \"uploaded\", \"downloaded\"",
	    [InfoHash, PeerId]) of
	{ok, 1, _, [{OldUploaded, OldDownloaded}]}
	  when Uploaded >= OldDownloaded,
	       Downloaded >= OldDownloaded ->
	    model_stats:add_counter(up, InfoHash, Uploaded - OldUploaded),
	    model_stats:add_counter(down, InfoHash, Downloaded - OldDownloaded);
	{ok, _, _, _} ->
	    ignore;
	{ok, _} ->
	    ignore
    end.
