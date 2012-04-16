-module(model_torrents).

-export([add_torrent/2, get_torrent/1]).

-include("../include/model.hrl").

-define(POOL, pool_users).
-define(Q(Stmt, Params), model_sup:equery(?POOL, Stmt, Params)).
-define(T(Fun), model_sup:transaction(?POOL, Fun)).

add_torrent(InfoHash, Torrent) ->
    ?Q("INSERT INTO torrents (\"info_hash\", \"torrent\") VALUES ($1, $2)", [InfoHash, Torrent]).

get_torrent(InfoHash) ->
    case ?Q("SELECT \"torrent\" FROM torrents WHERE \"info_hash\"=$1",
	    [InfoHash]) of
	{ok, _, [{Torrent}]} ->
	    {ok, Torrent};
	{ok, _, []} ->
	    {error, not_found}
    end.
