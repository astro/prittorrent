-module(model_enclosures).

-export([to_hash/1, set_torrent/3]).

-include("../include/model.hrl").

-define(POOL, pool_users).
-define(Q(Stmt, Params), model_sup:equery(?POOL, Stmt, Params)).
-define(T(Fun), model_sup:transaction(?POOL, Fun)).

to_hash(Limit) ->
    ?T(fun(Q) ->
	       case Q("SELECT \"url\" FROM enclosures_to_hash LIMIT $1", [Limit]) of
		   {ok, _, [{URL}]} ->
		       case Q("SELECT count(\"url\") FROM enclosure_torrents WHERE \"url\"=$1", [URL]) of
			   {ok, _, [{0}]} ->
			       ?Q("INSERT INTO enclosure_torrents (\"url\", \"last_update\") VALUES ($1, CURRENT_TIMESTAMP)", [URL]);
			   {ok, _, [{1}]} ->
			       ?Q("UPDATE enclosure_torrents SET \"last_update\"=CURRENT_TIMESTAMP WHERE \"url\"=$1", [URL])
		       end,
		       {ok, URL};
		   _ ->
		       nothing
	       end
       end).

set_torrent(URL, Error, InfoHash) ->
    ?T(fun(Q) ->
	       case Q("SELECT count(\"url\") FROM enclosure_torrents WHERE \"url\"=$1", [URL]) of
		   {ok, _, [{0}]} ->
		       ?Q("INSERT INTO enclosure_torrents (\"url\", \"last_update\", \"info_hash\", \"error\") VALUES ($1, CURRENT_TIMESTAMP, $2, $3)", [URL, InfoHash, Error]);
		   {ok, _, [{1}]} ->
		       ?Q("UPDATE enclosure_torrents SET \"last_update\"=CURRENT_TIMESTAMP, \"info_hash\"=$2, \"error\"=$3 WHERE \"url\"=$1", [URL, InfoHash, Error])
	       end
       end).
