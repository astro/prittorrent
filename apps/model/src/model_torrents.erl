-module(model_torrents).

-export([add_torrent/4, get_torrent/1, get_info/1, exists/1, calculate_names_sizes/0]).

-include("../include/model.hrl").

-define(POOL, pool_torrents).
-define(Q(Stmt, Params), model_sup:equery(?POOL, Stmt, Params)).
-define(T(Fun), model_sup:transaction(?POOL, Fun)).

add_torrent(InfoHash, Name, Size, Torrent) ->
    ?Q("INSERT INTO torrents (\"info_hash\", \"name\", \"size\", \"torrent\") VALUES ($1, $2, $3, $4)",
       [InfoHash, Name, Size, Torrent]).

get_torrent(InfoHash) ->
    case ?Q("SELECT \"name\", \"torrent\" FROM torrents WHERE \"info_hash\"=$1",
	    [InfoHash]) of
	{ok, _, [{Name, Torrent}]} ->
	    {ok, Name, Torrent};
	{ok, _, []} ->
	    {error, not_found}
    end.

get_info(InfoHash) ->
    case ?Q("SELECT \"name\", \"size\" FROM torrents WHERE \"info_hash\"=$1",
	    [InfoHash]) of
	{ok, _, [{Name, Size}]} ->
	    {ok, Name, Size};
	_ ->
	    {error, not_found}
    end.

exists(<<InfoHash:20/binary>>) ->
    case ?Q("SELECT TRUE FROM torrents WHERE \"info_hash\"=$1",
	    [InfoHash]) of
	{ok, _, [{true}]} ->
	    true;
	{ok, _, _} ->
	    false
    end;
exists(_) ->
    false.


%% Maintenance
calculate_names_sizes() ->
    ?T(fun(Q) ->
	       {ok, _, Torrents} =
		   Q("SELECT \"info_hash\", \"torrent\" FROM torrents WHERE \"name\" IS NULL OR \"size\" IS NULL", []),
	       lists:foreach(
		 fun({InfoHash, TorrentFile}) ->
			 Torrent = benc:parse(TorrentFile),
			 Info = proplists:get_value(<<"info">>, Torrent),
			 Name = proplists:get_value(<<"name">>, Info),
			 Size = proplists:get_value(<<"length">>, Info),
			 Q("UPDATE torrents SET \"name\"=$2, \"size\"=$3 WHERE \"info_hash\"=$1",
			   [InfoHash, Name, Size])
		 end, Torrents)
       end).
