-module(model_feeds).

-export([prepare_update/1, write_update/4]).


-define(POOL, pool_users).
-define(Q(Stmt, Params), model_sup:equery(?POOL, Stmt, Params)).

prepare_update(FeedURL) ->
    case ?Q("UPDATE \"feeds\" SET \"last_update\"=CURRENT_TIMESTAMP WHERE \"url\"=$1", [FeedURL]) of
	R ->
	    io:format("updated 1? ~p~n", [R])
    end,

    case ?Q("SELECT \"etag\", \"last_modified\" FROM \"feeds\" WHERE \"url\"=$1", [FeedURL]) of
	{ok, _, [{Etag, LastModified}]} ->
	    {ok, Etag, LastModified};
	{ok, _, _} ->
	    {ok, undefined, undefined}
    end.

write_update(FeedURL, {Etag, LastModified}, Error, Xml) ->
    Stmt = "UPDATE \"feeds\" SET \"last_update\"=CURRENT_TIMESTAMP" ++
	if
	    is_binary(Etag) ->
		", \"etag\"=$2";
	    true ->
		""
	end ++
	if
	    is_list(Etag) ->
		", \"last_modified\"=$3";
	    true ->
		""
	end ++
	if
	    is_list(Etag) ->
		", \"error\"=$4";
	    true ->
		""
	end ++
	if
	    is_list(Etag) ->
		", \"xml\"=$5";
	    true ->
		""
	end ++
	" WHERE \"url\"=$1",
    Params = [FeedURL, Etag, LastModified, Error, Xml],
    ?Q(Stmt, Params).
