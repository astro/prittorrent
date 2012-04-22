-module(model_users).

-export([register/3, authenticate/2, activate/1,
	 get_details/1,
	 get_feeds/1, get_feed/2, add_feed/2]).


-define(POOL, pool_users).
-define(Q(Stmt, Params), model_sup:equery(?POOL, Stmt, Params)).

register(Name, Email, Password) ->
    ?Q("INSERT INTO users (\"name\", \"email\", \"password\") VALUES ($1, $2, $3)",
       [Name, Email, Password]).

authenticate(Name, Password) ->
    case ?Q("SELECT \"name\" FROM users WHERE \"name\"=$1 AND \"password\"=$2 AND \"activated\"",
	    [Name, Password]) of
	{ok, _, [_ | _]} ->
	    ok;
	_ ->
	    denied
    end.

activate(Name) ->
    {ok, 1} = ?Q("UPDATE users SET \"activated\"=TRUE WHERE \"name\"=$1",
		 [Name]).



get_details(Name) ->
    case ?Q("SELECT \"title\", \"image\", \"homepage\" FROM users WHERE \"name\"=$1",
	    [Name]) of
	{ok, _, [{Title1, Image, Homepage}]} ->
	    Title2 =
		if
		    is_binary(Title1),
		    size(Title1) > 0 ->
			Title1;
		    true ->
			Name
		end,
	    {ok, Title2, Image, Homepage};
	{ok, _, []} ->
	    {error, not_found}
    end.


get_feeds(Name) ->
    {ok, _, Rows} = ?Q("SELECT \"slug\", \"feed\" FROM user_feeds WHERE \"user\"=$1 ORDER BY LOWER(\"slug\")",
		       [Name]),
    Rows.

get_feed(Name, Slug) ->
    case ?Q("SELECT \"feed\" FROM user_feeds WHERE \"user\"=$1 AND \"slug\"=$2 LIMIT 1",
	    [Name, Slug]) of
	{ok, _, [{Feed}]} ->
	    {ok, Feed};
	{ok, _, []} ->
	    {error, not_found}
    end.
    

add_feed(Name, "http://" ++ _ = Url) ->
    {ok, 1} = ?Q("INSERT INTO user_feeds (\"user\", \"feed\") VALUES ($1, $2)",
		 [Name, Url]);
add_feed(_, _) ->
    exit(invalid_url).
