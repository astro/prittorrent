-module(model_users).

-export([register/2, get_salted/1, set_salted/2,
	 get_details/1, set_details/4,
	 get_by_email/1,
	 get_feeds/1, get_feed/2,
	 add_feed/3, rm_feed/2,
	 get_user_feed/2, set_user_feed/4]).


-define(POOL, pool_users).
-define(Q(Stmt, Params), model_sup:equery(?POOL, Stmt, Params)).
-define(T(Fun), model_sup:transaction(?POOL, Fun)).

%% TODO: report user exists
register(Name, Email) ->
    ?T(fun(Q) ->
	       case Q("SELECT COUNT(\"name\") FROM users WHERE \"name\"=$1",
		      [Name]) of
		   {ok, _, [{0}]} ->
		       {ok, 1} =
			   Q("INSERT INTO users (\"name\", \"email\", \"salt\") VALUES ($1, $2, $3)",
                             [Name, Email, generate_salt()]),
		       ok;
		   {ok, _, _} ->
		       {error, exists}
	       end
       end).

generate_salt() ->
    util:seed_random(),

    << <<(random:uniform(256) - 1):8>>
       || _ <- lists:seq(1, 8) >>.

get_salted(Name) ->
    case ?Q("SELECT \"salted\", \"salt\" FROM users WHERE \"name\"=$1", [Name]) of
	{ok, _, [{Salted, Salt}]} ->
	    {ok, Salted, Salt};
	{ok, _, []} ->
	    {error, not_found}
    end.

set_salted(Name, Salted) ->
    {ok, 1} =
	?Q("UPDATE users SET \"salted\"=$2 WHERE \"name\"=$1", [Name, Salted]).

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

set_details(UserName, Title, Image, Homepage) ->
    {ok, 1} =
	?Q("UPDATE users SET \"title\"=$2, \"image\"=$3, \"homepage\"=$4 WHERE \"name\"=$1",
	   [UserName, Title, Image, Homepage]).

get_by_email(Email) ->
    {ok, _, Rows} =
	?Q("SELECT \"name\" FROM users WHERE \"email\"=$1", [Email]),
    [Name || {Name} <- Rows].


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

add_feed(Name, Slug, Url) ->
    case ?Q("SELECT * FROM add_user_feed($1, $2, $3)",
	    [Name, Slug, Url]) of
	{ok, _, [{IsNew}]} ->
	    {ok, IsNew};
	{error, Reason} ->
	    {error, Reason}
    end.

rm_feed(Name, Slug) ->
    {ok, _} =
	?Q("DELETE FROM user_feeds WHERE \"user\"=$1 AND \"slug\"=$2",
	   [Name, Slug]),
    %% TODO: remove unused from feeds
    ok.

get_user_feed(UserName, Slug) ->
    case ?Q("SELECT \"feed\", \"public\", \"title\" FROM user_feeds WHERE \"user\"=$1 AND \"slug\"=$2 LIMIT 1",
	    [UserName, Slug]) of
	{ok, _, [{Feed, Public, Title}]} ->
	    {ok, Feed, Public, Title};
	{ok, _, []} ->
	    {error, not_found}
    end.

set_user_feed(UserName, Slug, Public, Title) ->
    {ok, 1} =
	?Q("UPDATE user_feeds SET \"public\"=$3, \"title\"=$4 WHERE \"user\"=$1 AND \"slug\"=$2",
	   [UserName, Slug, Public, Title]).
