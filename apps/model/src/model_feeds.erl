-module(model_feeds).

-export([to_update/1, prepare_update/1, write_update/8,
	 feed_details/1, feed_items/1, user_items/1]).

-include("../include/model.hrl").

-define(POOL, pool_users).
-define(Q(Stmt, Params), model_sup:equery(?POOL, Stmt, Params)).
-define(T(Fun), model_sup:transaction(?POOL, Fun)).

to_update(MaxAge1) ->
    MaxAge2 = {{0,0,MaxAge1},0,0},
    case ?Q("SELECT next_url, wait FROM feed_to_update($1)",
	    [MaxAge2]) of
	{ok, _, [{NextURL, {{H, M, S}, Days, Months}}]} ->
	    Wait = S + 60 * (M + (60 * (H + 24 * (Days + 30 * Months)))),
	    {ok, {NextURL, Wait}};
	{ok, _, _} ->
	    %% Nothing in database? Wait like 10s...
	    {<<"">>, 10}
    end.

prepare_update(FeedURL) ->
    case ?Q("UPDATE \"feeds\" SET \"last_update\"=CURRENT_TIMESTAMP WHERE \"url\"=$1", [FeedURL]) of
	{ok, 1} ->
	    ok;
	{ok, N} ->
	    exit({n_feeds, N})
    end,

    case ?Q("SELECT \"etag\", \"last_modified\" FROM \"feeds\" WHERE \"url\"=$1", [FeedURL]) of
	{ok, _, [{Etag, LastModified}]} ->
	    {ok, Etag, LastModified};
	{ok, _, _} ->
	    {ok, undefined, undefined}
    end.

%% TODO: transaction
-spec(write_update/8 :: (string(),
			 {binary() | null, binary() | null},
			 binary() | null,
			 binary() | null,
			 binary() | null,
			 binary() | null,
			 binary() | null,
			 [#feed_item{}])
			-> ok).
write_update(FeedURL, {Etag, LastModified},
	     Error, Xml, Title, Homepage, Image, Items) when is_list(Etag) ->
    write_update(FeedURL, {list_to_binary(Etag), LastModified},
		 Error, Xml, Title, Homepage, Image, Items);
write_update(FeedURL, {Etag, LastModified},
	     Error, Xml, Title, Homepage, Image, Items) when is_list(LastModified) ->
    write_update(FeedURL, {Etag, list_to_binary(LastModified)},
		 Error, Xml, Title, Homepage, Image, Items);
%% TODO: don't drop xml on error!
write_update(FeedURL, {Etag, LastModified}, Error, Xml, Title, Homepage, Image, Items) ->
    ?T(fun(Q) ->
	       %% Update feed entry
	       Stmt = "UPDATE \"feeds\" SET \"last_update\"=CURRENT_TIMESTAMP, \"etag\"=$2, \"last_modified\"=$3, \"error\"=$4, \"xml\"=$5, \"title\"=$6, \"homepage\"=$7, \"image\"=$8 WHERE \"url\"=$1",
	       Params = [FeedURL,
			 enforce_string(Etag), enforce_string(LastModified), 
			 enforce_string(Error), enforce_string(Xml),
			 enforce_string(Title), enforce_string(Homepage),
			 enforce_string(Image)],
	       case Q(Stmt, Params) of
		   {ok, 1} ->
		       ok;	   
		   {ok, N} ->
		       exit({n_feeds, N})
	       end,

	       %% Update items
	       lists:foreach(
		 fun(#feed_item{} = Item) ->
			 case Q("SELECT count(\"id\") FROM \"feed_items\" WHERE \"feed\"=$1 AND \"id\"=$2",
				[FeedURL, Item#feed_item.id]) of
			     {ok, _, [{0}]} ->
				 io:format("New feed item:~n~p~n", [Item#feed_item.title]),
				 lists:foreach(fun(Enclosure) ->
						       io:format("  e ~s~n", [Enclosure])
					       end, Item#feed_item.enclosures),
				 {ok, 1} =
				     Q("INSERT INTO \"feed_items\" (\"feed\", \"id\", \"title\", \"published\", \"homepage\", \"payment\", \"image\", \"xml\", \"updated\") VALUES ($1, $2, $3, ($4::text)::timestamp, $5, $6, $7, $8, CURRENT_TIMESTAMP)",
				       [FeedURL, Item#feed_item.id,
					Item#feed_item.title, Item#feed_item.published, 
					enforce_string(Item#feed_item.homepage),
					enforce_string(Item#feed_item.payment),
					enforce_string(Item#feed_item.image),
					Item#feed_item.xml]);
			     {ok, _, [{1}]} ->
				 {ok, 1} =
				     Q("UPDATE \"feed_items\" SET \"title\"=$3, \"homepage\"=$4, \"payment\"=$5, \"image\"=$6, \"xml\"=$7, \"updated\"=CURRENT_TIMESTAMP WHERE \"feed\"=$1 AND \"id\"=$2",
				       [FeedURL, Item#feed_item.id,
					Item#feed_item.title,
					enforce_string(Item#feed_item.homepage), 
					enforce_string(Item#feed_item.payment), 
					enforce_string(Item#feed_item.image), 
					Item#feed_item.xml])
			 end,
			 %% Update enclosures
			 Q("DELETE FROM \"enclosures\" WHERE \"feed\"=$1 AND \"item\"=$2",
			   [FeedURL, Item#feed_item.id]),
			 lists:foreach(
			   fun(Enclosure) ->
				   Q("INSERT INTO \"enclosures\" (\"feed\", \"item\", \"url\") VALUES ($1, $2, $3)",
				     [FeedURL, Item#feed_item.id, Enclosure])
			   end, Item#feed_item.enclosures)
		 end, Items),

	       ok
       end).


feed_details(FeedURL) ->
    case ?Q("SELECT \"title\", \"homepage\", \"image\" FROM feeds WHERE \"url\"=$1",
	    [FeedURL]) of
	{ok, _, [{Title, Homepage, Image}]} ->
	    {ok, Title, Homepage, Image};
	{ok, _, []} ->
	    {error, not_found}
    end.


-spec(feed_items/1 :: (string()) -> [#feed_item{}]).
%% FIXME: Xml not always needed
feed_items(FeedURL) ->
    {ok, _, Records} =
	?Q("SELECT \"feed\", \"id\", \"title\", \"homepage\", \"published\", \"payment\" FROM torrentified_items WHERE \"feed\"=$1 ORDER BY \"published\" DESC", [FeedURL]),
    [#feed_item{feed = Feed,
		id = Id,
		title = Title,
		published = Published,
		homepage = Homepage,
		payment = Payment}
     || {Feed, Id, Title, Homepage, Published, Payment} <- Records].

user_items(UserName) ->
    {ok, _, Records} =
	?Q("SELECT \"feed\", \"id\", \"title\", \"homepage\", \"published\", \"payment\" FROM torrentified_items WHERE \"feed\" IN (SELECT \"feed\" FROM user_feeds WHERE \"user\"=$1) ORDER BY \"published\" DESC LIMIT 30", [UserName]),
    [#feed_item{feed = Feed,
		id = Id,
		title = Title,
		published = Published,
		homepage = Homepage,
		payment = Payment}
     || {Feed, Id, Title, Homepage, Published, Payment} <- Records].

%%
%% Helpers
%%

enforce_string(S) when is_binary(S);
		       is_list(S) ->
    S;
enforce_string(_) ->
    <<"">>.
