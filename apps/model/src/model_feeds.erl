-module(model_feeds).

-export([to_update/1, prepare_update/1, write_update/5, feed_items/1]).

-include("../include/model.hrl").

-define(POOL, pool_users).
-define(Q(Stmt, Params), model_sup:equery(?POOL, Stmt, Params)).
-define(T(Fun), model_sup:transaction(?POOL, Fun)).

to_update(Limit) ->
    case ?Q("SELECT \"url\" FROM \"feeds\" WHERE \"last_update\" IS NULL LIMIT $1", [Limit]) of
	{ok, _, [_ | _] = Results1} ->
	    {ok, [{URL, {{1970,1,1},{0,0,0}}}
		  || {URL} <- Results1]};
	_ ->
	    {ok, _, Results} =
		?Q("SELECT \"url\", \"last_update\" FROM \"feeds\" ORDER BY \"last_update\" ASC LIMIT $1", [Limit]),
	    {ok, [{URL, {YMD, {H, M, trunc(S)}}}
		  || {URL, {YMD, {H, M, S}}} <- Results]}
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
-spec(write_update/5 :: (string(),
			 {binary() | null, binary() | null},
			 binary() | null,
			 binary() | null,
			 [#feed_item{}])
			-> ok).
write_update(FeedURL, {Etag, LastModified},
	     Error, Xml, Items) when is_list(Etag) ->
    write_update(FeedURL, {list_to_binary(Etag), LastModified},
		 Error, Xml, Items);
write_update(FeedURL, {Etag, LastModified},
	     Error, Xml, Items) when is_list(LastModified) ->
    write_update(FeedURL, {Etag, list_to_binary(LastModified)},
		 Error, Xml, Items);
%% TODO: don't drop xml on error!
write_update(FeedURL, {Etag, LastModified}, Error, Xml, Items) ->
    ?T(fun(Q) ->
	       %% Update feed entry
	       Stmt = "UPDATE \"feeds\" SET \"last_update\"=CURRENT_TIMESTAMP, \"etag\"=$2, \"last_modified\"=$3, \"error\"=$4, \"xml\"=$5 WHERE \"url\"=$1",
	       Params = [FeedURL,
			 enforce_string(Etag), enforce_string(LastModified), 
			 enforce_string(Error), enforce_string(Xml)],
	       case Q(Stmt, Params) of
		   {ok, 1} ->
		       ok;	   
		   {ok, N} ->
		       exit({n_feeds, N})
	       end,

	       %% Update items
	       lists:foreach(
		 fun(#feed_item{} = Item) ->
			 io:format("Item ~s: ~s~n", [Item#feed_item.id, Item#feed_item.title]),
			 case Q("SELECT count(\"id\") FROM \"feed_items\" WHERE \"feed\"=$1 AND \"id\"=$2",
				[FeedURL, Item#feed_item.id]) of
			     {ok, _, [{0}]} ->
				 io:format("New feed item:~n~p~n", [Item]),
				 {ok, 1} =
				     Q("INSERT INTO \"feed_items\" (\"feed\", \"id\", \"title\", \"published\", \"homepage\", \"payment\", \"xml\", \"updated\") VALUES ($1, $2, $3, ($4::text)::timestamp, $5, $6, $7, CURRENT_TIMESTAMP)",
				       [FeedURL, Item#feed_item.id,
					Item#feed_item.title, Item#feed_item.published, 
					enforce_string(Item#feed_item.homepage), enforce_string(Item#feed_item.payment), 
					Item#feed_item.xml]);
			     {ok, _, [{1}]} ->
				 {ok, 1} =
				     Q("UPDATE \"feed_items\" SET \"title\"=$3, \"homepage\"=$4, \"payment\"=$5, \"xml\"=$6, \"updated\"=CURRENT_TIMESTAMP WHERE \"feed\"=$1 AND \"id\"=$2",
				       [FeedURL, Item#feed_item.id,
					Item#feed_item.title,
					enforce_string(Item#feed_item.homepage), enforce_string(Item#feed_item.payment), 
					Item#feed_item.xml])
			 end,
			 %% Update enclosures
			 Q("DELETE FROM \"enclosures\" WHERE \"feed\"=$1 AND \"item\"=$2",
			   [FeedURL, Item#feed_item.id]),
			 lists:foreach(
			   fun(Enclosure) ->
				   io:format("  e ~s~n", [Enclosure]),
				   Q("INSERT INTO \"enclosures\" (\"feed\", \"item\", \"url\") VALUES ($1, $2, $3)",
				     [FeedURL, Item#feed_item.id, Enclosure])
			   end, Item#feed_item.enclosures)
		 end, Items),

	       ok
       end).


-spec(feed_items/1 :: (string()) -> [#feed_item{}]).
feed_items(FeedURL) ->
    {ok, _, Records} =
	?Q("SELECT \"feed\", \"id\", \"title\", \"homepage\", \"published\", \"payment\", \"xml\" FROM feed_items WHERE \"feed\"=$1", [FeedURL]),
    [#feed_item{feed = Feed,
		id = Id,
		title = Title,
		published = Published,
		homepage = Homepage,
		payment = Payment,
		xml = Xml}
     || {Feed, Id, Title, Homepage, Published, Payment, Xml} <- Records].

enforce_string(S) when is_binary(S);
		       is_list(S) ->
    S;
enforce_string(_) ->
    <<"">>.
