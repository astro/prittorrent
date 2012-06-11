-module(model_feeds).

-export([to_update/1, prepare_update/1, write_update/8,
	 user_feeds_details/2, user_feed_details/2,
	 feed_data/2, get_directory/0]).

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
    T1 = util:get_now_us(),
    ?T(fun(Q) ->
	       %% Update feed entry
	       case Error of
		   null ->
		       Stmt = "UPDATE \"feeds\" SET \"last_update\"=CURRENT_TIMESTAMP, \"etag\"=$2, \"last_modified\"=$3, \"error\"=null, \"xml\"=$4, \"title\"=$5, \"homepage\"=$6, \"image\"=$7 WHERE \"url\"=$1",
		       Params = [FeedURL,
				 enforce_string(Etag), enforce_string(LastModified), 
				 enforce_string(Xml),
				 enforce_string(Title), enforce_string(Homepage),
				 enforce_string(Image)];
		   not_modified ->
		       Stmt = "UPDATE \"feeds\" SET \"last_update\"=CURRENT_TIMESTAMP, \"etag\"=$2, \"last_modified\"=$3 WHERE \"url\"=$1",
		       Params = [FeedURL,
				 enforce_string(Etag), enforce_string(LastModified)];
		   _ when is_binary(Error) ->
		       Stmt = "UPDATE \"feeds\" SET \"last_update\"=CURRENT_TIMESTAMP, \"etag\"=$2, \"last_modified\"=$3, \"error\"=$4 WHERE \"url\"=$1",
		       Params = [FeedURL,
				 enforce_string(Etag), enforce_string(LastModified), 
				 enforce_string(Error)]
	       end,
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
			 {ok, _, ToDeleteRows} =
			     Q("SELECT \"url\" FROM \"enclosures\" WHERE \"feed\"=$1 AND \"item\"=$2",
			       [FeedURL, Item#feed_item.id]),
			 %% Hrm, ToDelete is not worth the sets overhead 99.9% of the time
			 ToDelete =
			     lists:foldl(
			       fun(Enclosure, ToDelete) ->
				       case sets:is_element(Enclosure, ToDelete) of
					   true ->
					       sets:del_element(Enclosure, ToDelete);
					   false ->
					       Q("INSERT INTO \"enclosures\" (\"feed\", \"item\", \"url\") VALUES ($1, $2, $3)",
						 [FeedURL, Item#feed_item.id, Enclosure]),
					       ToDelete
				       end
			       end,
			       sets:from_list([Enclosure || {Enclosure} <- ToDeleteRows]),
			       list_uniq(Item#feed_item.enclosures)),
			 lists:foreach(
			   fun(Enclosure) ->
				   ?Q("DELETE FROM \"enclosures\" WHERE \"feed\"=$1 AND \"item\"=$2 AND \"url\"=$3",
				      [FeedURL, Item#feed_item.id, Enclosure])
			   end, sets:to_list(ToDelete))
		 end, Items),

	       ok
       end),
    
    T2 = util:get_now_us(),
    io:format("[~.1fms] write_update ~s - ~B items~n", [(T2 - T1) / 1000, FeedURL, length(Items)]).


user_feeds_details(UserName, Private) ->
    PublicCond = case Private of
		     true ->
			 "";
		     _ ->
			 " AND user_feeds.\"public\""
		 end,
    case ?Q("SELECT user_feeds.\"slug\", feeds.\"url\", COALESCE(user_feeds.\"title\", feeds.\"title\"), feeds.\"homepage\", feeds.\"image\", COALESCE(user_feeds.\"public\", FALSE) FROM user_feeds INNER JOIN feeds ON user_feeds.feed=feeds.url WHERE user_feeds.\"user\"=$1" ++ PublicCond ++ " ORDER BY LOWER(feeds.\"title\") ASC",
	    [UserName]) of
	{ok, _, Rows} ->
	    {ok, Rows};
	{error, Reason} ->
	    {error, Reason}
    end.

user_feed_details(UserName, Slug) ->
    case ?Q("SELECT feeds.\"url\", COALESCE(user_feeds.\"title\", feeds.\"title\"), feeds.\"homepage\", feeds.\"image\", COALESCE(user_feeds.\"public\", FALSE), feeds.\"torrentify\" FROM user_feeds INNER JOIN feeds ON user_feeds.feed=feeds.url WHERE user_feeds.\"user\"=$1 AND user_feeds.\"slug\"=$2",
	    [UserName, Slug]) of
	{ok, _, [{URL, Title, Homepage, Image, Public, Torrentify}]} ->
	    {ok, URL, Title, Homepage, Image, Public, Torrentify};
	{ok, _, []} ->
	    {error, not_found};
	{error, Reason} ->
	    {error, Reason}
    end.

-spec(feed_data/2 :: (binary(), integer())
		     -> ({ok, binary(), [binary()], [{binary(), binary()}]} |
			 {error, not_found})).
feed_data(FeedURL, MaxEnclosures) ->
    case ?Q("SELECT \"xml\" FROM feeds WHERE \"url\"=$1",
	    [FeedURL]) of
	{ok, _, [{FeedXml}]} ->
	    {ok, _, Rows} =
		?Q("SELECT enclosures.url, torrents.name, feed_items.xml FROM feed_items JOIN enclosures ON (feed_items.feed=enclosures.feed AND feed_items.id=enclosures.item) JOIN enclosure_torrents USING (url) JOIN torrents USING (info_hash) WHERE feed_items.feed=$1 ORDER BY feed_items.published DESC, feed_items.id ASC LIMIT $2",
		   [FeedURL, MaxEnclosures]),
	    EnclosureMap =
		[{URL, Name}
		 || {URL, Name, _Xml} <- Rows],
	    ItemXmls =
		list_drop_subsequent_dups(
		  [Xml
		   || {_URL, _Name, Xml} <- Rows]),
	    {ok, FeedXml, ItemXmls, EnclosureMap};
	{ok, _, []} ->
	    {error, not_found}
    end.

get_directory() ->
    {ok, _, Rows} =
	?Q("SELECT \"user\", \"title\", \"image\", \"slug\", \"feed_title\" FROM directory", []),
    group_directory_feeds(Rows).

group_directory_feeds([]) ->
    [];
group_directory_feeds([{User1, Title1, Image1, _, _} | _] = Directory) ->
    {Directory1, Directory2} =
	lists:splitwith(
	  fun({User2, _, _, _, _}) ->
		     User1 == User2
	  end, Directory),
    [{User1, Title1, Image1,
      [{Slug, FeedTitle}
       || {_, _, _, Slug, FeedTitle} <- Directory1]
     } | group_directory_feeds(Directory2)].


%%
%% Helpers
%%

enforce_string(S) when is_binary(S);
		       is_list(S) ->
    S;
enforce_string(_) ->
    <<"">>.

list_drop_subsequent_dups([]) ->
    [];
list_drop_subsequent_dups([E, E | L]) ->
    list_drop_subsequent_dups([E | L]);
list_drop_subsequent_dups([E | L]) ->
    [E | list_drop_subsequent_dups(L)].

list_uniq([]) ->
    [];
list_uniq([E | L]) ->
    [E |
     list_uniq([E1
		|| E1 <- L,
		   E1 =/= E])].
