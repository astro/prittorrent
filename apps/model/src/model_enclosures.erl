-module(model_enclosures).

-export([to_hash/0, set_torrent/3,
	 get_torrent_by_name/3, purge/3,
	 recent_downloads/0, popular_downloads/0,
	 recent_downloads_without_popular/0,
	 user_downloads/1, feed_downloads/1]).

-include("../include/model.hrl").

-define(POOL, pool_users).
-define(Q(Stmt, Params), model_sup:equery(?POOL, Stmt, Params)).
-define(T(Fun), model_sup:transaction(?POOL, Fun)).

to_hash() ->
    case ?Q("SELECT \"enclosure_url\" FROM enclosure_to_hash()", []) of
	{ok, _, [{URL}]}
	  when is_binary(URL),
	       size(URL) > 0 ->
	    {ok, URL};
	{ok, _, [{null}]} ->
	    nothing
    end.

set_torrent(URL, Error, InfoHash) ->
    ?T(fun(Q) ->
	       case Q("SELECT count(\"url\") FROM enclosure_torrents WHERE \"url\"=$1", [URL]) of
		   {ok, _, [{0}]} ->
		       Q("INSERT INTO enclosure_torrents (\"url\", \"last_update\", \"info_hash\", \"error\") VALUES ($1, CURRENT_TIMESTAMP, $2, $3)", [URL, InfoHash, Error]);
		   {ok, _, [{1}]} ->
		       Q("UPDATE enclosure_torrents SET \"last_update\"=CURRENT_TIMESTAMP, \"info_hash\"=$2, \"error\"=$3 WHERE \"url\"=$1", [URL, InfoHash, Error])
	       end
       end).

get_torrent_by_name(UserName, Slug, Name) ->
    case ?Q("SELECT torrents.\"torrent\" FROM downloads_by_user JOIN torrents USING (info_hash) WHERE downloads_by_user.\"user\"=$1 AND downloads_by_user.\"slug\"=$2 AND downloads_by_user.\"name\"=$3",
	    [UserName, Slug, Name]) of
	{ok, _, [{Torrent} | _]} ->
	    {ok, Torrent};
	{ok, _, []} ->
	    {error, not_found}
    end.

purge(UserName, Slug, Name) ->
    ?Q("SELECT * FROM purge_download($1, $2, $3)", [UserName, Slug, Name]).

recent_downloads() ->
    query_downloads("\"feed_public\"", [],
		    published, 32).

popular_downloads() ->
    query_downloads("\"feed_public\" AND (\"seeders\" + \"leechers\") > 0", [],
		    popularity, 24).

recent_downloads_without_popular() ->
    query_downloads("\"feed_public\" AND \"info_hash\" NOT IN (SELECT \"info_hash\" FROM downloads_by_popularity LIMIT 24)", [],
		    published, 24).

user_downloads(UserName) ->
    query_downloads("\"feed_public\" AND \"user\"=$1", [UserName],
		    published, 23).

feed_downloads(Feed) ->
    query_downloads("\"feed\"=$1", [Feed],
		    published, 50).

query_downloads(Cond, Params, Order, Limit) ->
    TabName = case Order of
		  published -> "downloads_by_published";
		  popularity -> "downloads_by_popularity"
	      end,
    case ?Q("SELECT \"user\", \"slug\", \"feed\", \"item\", \"enclosure\", \"info_hash\", \"name\", \"size\", \"feed_title\", \"title\", \"published\", \"homepage\", \"payment\", \"image\", \"seeders\", \"leechers\", \"upspeed\", \"downspeed\", \"downloaded\" FROM " ++ TabName ++ " WHERE " ++ Cond ++ " LIMIT " ++ integer_to_list(Limit), Params) of
	{ok, _, Rows} ->
	    Downloads =
		rows_to_downloads(Rows),
	    FeedItems = group_downloads(Downloads),
	    {ok, FeedItems};
	{error, Reason} ->
	    {error, Reason}
    end.

rows_to_downloads(Rows) ->
    [#download{user = User,
	       slug = Slug,
	       feed = Feed,
	       item = Item,
	       enclosure = Enclosure,
	       info_hash = InfoHash,
	       name = Name,
	       size = Size,
	       feed_title = FeedTitle,
	       title = Title,
	       published = Published,
	       homepage = Homepage,
	       payment = Payment,
	       image = Image,
	       seeders = Seeders,
	       leechers = Leechers,
	       upspeed = Upspeed,
	       downspeed = Downspeed,
	       downloaded = Downloaded}
     || {User, Slug, Feed, Item, Enclosure,
	 InfoHash, Name, Size,
	 FeedTitle,
	 Title, Published, Homepage, Payment, Image,
	 Seeders, Leechers, Upspeed, Downspeed,
	 Downloaded
	} <- Rows].

%% By homepage
group_downloads([]) ->
    [];
group_downloads([Download | Downloads]) ->
    #download{user = User,
	      slug = Slug,
	      feed = Feed,
	      item = Item,
	      feed_title = FeedTitle,
	      title = Title,
	      published = Published,
	      homepage = Homepage,
	      payment = Payment,
	      image = Image} = Download,
    {SiblingDownloads, OtherDownloads} =
	lists:splitwith(
	  fun(#download{item = Item1,
			homepage = Homepage1}) ->
		  if
		      is_binary(Homepage1),
		      size(Homepage1) > 0 ->
			  Homepage == Homepage1;
		      true ->
			  Item == Item1
		  end
	  end, Downloads),
    FeedItem =
	#feed_item{user = User,
		   slug = Slug,
		   feed = Feed,
		   id = Item,
		   feed_title = FeedTitle,
		   title = Title,
		   published = Published,
		   homepage = Homepage,
		   payment = Payment,
		   image = Image,
		   %% Duplicate downloads may occur for merged
		   %% feed_items (by homepage):
		   downloads = unique_downloads([Download | SiblingDownloads])
		  },
    [FeedItem | group_downloads(OtherDownloads)].

%% Also sorts by name
unique_downloads(Downloads) ->
    ByName =
	lists:foldl(fun(#download{name = Name} = Download, ByName) ->
			    case gb_trees:is_defined(Name, ByName) of
				false ->
				    gb_trees:insert(Name, Download, ByName);
				true ->
				    %% Drop duplicate
				    ByName
			    end
		    end, gb_trees:empty(), Downloads),
    [Download
     || {_Name, Download} <- gb_trees:to_list(ByName)].
