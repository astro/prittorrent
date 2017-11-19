-module(model_enclosures).

-export([to_hash/0, to_recheck/0, set_torrent/6,
	 get_info_hash_by_name/3, get_torrent_by_name/3,
	 purge/3,
	 recent_downloads/1, popular_downloads/2,
	 user_downloads/2, feed_downloads/2,
	 enclosure_downloads/1]).

-include("../include/model.hrl").

-define(POOL, pool_users).
-define(Q(Stmt, Params), model_sup:equery(?POOL, Stmt, Params)).
-define(T(Fun), model_sup:transaction(?POOL, Fun)).

to_hash() ->
    ?T(fun(Q) ->
               LockResult = Q("LOCK TABLE \"enclosure_torrents\" IN SHARE ROW EXCLUSIVE MODE", []),
               case LockResult of
                   {ok, [], []} ->
                       case Q("SELECT \"enclosure_url\" FROM enclosure_to_hash()", []) of
                           {ok, _, [{URL}]}
                             when is_binary(URL),
                                  size(URL) > 0 ->
                               {ok, URL};
                           {ok, _, [{null}]} ->
                               nothing
                       end;
                   {error, timeout} ->
                       nothing
               end
       end).

to_recheck() ->
    ?T(fun(Q) ->
               LockResult = Q("LOCK TABLE \"enclosure_torrents\" IN SHARE ROW EXCLUSIVE MODE", []),
               case LockResult of
                   {ok, [], []} ->
                       case Q("SELECT e_url, e_length, e_etag, e_last_modified FROM enclosure_to_recheck()", []) of
                           {ok, _, [{URL, Length, ETag, LastModified}]}
                             when is_binary(URL),
                                  size(URL) > 0 ->
                               {ok, URL, Length, ETag, LastModified};
                           {ok, _, [{null, null, null, null}]} ->
                               nothing
                       end;
                   {error, timeout} ->
                       nothing
               end
       end).


set_torrent(URL, Error, InfoHash, Length, undefined, LastModified) ->
    set_torrent(URL, Error, InfoHash, Length, null, LastModified);
set_torrent(URL, Error, InfoHash, Length, ETag, undefined) ->
    set_torrent(URL, Error, InfoHash, Length, ETag, null);
set_torrent(URL, Error, InfoHash, Length, ETag, LastModified) ->
    ?T(fun(Q) ->
	       case Q("SELECT count(\"url\") FROM enclosure_torrents WHERE \"url\"=$1", [URL]) of
		   {ok, _, [{0}]} ->
		       Q("INSERT INTO enclosure_torrents (\"url\", \"last_update\", \"length\", \"etag\", \"last_modified\", \"info_hash\", \"error\") VALUES ($1, CURRENT_TIMESTAMP, $2, $3, $4, $5, $6)", [URL, Length, ETag, LastModified, InfoHash, Error]);
		   {ok, _, [{1}]} ->
		       Q("UPDATE enclosure_torrents SET \"last_update\"=CURRENT_TIMESTAMP, \"length\"=$2, \"etag\"=$3, \"last_modified\"=$4, \"info_hash\"=$5, \"error\"=$6 WHERE \"url\"=$1", [URL, Length, ETag, LastModified, InfoHash, Error])
	       end
       end).

get_torrent_by_name(UserName, Slug, Name) ->
    case ?Q("SELECT torrents.\"torrent\" FROM user_feeds JOIN enclosures USING (feed) JOIN enclosure_torrents USING (url) JOIN torrents USING (info_hash) WHERE user_feeds.\"user\"=$1 AND user_feeds.\"slug\"=$2 AND torrents.\"name\"=$3",
	    [UserName, Slug, Name]) of
	{ok, _, [{Torrent} | _]} ->
	    {ok, Torrent};
	{ok, _, []} ->
	    {error, not_found}
    end.

get_info_hash_by_name(UserName, Slug, Name) ->
    case ?Q("SELECT torrents.\"info_hash\" FROM user_feeds JOIN enclosures USING (feed) JOIN enclosure_torrents USING (url) JOIN torrents USING (info_hash) WHERE user_feeds.\"user\"=$1 AND user_feeds.\"slug\"=$2 AND torrents.\"name\"=$3",
	    [UserName, Slug, Name]) of
	{ok, _, [{InfoHash} | _]} ->
	    {ok, InfoHash};
	{ok, _, []} ->
	    {error, not_found}
    end.

purge(UserName, Slug, Name) ->
    ?Q("SELECT * FROM purge_download($1, $2, $3)", [UserName, Slug, Name]).

recent_downloads(Limit) ->
    query_downloads("get_recent_downloads($1)", [Limit]).

popular_downloads(Limit, peers) ->
    query_downloads("get_popular_downloads($1)", [Limit]);

popular_downloads(Limit, Period) when is_integer(Period) ->
    query_downloads("get_most_downloaded($1, $2)", [Limit, Period]);
popular_downloads(Limit, all) ->
    %% get_popular_downloads() will select "downloaded"
    popular_downloads(Limit, 10000).


user_downloads(UserName, Limit) ->
    query_downloads("get_user_recent_downloads($2, $1)", [UserName, Limit]).

feed_downloads(Feed, Limit) ->
    query_downloads("get_recent_downloads($2, $1)", [Feed, Limit]).

enclosure_downloads(Enclosure) ->
    {ok, _, Rows} =
	?Q("SELECT * FROM get_enclosure_downloads($1)", [Enclosure]),
    rows_to_downloads(Rows).

query_downloads(View, Params) ->
    case ?Q("SELECT * FROM " ++ View, Params) of
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
	       type = Type,
	       feed_title = FeedTitle,
	       title = Title,
	       lang = Lang,
	       summary = Summary,
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
	 FeedTitle, _FeedPublic,
	 InfoHash, Name, Size, Type,
	 Title, Lang, Summary, Published, Homepage, Payment, Image,
	 Seeders, Leechers, Upspeed, Downspeed,
	 Downloaded  %% ordered like in Postgres TYPE 'download'
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
	      lang = Lang,
	      summary = Summary,
	      published = Published,
	      homepage = Homepage,
	      payment = Payment,
	      image = Image} = Download,
    {SiblingDownloads, OtherDownloads} =
	lists:splitwith(
	  fun(#download{user = User1,
			item = Item1,
			title = Title1,
			homepage = Homepage1}) when User == User1 ->
		  if
		      %% Items from the very same homepage
		      is_binary(Homepage1),
		      size(Homepage1) > 0,
		      Homepage == Homepage1 ->
			  true;
		      %% Probably same homepage, but masked by
		      %% Feedburner's redirecting URLs to track users
		      is_binary(Title1),
		      size(Title1) > 0,
		      Title == Title1 ->
			  true;
		      %% Equal item id
		      true ->
			  Item == Item1
		  end;
	     (_) ->
		  false
	  end, Downloads),
    FeedItem =
	#feed_item{user = User,
		   slug = Slug,
		   feed = Feed,
		   id = Item,
		   feed_title = FeedTitle,
		   title = Title,
		   lang = Lang,
		   summary = Summary,
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
