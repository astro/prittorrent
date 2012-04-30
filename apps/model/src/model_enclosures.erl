-module(model_enclosures).

-export([to_hash/0, set_torrent/3, item_torrents/2,
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

%% TODO: rm, expensive view
item_torrents(Feed, Item) ->
    {ok, _, Torrents} =
	?Q("SELECT \"url\", \"info_hash\" FROM item_torrents WHERE \"feed\"=$1 AND \"item\"=$2 ORDER BY \"url\"", [Feed, Item]),
    Torrents.

user_downloads(UserName) ->
    query_downloads("\"feed\" IN (SELECT \"feed\" FROM user_feeds WHERE \"user\"=$1)", [UserName]).

feed_downloads(Feed) ->
    io:format("feed_downloads ~p~n", [Feed]),
    query_downloads("\"feed\"=$1", [Feed]).

query_downloads(Cond, Params) ->
    case ?Q("SELECT \"feed\", \"item\", \"enclosure\", \"info_hash\", \"name\", \"size\", \"title\", \"published\", \"homepage\", \"payment\", \"image\" FROM downloads_cache WHERE " ++ Cond ++ " ORDER BY \"published\" DESC", Params) of
	{ok, _, Rows} ->
	    Downloads =
		[#download{feed = Feed,
			   item = Item,
			   enclosure = Enclosure,
			   info_hash = InfoHash,
			   name = Name,
			   size = Size,
			   title = Title,
			   published = Published,
			   homepage = Homepage,
			   payment = Payment,
			   image = Image}
		 || {Feed, Item, Enclosure,
		     InfoHash, Name, Size,
		     Title, Published, Homepage, Payment, Image
		    } <- Rows],
	    {ok, group_downloads(Downloads)};
	{error, Reason} ->
	    {error, Reason}
    end.

%% By homepage
group_downloads([]) ->
    [];
group_downloads([Download | Downloads]) ->
    #download{feed = Feed,
	      item = Item,
	      title = Title,
	      published = Published,
	      homepage = Homepage,
	      payment = Payment,
	      image = Image} = Download,
    {SiblingDownloads, OtherDownloads} =
	lists:splitwith(
	  fun(#download{homepage = Homepage1}) ->
		  Homepage == Homepage1
	  end, Downloads),
    FeedItem =
	#feed_item{feed = Feed,
		   id = Item,
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
