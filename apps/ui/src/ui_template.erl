-module(ui_template).

-export([render_index/0, render_user/1,
	 render_user_feed/2, export_feed/2]).

-include_lib("model/include/model.hrl").

template_head() ->
    <<"<!DOCTYPE html>
<html>
  <head>
    <title>Bitlove</title>
    <link rel=\"stylesheet\" type=\"text/css\" href=\"/static/style.css\"/>
    <link rel=\"stylesheet\" type=\"text/css\" href=\"/static/style.css\"/>
    <link rel=\"shortcut icon\" type=\"image/png\" href=\"/static/favicon.png\"/>
  </head>
  <body>
    <header>
      <h1>Bitlove</h1>
      <p class=\"slogan\">♥ Lovely BitTorrent Speed For Your Podcast Downloads</p>
    </header>
">>.

template_foot() ->
    <<"    <footer>
      <p>
	Are you a podcast publisher?
      </p>
      <p>
	<a href=\"/signup\">Sign up</a>
	•
	<a href=\"/login\">Log in</a>
      </p>
    </footer>
  </body>
</html>
">>.

render_item(Title) ->
    [<<"<h4>">>, escape(Title), <<"</h4>">>].

render_enclosure({URL, InfoHash}) ->
    Title = extract_name_from_url(URL),
    render_torrent(Title, InfoHash, 0, 0, 0, 0).

render_torrent(Title, InfoHash, Size, Seeders, Leechers, Bandwidth) ->
    [<<"<ul class=\"download\">
	  <li class=\"torrent\">
	    <a href=\"">>, escape_attr(ui_link:torrent(InfoHash)), <<"\">">>, escape(Title), <<"</a>
	  </li>">>,
     <<"<li class=\"stats\">
	    <span class=\"size\" title=\"Download size\">">>, integer_to_list(Size), <<" Bytes</span>
	    <span class=\"s\" title=\"Seeders\">">>, integer_to_list(Seeders), <<"</span>
	    <span class=\"l\" title=\"Leechers\">">>, integer_to_list(Leechers), <<"</span>
	    <span class=\"bw\" title=\"Total Bandwidth\">">>, integer_to_list(Bandwidth), <<" B/s</span>
	  </li>">>,
     <<"</ul>">>].

page_1column(Col) ->
    [template_head(), <<"<section class=\"col1\">
">>, Col, <<"</section>">>, template_foot()].

page_2column(Col1, Col2) ->
    [template_head(), <<"<section class=\"col1\">
">>, Col1, <<"</section>

<section class=\"col2\">">>, Col2, <<"</section>">>, template_foot()].

render_index() ->
    page_2column(
      [<<"      <div class=\"line\">
	<h2>Recent Torrents</h2>
	<!--ul class=\"feeds\">
	  <li><a href=\"fu.rss\">RSS</a></li>
	  <li><a href=\"fu.atom\">ATOM</a></li>
	</ul-->
      </div>
      <article>
">>,
       <<"</article>">>],
      [<<"<div class=\"line\">
	<h2>Popular Feeds</h2>
	<ul class=\"feeds\">
	  <li><a href=\"fu.rss\">RSS</a></li>
	  <li><a href=\"fu.atom\">ATOM</a></li>
	</ul>
      </div>

      <article>
">>,
      <<"</article>">>]).

render_user(_UserName) ->
    throw({http, 404}).

render_user_feed(UserName, Feed) ->
    FeedURL = model_users:get_feed(UserName, Feed),
    page_1column(
      lists:map(fun(#feed_item{id = ItemId,
			       title = ItemTitle}) ->
			Torrents = model_enclosures:item_torrents(FeedURL, ItemId),
			[<<"<article>">>,
			 render_item(ItemTitle),
			 lists:map(fun render_enclosure/1, Torrents),
			 <<"</article>">>]
		end, model_feeds:feed_items(FeedURL))).

export_feed(_UserName, _Slug) ->
    throw({http, 404}).

%%
%% Helpers
%%

extract_name_from_url(URL) when is_binary(URL) ->
    extract_name_from_url(binary_to_list(URL));
extract_name_from_url(URL) ->
    lists:foldl(fun([_ | _] = Token, _) ->
			Token
		end, "unnamed", string:tokens(URL, "/")).


escape(Bin) when is_binary(Bin) ->
    escape(binary_to_list(Bin));
escape(S) ->
    [case C of
	 $& -> <<"&amp;">>;
	 $< -> <<"&lt;">>;
	 $> -> <<"&gt;">>;
	 _ -> C
     end || C <- S].

escape_attr(Bin) when is_binary(Bin) ->
    escape(binary_to_list(Bin));
escape_attr(S) ->
    [case C of
	 $& -> <<"&amp;">>;
	 $< -> <<"&lt;">>;
	 $> -> <<"&gt;">>;
	 $' -> <<"&apos;">>;
	 $" -> <<"&quot;">>;
	 _ -> C
     end || C <- S].
