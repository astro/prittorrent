-module(ui_template).

-export([render_index/0, render_user/1,
	 render_user_feed/2, export_feed/2]).

template_head() ->
    <<"<!DOCTYPE html>
<html>
  <head>
    <title>Bitlove</title>
    <link rel=\"stylesheet\" type=\"text/css\" href=\"static/style.css\"/>
  </head>
  <body><div class=\"frame\"><div class=\"pane\">
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
  </div></div></body>
</html>
">>.

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
	<h2>Popular Feeds</h2>
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
">>],
      <<"</article>">>]).

render_user(_UserName) ->
    throw({http, 404}).

render_user_feed(_UserName, _Slug) ->
    throw({http, 404}).

export_feed(_UserName, _Slug) ->
    throw({http, 404}).
