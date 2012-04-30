-module(ui_template).

-export([render_index/0, render_user/1,
	 render_user_feed/2, export_feed/2]).

-include_lib("model/include/model.hrl").

html(HeadEls, Contents) ->
    [<<"<?xml version='1.0' encoding='UTF-8'?>\n<!DOCTYPE html>\n">>,
     html:to_iolist(
       {html,
	[{head,
	  [{title, "Bitlove"},
	   {link, [{rel, "stylesheet"},
		   {type, "text/css"},
		   {href, "/static/style.css"}], ""},
	   {link, [{rel, "shortcut icon"},
		   {type, "image/png"},
		   {href, "/static/favicon.png"}], ""}
	   | HeadEls]},
	 {body,
	  [{header, [{class, "site"}],
	    [{h1, 
	      {a, [{href, "/"}], "Bitlove"}
	     },
	     {p, [{class, "slogan"}], "Peer-to-Peer Love for Your Podcast Downloads"}
	    ]} | Contents] ++
	      [{footer,
		[{'div',
		  [{p,
		    [<<"Vision by ">>,
		     {a, [{href, <<"http://tim.geekheim.de/">>}], <<"Tim Pritlove">>}
		    ]},
		   {p,
		    [<<"Open Source by ">>,
		     {a, [{href, <<"http://spaceboyz.net/~astro/">>}], <<"Astro">>}
		    ]},
		   {p,
		    [<<"Report Bugs ">>,
		     {a, [{href, <<"https://github.com/astro/prittorrent/issues">>}], <<"on Github!">>}
		    ]}
		  ]},
		 {'div',
		  [{p, <<"Are you a podcast publisher?">>},
		   {p, <<"Sign up + add your feeds soon!">>}
		  ]}
		]},
	       {script, [{type, <<"text/javascript">>}],
		[<<"/* ">>,
		 {'!CDATA', <<" */
(function() {
    var s = document.createElement('script');
    var t = document.getElementsByTagName('script')[0];

    s.type = 'text/javascript';
    s.async = true;
    s.src = 'https://api.flattr.com/js/0.6/load.js?mode=auto&popout=1&button=compact';

    t.parentNode.insertBefore(s, t);
 })();
/* ">>},
		 <<" */">>]
	       }]}
	]}
      )].

render_link(URL = <<"http://", URL1/binary>>) ->
    render_link(URL, URL1);
render_link(URL = <<"https://", URL1/binary>>) ->
    render_link(URL, URL1);
render_link(URL) ->
    io:format("render_link ~p~n", [URL]),
    render_link(URL, URL).

render_link(URL, Text) ->
    {a, [{href, URL}], Text}.


render_meta(Heading, Title, Image, Homepage) ->
    {'div', [{class, "meta"}],
     [if
	  is_binary(Image),
	  size(Image) > 0 ->
	      {img, [{src, Image},
		     {class, "logo"}], []};
	  true ->
	      []
      end,
      {'div',
       [{Heading, Title},
	if
	    is_binary(Homepage),
	    size(Homepage) > 0 ->
		{p, [{class, "homepage"}],
		 render_link(Homepage)};
	    true ->
		[]
	end
      ]}
    ]}.


render_item(ItemLink, Title, Image, Homepage, Payment) ->
    {'div',
     [if
	  is_binary(Image),
	  size(Image) > 0 ->
	      {img, [{src, Image},
		     {class, "logo"}], []};
	  true ->
	      []
      end,
      {'div', [{class, <<"flattr">>}],
       [
	case Payment of
	    %% Transform an autosubmit link to Flattr button
	    <<"https://flattr.com/submit/auto?",
	      Payment1/binary>> ->
		{a, [{class, <<"FlattrButton">>},
		     {href, Payment}
		     | [case K of
			    <<"user_id">> ->
				{"data-flattr-uid", V};
			    _ ->
				{"data-flattr-" ++ K, V}
			end
			|| {K, V} <- cowboy_http:x_www_form_urlencoded(
				       Payment1, fun cowboy_http:urldecode/1)
		       ]],
		 <<"Support the podcaster">>};
	    _ when is_binary(Payment),
		   size(Payment) > 0 ->
		{a, [{href, Payment}],
		 <<"Support the podcaster">>};
	    _ ->
		[]
	end,
	{br, []},
	{a, [{class, <<"FlattrButton">>},
	     {href, <<"https://flattr.com/profile/Astro">>},
	     {'data-flattr-url', <<(ui_link:base())/binary,
				   ItemLink/binary>>},
	     {'data-flattr-uid', <<"Astro">>},
	     {'data-flattr-title', <<Title/binary, " on Bitlove">>},
	     {'data-flattr-description', <<"Torrentification & Seeding">>},
	     {'data-flattr-category', <<"rest">>},
	     {'data-flattr-tags', <<"torrent,bittorrent,p2p,filesharing">>}
	    ],
	 <<"Support Bitlove">>}
       ]},
      {'div',
       [{h3,
	 {a, [{href, ItemLink}], Title}
	},
	if
	    is_binary(Homepage),
	    size(Homepage) > 0 ->
		{p, [{class, "homepage"}],
		 render_link(Homepage)};
	    true ->
		[]
	end
      ]}
    ]}.

%% TODO: rm slow code path
render_enclosure({_URL, InfoHash}) ->
    case model_torrents:get_info(InfoHash) of
	{ok, Name, Size} ->
	    {ok, Leechers, Seeders, Downspeed, _Downloaded} =
		model_tracker:scrape(InfoHash),
	    {ok, MySeeders} = application:get_env(ui, seeders),
	    render_torrent(Name, InfoHash, Size, Seeders + length(MySeeders), Leechers, Downspeed);
	{error, not_found} ->
	    io:format("Enclosure not found: ~p ~p~n", [_URL, InfoHash]),
	    []
    end;
render_enclosure(#download{name = Name,
			   info_hash = InfoHash,
			   size = Size}) ->
    Seeders = 0, %% TODO
    Leechers = 0, %% TODO
    Downspeed = 0, %% TODO
    {ok, MySeeders} = application:get_env(ui, seeders),
    render_torrent(Name, InfoHash, Size, Seeders + length(MySeeders), Leechers, Downspeed).


render_torrent(Title, InfoHash, Size, Seeders, Leechers, Bandwidth) ->
    {ul, [{class, "download"}],
     [{li, [{class, "torrent"}],
       {a, [{href, ui_link:torrent(InfoHash)}], Title}
      },
      {li, [{class, "stats"}],
       [{span, [{class, "size"},
		{title, "Download size"}], size_to_human(Size)},
	{span, [{class, "s"},
		{title, "Seeders"}], integer_to_list(Seeders)},
	{span, [{class, "l"},
		{title, "Leechers"}], integer_to_list(Leechers)},
	{span, [{class, "bw"},
		{title, "Current Total Bandwidth"}], [size_to_human(Bandwidth), "/s"]}
       ]}
     ]}.

page_1column(FeedLink, Col) ->
    HeadEls =
	if
	    is_binary(FeedLink),
	    size(FeedLink) > 0 ->
		FeedURL =
		    <<(ui_link:base())/binary,
		      FeedLink/binary>>,
		%% TODO: type can be ATOM
		[{alternate, [{rel, <<"alternate">>},
			      {type, <<"application/rss+xml">>},
			      {href, FeedURL}], []}];
	    true ->
		[]
	end,
    html(HeadEls,
	 [{section, [{class, "col"}], Col}]).

page_2column(Col1, Col2) ->
    page_2column([], Col1, Col2).

page_2column(Prologue, Col1, Col2) ->
    html([],
	 [Prologue,
	  {section, [{class, "col1"}], Col1},
	  {section, [{class, "col2"}], Col2}
	 ]).

%% TODO
render_index() ->
    page_2column(
      [{'div',
	[{h2, "Recent Torrents"}
	]} |
       []],
      [{'div',
	[{h2, "Popular Feeds"}
	]} |
       []]
     ).

%% Feeds, Recent Episodes
render_user(UserName) ->
    {UserTitle, UserImage, UserHomepage} =
	case model_users:get_details(UserName) of
	    {ok, Title1, Image1, Homepage1} ->
		{Title1, Image1, Homepage1};
	    {error, not_found} ->
		throw({http, 404})
	end,
    UserFeeds = model_users:get_feeds(UserName),
    {ok, UserDownloads} =
	model_enclosures:user_downloads(UserName),

    page_2column(
      {header, [{class, "user"}],
       render_meta(h2, UserTitle, UserImage, UserHomepage)
      },
      [{h2, "Feeds"} |
       lists:map(fun({Slug, Feed}) ->
			 case model_feeds:feed_details(Feed) of
			     {ok, Title, Homepage, Image}
			       when is_binary(Title),
				    size(Title) > 0 ->
				 io:format("Details: ~p ~p ~p~n", [Title, Homepage, Image]),
				 {article,
				  
				  [{'div',
				    [if
					 is_binary(Image),
					 size(Image) > 0 ->
					     {img, [{src, Image},
						    {class, "logo"}], ""};
					 true ->
					     []
				     end,
				     {'div',
				      [{h3, 
					[{a, [{href, ui_link:link_user_feed(UserName, Slug)}], Title}]},
				       if
					   is_binary(Homepage),
					   size(Homepage) > 0 ->
					       {p, [{class, "homepage"}],
						[render_link(Homepage)]};
					   true ->
					       []
				       end
				      ]}
				    ]}
				  ]};
			     _ ->
				 ""
			 end
		 end, UserFeeds)
      ],
      [{h2, "Recent Episodes"} |
       lists:map(fun(#feed_item{feed = FeedURL,
				id = ItemId,
				title = ItemTitle,
				image = ItemImage,
				homepage = ItemHomepage,
				payment = ItemPayment,
				downloads = ItemDownloads}) ->
			 {value, {Slug, FeedURL}} =
			     lists:keysearch(FeedURL, 2, UserFeeds),
			 ItemLink = ui_link:link_item(UserName, Slug, ItemId),
			 {article, [{class, "item"}],
			  [render_item(ItemLink, ItemTitle, ItemImage, ItemHomepage, ItemPayment) |
			   lists:map(fun render_enclosure/1, ItemDownloads)
			  ]}
		 end, UserDownloads)
      ]
     ).

render_user_feed(UserName, Slug) ->
    {UserTitle, _UserImage, _UserHomepage} =
	case model_users:get_details(UserName) of
	    {ok, Title1, Image1, Homepage1} ->
		{Title1, Image1, Homepage1};
	    {error, not_found} ->
		throw({http, 404})
	end,

    FeedURL =
	case model_users:get_feed(UserName, Slug) of
	    {ok, FeedURL1} ->
		FeedURL1;
	    {error, not_found} ->
		throw({http, 404})
	end,
    {ok, FeedTitle, FeedHomepage, FeedImage} =
	model_feeds:feed_details(FeedURL),
    {ok, FeedDownloads} =
	model_enclosures:feed_downloads(FeedURL),
    
    page_1column(
      ui_link:link_user_feed_xml(UserName, Slug),
      [{header, [{class, "feed"}],
	render_meta(h2,
		    [FeedTitle,
		     {a, [{class, "feedicon"},
			  {href, ui_link:link_user_feed_xml(UserName, Slug)}],
		      <<" (Subscribe)">>},
		     {span, [{class, "publisher"}],
		      [<<" by ">>,
		       {a, [{href, ui_link:link_user(UserName)}],
			UserTitle}
		      ]}
		    ], FeedImage, FeedHomepage)
       } |
       lists:map(fun(#feed_item{id = ItemId,
				title = ItemTitle,
				image = ItemImage,
				homepage = ItemHomepage,
				payment = ItemPayment,
				downloads = ItemDownloads}) ->
			 ItemLink = ui_link:link_item(UserName, Slug, ItemId),
			 {article, [{class, "item"},
				    {id, ItemId}],
			  [render_item(ItemLink, ItemTitle, ItemImage, ItemHomepage, ItemPayment) |
			   lists:map(fun render_enclosure/1, ItemDownloads)
			  ]}
		 end, FeedDownloads)
      ]).

export_feed(UserName, Slug) ->
    case model_users:get_feed(UserName, Slug) of
	{ok, FeedURL} ->
	    {ok, FeedXml, ItemXmls, EnclosuresMap} =
		model_feeds:feed_data(FeedURL),
	    [FeedEl | _] =
		exmpp_xml:parse_document(FeedXml,
					 [{names_as_atom, false},
					  {engine, expat}]),
	    Type = feeds_parse:get_type(FeedEl),
	    ItemEls =
		lists:map(
		  fun(Xml) ->
			  [ItemEl | _] =
			      exmpp_xml:parse_document(
				Xml,
				[{names_as_atom, false},
				 {engine, expat}]),
			  feeds_parse:replace_item_enclosures(
			    ItemEl,
			    fun(URL) ->
				    case proplists:get_value(URL, EnclosuresMap) of
					<<InfoHash:20/binary>> ->
					    <<(ui_link:base())/binary,
					      (ui_link:torrent(InfoHash))/binary>>;
					_ ->
					    io:format("Cannot map enclosure ~s~n", [URL]),
					    URL
				    end
			    end)
		  end, ItemXmls), 
	    CompleteFeedEl = feeds_parse:merge_items(FeedEl, ItemEls),
	    Body = exmpp_xml:document_to_iolist(CompleteFeedEl),
	    {ok, Type,
	     [<<"<?xml version='1.0' encoding='UTF-8'?>\n">>,
	      Body]};
	{error, not_found} ->
	    throw({http, 404})
    end.

%%
%% Helpers
%%


size_to_human(Size)
  when Size < 1024 ->
    io_lib:format("~B B", [Size]);
size_to_human(Size) ->
    size_to_human(Size / 1024, "KMGT").

size_to_human(Size, [Unit | Units])
  when Size < 1024;
       length(Units) < 1 ->
    io_lib:format("~.1f ~cB", [Size, Unit]);
size_to_human(Size, [_ | Units]) ->
    size_to_human(Size / 1024, Units).

