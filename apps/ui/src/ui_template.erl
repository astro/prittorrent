-module(ui_template).

-export([render_index/0, render_user/1,
	 render_user_feed/2, export_feed/2]).

-include_lib("model/include/model.hrl").

-record(render_opts, {publisher = false,
		      homepage = false,
		      flattr = false}).

html(HtmlTitle, HeadEls, Contents) ->
    [<<"<?xml version='1.0' encoding='UTF-8'?>\n<!DOCTYPE html>\n">>,
     html:to_iolist(
       {html,
	[{head,
	  [{title, HtmlTitle},
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
	      {a, [{href, <<"/">>}], <<"Bitlove">>}
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
		    [<<"Open Source & Service by ">>,
		     {a, [{href, <<"http://spaceboyz.net/~astro/">>}], <<"Astro">>}
		    ]},
		   {p,
		    [<<"Report Bugs ">>,
		     {a, [{href, <<"https://github.com/astro/prittorrent/issues">>}], <<"on Github!">>}
		    ]}
		  ]},
		 {'div',
		  [{p,
		    [<<"Follow us on Twitter:">>,
		     {br, []},
		     {a, [{href, <<"http://twitter.com/bitlove_org">>}],
		      <<"@bitlove_org">>}
		    ]}
		  ]},
		 {'div',
		  [{p, <<"Are you a podcast publisher?">>},
		   {p, <<"Sign up + add your feeds soon!">>},
		   {p, 
		    [{a, [{href, <<"mailto:mail@bitlove.org">>}],
		      <<"mail@bitlove.org">>}
		    ]}
		  ]}
		]},
	       {script, [{src, <<"/static/flattrdropdown.js">>},
			 {type, <<"text/javascript">>}], <<" ">>}
	      ]}
	]}
      )].


render_link(URL = <<"http://", URL1/binary>>) ->
    render_link(URL, URL1);
render_link(URL = <<"https://", URL1/binary>>) ->
    render_link(URL, URL1);
render_link(URL) ->
    render_link(URL, URL).

render_link(URL, Text1) ->
    Text3 =
	case ends_with_only_slash(Text1) of
	    true ->
		{Text2, _} =
		    split_binary(Text1, size(Text1) - 1),
		Text2;
	    false ->
		Text1
	end,
    {a, [{href, URL}], Text3}.

ends_with_only_slash(<<$/>>) ->
    true;
ends_with_only_slash(<<>>) ->
    false;
ends_with_only_slash(<<$/, _/binary>>) ->
    false;
ends_with_only_slash(<<_, Bin/binary>>) ->
    ends_with_only_slash(Bin).



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


render_item(Opts, #feed_item{user = User,
			     slug = Slug,
			     id = ItemId,
			     feed_title = FeedTitle,
			     published = Published,
			     title = Title,
			     image = Image,
			     homepage = Homepage,
			     payment = Payment}) ->
    ItemLink = ui_link:link_item(User, Slug, ItemId),
    {'div',
     [if
	  is_binary(Image),
	  size(Image) > 0 ->
	      {img, [{src, Image},
		     {class, "logo"}], []};
	  true ->
	      []
      end,
      {'div', [{class, <<"right">>}],
       [case Published of
	    {{Y, Mo, D}, {H, M, _S}} ->
		{p, [{class, "published"}],
		 [io_lib:format("~B-~2..0B-~2..0B",
				[Y, Mo, D]),
		  {br, []},
		  io_lib:format("~2..0B:~2..0B",
				[H, M])
		 ]};
	    _ ->
		[]
	end,
	if
	    Opts#render_opts.flattr ->
		PaymentData1 =
		    if
			is_binary(Payment),
			size(Payment) > 0 ->
			    [{dl, [{class, <<"flattr">>}],
			      [{dt, [<<"Support ">>, User]},
			       {dd,
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
							{"data-flattr-" ++ binary_to_list(K), V}
						end
						|| {K, V} <- cowboy_http:x_www_form_urlencoded(
							       Payment1, fun cowboy_http:urldecode/1)
					       ]],
					 <<"Support the publisher">>};
				    _  ->
					{a, [{href, Payment}],
					 Payment}
				end}
			      ]}];
			true ->
			    []
		    end,
		PaymentData2 =
		    [{dl, [{class, <<"flattr">>}],
		      [{dt, <<"Support bitlove.org">>},
		       {dd,
			{a, [{class, <<"FlattrButton">>},
			     {href, <<"https://flattr.com/profile/Astro">>},
			     {'data-flattr-url', <<(ui_link:base())/binary,
						   ItemLink/binary>>},
			     {'data-flattr-uid', <<"Astro">>},
			     {'data-flattr-title', <<"Torrent for ", Title/binary, " on Bitlove">>},
			     {'data-flattr-description', <<"Torrentification & Seeding">>},
			     {'data-flattr-category', <<"rest">>},
			     {'data-flattr-tags', <<"torrent,bittorrent,p2p,filesharing">>}
			    ], <<"on Flattr">>}
		       }
		      ]}],

		{'div', [{class, <<"flattrdropdown">>},
			 {title, <<"Support the podcaster and Bitlove">>},
			 {'data-payment', [html:to_iolist(PaymentData1), html:to_iolist(PaymentData2)]}
			], <<"Flattr ▾">>};
	    true ->
		[]
	end
       ]},
      {'div',
       [if
	    Opts#render_opts.publisher ->
		{h3, [{class, "feed"}],
		 [{a, [{href, ui_link:link_user_feed(User, Slug)}],
		   [FeedTitle
		   ]}
		 ]};
	    true ->
		[]
	end,

	{h3,
	 [{a, [{href, ItemLink}], Title}
	 ]},
	if
	    Opts#render_opts.homepage,
	    is_binary(Homepage),
	    size(Homepage) > 0 ->
		{p, [{class, "homepage"}],
		 render_link(Homepage)};
	    true ->
		[]
	end
       ]}
     ]}.

render_enclosure(#download{name = Name,
			   info_hash = InfoHash,
			   size = Size,
			   seeders = Seeders,
			   leechers = Leechers,
			   downspeed = Downspeed,
			   downloaded = Downloaded}) ->
    render_torrent(Name, InfoHash, Size, Seeders + 1, Leechers, Downspeed, Downloaded).


render_torrent(Title, InfoHash, Size, Seeders, Leechers, Bandwidth, Downloaded) ->
    {ul, [{class, "download"}],
     [{li, [{class, "torrent"}],
       {a, [{href, ui_link:torrent(InfoHash)}], Title}
      },
      {li, [{class, "stats"}],
       [{span, [{class, "size"},
		{title, "Download size"}], size_to_human(Size)},
	{span, [{class, "d"},
		{title, "Complete downloads"}], integer_to_list(Downloaded)},
	{span, [{class, "s"},
		{title, "Seeders"}], integer_to_list(Seeders)},
	{span, [{class, "l"},
		{title, "Leechers"}], integer_to_list(Leechers)},
	if
	    Bandwidth > 0 ->
		{span, [{class, "bw"},
			{title, "Current Total Bandwidth"}], [size_to_human(Bandwidth), "/s"]};
	    true ->
		[]
	end
       ]}
     ]}.

render_downloads(Opts, Downloads) ->
    lists:map(
      fun(#feed_item{user = User,
		     id = ItemId,
		     downloads = ItemDownloads} = Item) ->
	      {article, [{class, "item"},
			 {id, ItemId}],
	       [render_item(Opts, Item),
		lists:map(fun render_enclosure/1, ItemDownloads),
		if
		    Opts#render_opts.publisher ->
			{p, [{class, "moreby"}],
			 [<<"More by ">>,
			  {a, [{href, ui_link:link_user(User)}],
			   User}
			 ]};
		    true ->
			[]
		end
	       ]}
      end, Downloads).

page_1column(Title,
	     FeedLink, Col) ->
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
    html(Title,
	 HeadEls,
	 [{section, [{class, "col"}], Col}]).

page_2column(Title,
	     Col1, Col2) ->
    page_2column(Title, [], Col1, Col2).

page_2column(Title, Prologue, Col1, Col2) ->
    html(Title, [],
	 [Prologue,
	  {section, [{class, "col1"}], Col1},
	  {section, [{class, "col2"}], Col2}
	 ]).

%% TODO
render_index() ->
    {ok, RecentDownloads} =
	model_enclosures:recent_downloads_without_popular(),
    {ok, PopularDownloads} =
	model_enclosures:popular_downloads(),
    
    page_2column(
      <<"Bitlove: Peer-to-Peer Love for Your Podcast Downloads">>,
      [{'div',
	[{h2, "Recent Torrents"}
	]} |
       render_downloads(#render_opts{publisher = true, flattr = true},
			RecentDownloads)],
      [{'div',
	[{h2, "Popular Torrents"}
	]} |
       render_downloads(#render_opts{publisher = true, flattr = true},
			PopularDownloads)]
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
    {ok, UserFeeds} =
	model_feeds:user_feeds_details(UserName),
    {ok, UserDownloads} =
	model_enclosures:user_downloads(UserName),

    page_2column(
      [UserName, <<" at Bitlove">>],
      {header, [{class, "user"}],
       render_meta(h2, UserTitle, UserImage, UserHomepage)
      },
      [{h2, "Feeds"} |
       lists:map(fun({Slug, _Feed, Title, Homepage, Image}) ->
			 {article, [{class, "feed"}],

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
			  ]}
		 end, UserFeeds)
      ],
      [{h2, "Recent Torrents"} |
       render_downloads(#render_opts{flattr = true}, UserDownloads)
      ]
     ).

render_user_feed(UserName, Slug) ->
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
      [FeedTitle, <<" on Bitlove">>],
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
			UserName}
		      ]}
		    ], FeedImage, FeedHomepage)
       } |
       render_downloads(#render_opts{flattr = true, homepage = true},
			FeedDownloads)
      ]).

export_feed(UserName, Slug) ->
    case model_users:get_feed(UserName, Slug) of
	{ok, FeedURL} ->
	    {ok, FeedXml, ItemXmls, EnclosuresMap} =
		model_feeds:feed_data(FeedURL, 23),
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
	    Body = feeds_parse:serialize(CompleteFeedEl),
	    {ok, Type,
	     [<<"<?xml version='1.0' encoding='UTF-8'?>\n">>,
	      Body]};
	{error, not_found} ->
	    throw({http, 404})
    end.

%%
%% Helpers
%%

size_to_human(Size) when not is_integer(Size) ->
    "∞";
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

