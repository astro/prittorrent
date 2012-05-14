-module(ui_template).

-export([render_login/1, render_signup/1,
	 render_index/1, render_user/2,
	 render_user_feed/3, export_feed/3]).

-include("../include/ui.hrl").
-include_lib("model/include/model.hrl").

-record(render_opts, {title,
		      publisher = false,
		      homepage = false,
		      flattr = false,
		      ui_req}).

html(#render_opts{title = HtmlTitle,
		  ui_req = #req{sid = Sid}
		 }, HeadEls, Contents) ->
    Session = if
		  is_binary(Sid) ->
		      model_session:validate(Sid);
		  true ->
		      undefined
	      end,

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
	    ]},
	   Contents,
	   case Session of
	       {ok, UserName} ->
		   {nav, [{id, "navbar"}],
		    [{p,
		      [<<"Logged in as ">>,
		       {a, [{href, ui_link:link_user(UserName)}], UserName}
		      ]},
		     {p,
		      {a, [{href, "/logout"}], <<"Logout">>}}
		    ]};
	       _ ->
		   []
	   end,
	   {footer,
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

page_1column(Opts,
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
    html(Opts,
	 HeadEls,
	 [{section, [{class, "col"}], Col}]).

page_2column(Title,
	     Col1, Col2) ->
    page_2column(Title, [], Col1, Col2).

page_2column(Opts, Prologue, Col1, Col2) ->
    html(Opts, [],
	 [Prologue,
	  {section, [{class, "col1"}], Col1},
	  {section, [{class, "col2"}], Col2}
	 ]).

render_login(Req) ->
    page_1column(
      #render_opts{title = <<"Bitlove: Login">>,
		   ui_req = Req},
      no_feed,
      [{noscript,
	<<"JavaScript is mandatory beyond this point">>},
       {form, [{class, "login"}],
	[{p,
	  [{label, [{for, "username"}], <<"Username:">>},
	   {input, [{id, "username"}], []}
	  ]},
	 {p,
	  [{label, [{for, "password"}], <<"Password:">>},
	   {input, [{id, "password"},
		    {type, "password"}], []}
	  ]},
	 {p, [{id, "progress"}], []},
	 {input, [{id, "login"},
		  {type, "submit"},
		  {value, "Login"}], []}
	]},
       {script, [{src, <<"/static/jquery-1.7.1.min.js">>},
		 {type, <<"text/javascript">>}], <<" ">>},
       {script, [{src, <<"/static/jsSHA.js">>},
		 {type, <<"text/javascript">>}], <<" ">>},
       {script, [{src, <<"/static/login.js">>},
		 {type, <<"text/javascript">>}], <<" ">>}
       ]).

render_signup(Req) ->
    page_1column(
      #render_opts{title = <<"Bitlove: Signup">>,
		   ui_req = Req},
      no_feed,
      [{noscript,
	<<"JavaScript is mandatory beyond this point">>},
       {form, [{class, "signup"}],
	[{p,
	  [{label, [{for, "username"}], <<"Username:">>},
	   {input, [{id, "username"}], []}
	  ]},
	 {p,
	  [{label, [{for, "email"}], <<"E-Mail:">>},
	   {input, [{id, "email"}], []}
	  ]},
	 {p, [{class, "tos"}],
	  [{input, [{type, "checkbox"},
		    {id, "tos-1"},
		    {value, "tos-1"}], []},
	   {label, [{for, "tos-1"}],
	    <<"The media I am about to publish here is free to copy.">>}
	  ]},
	 {p, [{class, "tos"}],
	  [{input, [{type, "checkbox"},
		    {id, "tos-2"},
		    {value, "tos-2"}], []},
	   {label, [{for, "tos-2"}],
	    <<"I will not use this service for content I am not allowed to (re-)distribute.">>}
	  ]},
	 {input, [{id, "signup"},
		  {type, "submit"},
		  {value, "Signup"}], []}
	]},
       {script, [{src, <<"/static/jquery-1.7.1.min.js">>},
		 {type, <<"text/javascript">>}], <<" ">>},
       {script, [{src, <<"/static/signup.js">>},
		 {type, <<"text/javascript">>}], <<" ">>}
       ]).

render_index(Req) ->
    {ok, RecentDownloads} =
	model_enclosures:recent_downloads_without_popular(),
    {ok, PopularDownloads} =
	model_enclosures:popular_downloads(),
    
    Opts = #render_opts{title = <<"Bitlove: Peer-to-Peer Love for Your Podcast Downloads">>,
			publisher = true,
			flattr = true,
			ui_req = Req},
    page_2column(
      Opts,
      [{'div',
	[{h2, "Recent Torrents"}
	]} |
       render_downloads(Opts, RecentDownloads)],
      [{'div',
	[{h2, "Popular Torrents"}
	]} |
       render_downloads(Opts, PopularDownloads)]
     ).

%% Feeds, Recent Episodes
render_user(Req, UserName) ->
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

    Opts = #render_opts{title = [UserName, <<" at Bitlove">>],
			flattr = true,
			ui_req = Req},

    page_2column(
      Opts,
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
       render_downloads(Opts, UserDownloads)
      ]
     ).

render_user_feed(Req, UserName, Slug) ->
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
    
    Opts = #render_opts{title = [FeedTitle, <<" on Bitlove">>],
			flattr = true,
			homepage = true,
			ui_req = Req},
    
    page_1column(
      Opts,
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
       render_downloads(Opts, FeedDownloads)
      ]).

export_feed(_Req, UserName, Slug) ->
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

