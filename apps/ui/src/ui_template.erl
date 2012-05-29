-module(ui_template).

-export([render_message/2, render_error/1,
	 render_login/1, render_signup/1,
	 render_activate/3, render_reactivate/1,
	 render_front/1,
	 render_new/1, render_top/1, render_directory/1,
	 render_user/2,
	 render_user_feed/3, export_feed/3]).

-include("../include/ui.hrl").
-include_lib("model/include/model.hrl").

-record(render_opts, {title,
		      publisher = false,
		      homepage = false,
		      flattr = false,
		      item_id_unique = false,
		      ui_req}).

-define(SCRIPT_TAG(Src),
	{script, [{src, Src},
		  {type, <<"text/javascript">>}], <<" ">>}).
-define(INCLUDE_JQUERY,
	?SCRIPT_TAG(<<"/static/jquery-1.7.1.min.js">>)).
-define(INCLUDE_JSSHA,
	?SCRIPT_TAG(<<"/static/jsSHA.js">>)).

-define(SCRIPT_FLATTR,
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
	}).

html(#render_opts{title = HtmlTitle,
		  ui_req = #req{session_user = SessionUser}
		 }, HeadEls, Contents) ->
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
	     {p, [{class, "slogan"}], "Peer-to-Peer Love for Your Podcast Downloads"},
	     {p, [{class, "siteflattr"}],
	      {a, [{class, "FlattrButton"},
		   {style, "display:none;"},
		   {href, "http://bitlove.org/"},
		   {rev, "flattr;button:large;"}],
	       {img, [{src, "http://api.flattr.com/button/flattr-badge-large.png"},
		      {alt, "Flattr this"},
		      {title, "Flattr this"},
		      {border, "0"}], []}
	      }}
	    ]},
	   {nav, [{class, "navtabs"}],
	    {ul,
	     [{li,
	       {a, [{href, "/new"}], <<"New">>}},
	      {li,
	       {a, [{href, "/top"}], <<"Top">>}},
	      {li,
	       {a, [{href, "/directory"}], <<"Directory">>}}
	     ]}},
	   {'div', [{class, "content"}], Contents},
	   if
	       is_binary(SessionUser) ->
		   {nav, [{id, "navbar"}],
		    [{p,
		      [<<"Logged in as ">>,
		       {a, [{href, ui_link:link_user(SessionUser)}], SessionUser}
		      ]},
		     {p,
		      {a, [{href, "/logout"}], <<"Logout">>}}
		    ]};
	       true ->
		   []
	   end,
	   {footer,
	    [{'div',
	      [{p,
		[<<"Twitter:">>,
		 {br, []},
		 {a, [{href, <<"http://twitter.com/bitlove_org">>}],
		  <<"@bitlove_org">>}
		]}
	      ]},
	     {'div',
	      [{p, <<"Contact:">>},
	       {p, 
		[{a, [{href, <<"mailto:mail@bitlove.org">>}],
		  <<"mail@bitlove.org">>}
		]}
	      ]},
	     {'div',
	      [{p,
		[<<"100% ">>,
		 {a, [{href, <<"https://github.com/astro/prittorrent">>}], <<"Open Source">>}
		]},
	       {p,
		<<"Bitlove.org is IPv6-ready!">>}
	      ]}
	    ]},
	   ?SCRIPT_FLATTR
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
    render_meta(Heading, Title, Image, Homepage, []).

render_meta(Heading, Title, Image, Homepage, Trailing) ->
    {'div', [{class, "meta"}],
     [if
	  is_binary(Image),
	  size(Image) > 0 ->
	      {img, [{src, Image},
		     {class, "logo"}], []};
	  true ->
	      []
      end,
      {'div', [{class, "title"}],
       [{Heading, Title},
	if
	    is_binary(Homepage),
	    size(Homepage) > 0 ->
		{p, [{class, "homepage"}],
		 render_link(Homepage)};
	    true ->
		[]
	end,
	Trailing
      ]}
    ]}.


render_item(Opts, #feed_item{user = User,
			     slug = Slug,
			     id = ItemId,
			     feed_title = FeedTitle,
			     published = {PublishedDate, {PublishedHour, PublishedMin, _}},
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
       [
	%% Avoid conversion with floating seconds
	case (catch erlang:universaltime_to_localtime({PublishedDate, {PublishedHour, PublishedMin, 0}})) of
	    %% No interest in seconds at all
	    {{Y, Mo, D}, {H, M, _S}} ->
		DateStr = io_lib:format("~B-~2..0B-~2..0B",
					[Y, Mo, D]),
		TimeStr = io_lib:format("~2..0B:~2..0B",
					[H, M]),
		{p, [{class, "published"}],
		 [case calendar:local_time() of
		      {{Y1, Mo1, D1}, _}
			when Y1 == Y,
			     Mo1 == Mo,
			     D1 == D ->
			  {span, [{class, "today"}], DateStr};
		      _ ->
			  DateStr
		  end,
		  {br, []},
		  TimeStr
		 ]};
	    _ ->
		[]
	end,
	if
	    Opts#render_opts.flattr,
	    is_binary(Payment),
	    size(Payment) > 0 ->
		[{'div', [{class, <<"flattr">>}],
		  case Payment of
		      %% Transform an autosubmit link to Flattr button
		      <<"https://flattr.com/submit/auto?", Payment1/binary>> ->
			  {a, [{class, <<"FlattrButton">>},
			       {rel, <<"payment">>},
			       {href, Payment} |
			       [case K of
				    <<"user_id">> ->
					{"data-flattr-uid", V};
				    _ ->
					K2 = [C
					      || C <- binary_to_list(K),
						 C =/= $&,
						 C =/= $;],
					{"data-flattr-" ++ K2, V}
				end
				|| {K, V} <- cowboy_http:x_www_form_urlencoded(
					       Payment1, fun cowboy_http:urldecode/1)
			       ]], <<"[Flattr]">>};
		      <<"http://flattr.com/", _/binary>> ->
			  {a, [{class, <<"FlattrButton">>},
			       {href, Payment},
			       {'data-flattr-url',
				if
				    is_binary(Homepage) -> Homepage;
				    true -> ""
				end},
			       {rel, <<"payment">>}], <<"[Flattr]">>};
		      _ ->
			  {a, [{href, Payment},
			       {rel, <<"payment">>}], <<"[Support]">>}
		  end
		 }];
	    true ->
		[]
	end
       ]},
      {'div', [{class, "title"}],
       [{h3,
	 [{a, [{href, ItemLink}], Title}
	 ]},
	if
	    Opts#render_opts.publisher ->
		{p, [{class, "feed"}],
		 [<<"in ">>,
		  {a, [{href, ui_link:link_user_feed(User, Slug)}],
		   FeedTitle},
		  <<" by ">>,
		  {a, [{href, ui_link:link_user(User)}],
		   User}
		 ]};
	    true ->
		[]
	end,
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

render_enclosure(#download{user = UserName,
			   slug = Slug,
			   name = Name,
			   size = Size,
			   seeders = Seeders,
			   leechers = Leechers,
			   downspeed = Downspeed,
			   downloaded = Downloaded}, ShowName) ->
    {ul, [{class, "download"}],
     [{li, [{class, "torrent"}],
       {a, [{href, ui_link:torrent(UserName, Slug, Name)},
	    {rel, "enclosure"}],
	[if
	     ShowName ->
		 [Name, $ ];
	     true ->
		 <<"Download ">>
	 end,
	 {span, [{class, "size"},
		 {title, "Download size"}], size_to_human(Size)}
	]}
      },
      {li, [{class, "stats"}],
       [if
	    Downspeed > 0 ->
		[DownspeedN, DownspeedUnit] = size_to_human(Downspeed),
		{dl,
		 [{dt, [DownspeedN, {span, [{class, "unit"}], [DownspeedUnit, "/s"]}]},
		  {dd, <<"Download speed">>}
		 ]};
	    true ->
		[]
	end,
	{dl,
	 [{dt, integer_to_list(Seeders + 1)},
	  {dd, <<"Seeders">>}
	 ]},
	{dl,
	 [{dt, integer_to_list(Leechers)},
	  {dd, <<"Leechers">>}
	 ]},
	{dl,
	 [{dt, integer_to_list(Downloaded)},
	  {dd, <<"Downloads">>}
	 ]}
       ]},
      {li, [{class, "graphs"}],
       [{img, [{src, <<"/", UserName/binary, "/", Slug/binary, "/", Name/binary, "/g/month/swarm.svg">>}], []},
	{img, [{src, <<"/", UserName/binary, "/", Slug/binary, "/", Name/binary, "/g/month/traffic.svg">>}], []},
	{img, [{src, <<"/", UserName/binary, "/", Slug/binary, "/", Name/binary, "/g/month/downloads.svg">>}], []}
       ]}
     ]}.

render_downloads(Opts, Downloads) ->
    lists:map(
      fun(#feed_item{id = ItemId,
		     downloads = ItemDownloads} = Item) ->
	      %% Prepare whether to display just "Download" or filenames
	      RenderEnclosure =
		  case length(ItemDownloads) of
		      1 ->
			  fun(Download) ->
				  render_enclosure(Download, false)
			  end;
		      _ ->
			  fun(Download) ->
				  render_enclosure(Download, true)
			  end
		  end,

	      {article, [{class, "item"} |
			 if
			     Opts#render_opts.item_id_unique ->
				 [{id, ItemId}];
			     true ->
				 []
			 end],
	       [render_item(Opts, Item),
		lists:map(RenderEnclosure, ItemDownloads)
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
		[{link, [{rel, <<"alternate">>},
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

%% Used for signup response
render_message(Req, Msg) ->
    page_1column(
      #render_opts{title = <<"Bitlove">>,
		   ui_req = Req},
      no_feed,
      {p, Msg}
     ).

render_error(403) ->
    page_1column(
      #render_opts{title = <<"Bitlove: Forbidden">>,
		   ui_req = #req{}},
      no_feed,
      {h2, <<"403: Forbidden">>}
     );

render_error(404) ->
    page_1column(
      #render_opts{title = <<"Bitlove: Not found">>,
		   ui_req = #req{}},
      no_feed,
      [{h2, <<"404: Not found">>},
       {img, [{src, <<"/static/404.jpg">>}], []},
       {p, [{class, "hint"}], <<"Here's a kitten instead.">>}
      ]
     );

render_error(500) ->
    page_1column(
      #render_opts{title = <<"Bitlove: Oops">>,
		   ui_req = #req{}},
      no_feed,
      [{h2, <<"500: Oops">>},
       {img, [{src, <<"/static/500.jpg">>}], []}
      ]
     );

render_error(Status) ->
    page_1column(
      #render_opts{title = <<"Bitlove: Oops">>,
		   ui_req = #req{}},
      no_feed,
      [{h2, <<"HTTP ", (list_to_binary(integer_to_list(Status)))/binary>>}
      ]
     ).


render_login(Req) ->
    page_1column(
      #render_opts{title = <<"Bitlove: Login">>,
		   ui_req = Req},
      no_feed,
      [{noscript,
	<<"JavaScript is mandatory beyond this point">>},
       {form, [{class, "login r"}],
	[{h2, <<"Podcaster Login">>},
	 {p,
	  [{label, [{for, "username"}], <<"Username:">>},
	   {input, [{id, "username"}], []}
	  ]},
	 {p,
	  [{label, [{for, "password"}], <<"Password:">>},
	   {input, [{id, "password"},
		    {type, "password"}], []}
	  ]},
	 {p, [{id, "progress"}], <<"">>},
	 {input, [{id, "login"},
		  {type, "submit"},
		  {value, "Login"}], []},
	 {p,
	  {a, [{href, "/reactivate"}], <<"Forgot password?">>}}
	]},
       ?INCLUDE_JQUERY,
       ?INCLUDE_JSSHA,
       ?SCRIPT_TAG(<<"/static/login.js">>)
       ]).

render_signup(Req) ->
    page_1column(
      #render_opts{title = <<"Bitlove: Signup">>,
		   ui_req = Req},
      no_feed,
      [{noscript,
	<<"JavaScript is mandatory beyond this point">>},
       {form, [{class, "signup"},
	       {method, "POST"},
	       %{enctype, "application/x-www-form-urlencoded"},
	       {action, "/signup"}],
	[{h2, <<"Podcaster Signup">>},
	 {p,
	  [{label, [{for, "username"}], <<"Username:">>},
	   {input, [{id, "username"},
		    {name, "username"}], []},
	   {span, [{class, "hint"}], <<"">>}
	  ]},
	 {p,
	  [{label, [{for, "email"}], <<"E-Mail:">>},
	   {input, [{id, "email"},
		    {name, "email"}], []}
	  ]},
	 {p, [{class, "tos"}],
	  [{input, [{type, "checkbox"},
		    {id, "tos-1"},
		    {name, "tos-1"},
		    {value, "tos-1"}], []},
	   {label, [{for, "tos-1"}],
	    <<" The media I am about to publish here is free to copy.">>}
	  ]},
	 {p, [{class, "tos"}],
	  [{input, [{type, "checkbox"},
		    {id, "tos-2"},
		    {name, "tos-2"},
		    {value, "tos-2"}], []},
	   {label, [{for, "tos-2"}],
	    <<" I will use this service only for content I am allowed to (re-)distribute.">>}
	  ]},
	 {input, [{id, "signup"},
		  {type, "submit"},
		  {value, "Signup"}], []}
	]},
       ?INCLUDE_JQUERY,
       ?SCRIPT_TAG(<<"/static/signup.js">>)
       ]).

render_activate(Req, HexToken, HexSalt) ->
    page_1column(
      #render_opts{title = <<"Bitlove: Activate your account">>,
		   ui_req = Req},
      no_feed,
      [{noscript,
	<<"JavaScript is mandatory beyond this point">>},
       {form, [{class, "login r"}],
	[{h2, <<"Activate your account">>},
	 {p,
	  [{label, [{for, "password1"}], <<"Password:">>},
	   {input, [{id, "password1"},
		    {type, "password"}], []}
	  ]},
	 {p,
	  [{label, [{for, "password2"}], <<"Repeat:">>},
	   {input, [{id, "password2"},
		    {type, "password"}], []}
	  ]},
	 {p, [{id, "progress"}], <<"">>},
	 {input, [{id, "activate"},
		  {type, "submit"},
		  {value, "Activate"},
		  {'data-token', HexToken},
		  {'data-salt', HexSalt}
		 ], []}
	]},
       ?INCLUDE_JQUERY,
       ?SCRIPT_TAG(<<"/static/jsSHA.js">>),
       ?SCRIPT_TAG(<<"/static/activate.js">>)
       ]).

render_reactivate(Req) ->
    page_1column(
      #render_opts{title = <<"Bitlove: Activate your account">>,
		   ui_req = Req},
      no_feed,
      [{form, [{class, "login r"},
	       {method, "POST"},
	       {enctype, "application/x-www-form-urlencoded"},
	       {action, "/reactivate"}],
	[{h2, <<"Reactivate your account">>},
	 {p,
	  [{label, [{for, "email"}], <<"E-Mail:">>},
	   {input, [{id, "email"},
		    {name, "email"}], []}
	  ]},
	 {input, [{type, "submit"},
		  {value, "Ok"}], []}
	]}
       ]).

render_front(Req) ->
    {ok, RecentDownloads} =
	model_enclosures:recent_downloads(3),
    
    Opts = #render_opts{title = <<"Bitlove: Peer-to-Peer Love for Your Podcast Downloads">>,
			publisher = true,
			flattr = true,
			ui_req = Req},
    page_2column(
      Opts,
      [{'p', [{class, "about"}],
	[{b, <<"Bitlove">>},
	 <<" is the fully ">>,
	 {b, <<"automatic">>},
	 <<" Podcast download service on ">>,
	 {b, <<"P2P speed">>},
	 <<". We generate a ">>,
	 {b, <<"Torrent">>},
	 <<" for all media files of an ">>,
	 {b, <<"RSS feed">>},
	 <<" and will ">>,
	 {b, <<"seed">>},
	 <<" them all the time.">>
	]},
       %% {h2, <<"Are You a Podcast Publisher?">>},
       %% {p, [{class, "about"}],
       %% 	<<"Sign up soon!">>},
       {'div', [{class, "navtabs"}],
	{ul,
	 [{li, <<"Sign up soon!">>},
	  {li,
	   {a, [{href, "/login"}],
	    <<"Podcaster Login">>}}
	 ]}
       }
      ],
      {'div', render_downloads(Opts, RecentDownloads)}
     ).

render_new(Req) ->
    {ok, RecentDownloads} =
	model_enclosures:recent_downloads(30),
    
    Opts = #render_opts{title = <<"Bitlove: New torrents">>,
			publisher = true,
			flattr = true,
			ui_req = Req},
    page_1column(
      Opts,
      no_feed,
      {'div',
       [{h2, <<"New Torrents">>} |
	render_downloads(Opts, RecentDownloads)]}
     ).

render_top(Req) ->
    {ok, PopularDownloads} =
	model_enclosures:popular_downloads(30),
    
    Opts = #render_opts{title = <<"Bitlove: Top torrents">>,
			publisher = true,
			flattr = true,
			ui_req = Req},
    page_1column(
      Opts,
      no_feed,
      {'div', 
       [{h2, <<"Top Torrents">>} |
	render_downloads(Opts, PopularDownloads)]}
     ).

render_directory_item({User, Title, Image, Feeds}) ->
    {article, [{class, "directory"}],
     [{'div', [{class, "meta"}],
       [if
	    is_binary(Image),
	    size(Image) > 0 ->
		{img, [{src, Image},
		       {class, "logo"}], []};
	    true ->
		[]
	end,
	{'div', [{class, "title"}],
	 {h2, 
	  {a, [{href, ui_link:link_user(User)}], Title}}
	},
	{ul, [{class, "feeds"}],
	 [{li,
	   {a, [{href, ui_link:link_user_feed(User, Slug)}],
	    FeedTitle}
	  } || {Slug, FeedTitle} <- Feeds]
	}
       ]}
     ]}.

render_directory(Req) ->
    Directory = model_feeds:get_directory(),
    {Directory1, Directory2} =
	lists:split(trunc(length(Directory) / 2), Directory),

    Opts = #render_opts{title = <<"Bitlove: Directory">>,
			publisher = true,
			flattr = true,
			ui_req = Req},

    page_2column(
      Opts,
      [{h2, <<"Directory of Torrentified Podcasters">>}],
      lists:map(fun render_directory_item/1, Directory1),
      lists:map(fun render_directory_item/1, Directory2)
     ).

%% Feeds, Recent Episodes
render_user(#req{session_user = SessionUser} = Req, UserName) ->
    IsSelf = SessionUser == UserName,

    {UserTitle, UserImage, UserHomepage} =
	case model_users:get_details(UserName) of
	    {ok, Title1, Image1, Homepage1} ->
		{Title1, Image1, Homepage1};
	    {error, not_found} ->
		throw({http, 404})
	end,
    {ok, UserFeeds} =
	model_feeds:user_feeds_details(UserName, IsSelf),
    {ok, UserDownloads} =
	model_enclosures:user_downloads(UserName, 20),

    Opts = #render_opts{title = [UserName, <<" at Bitlove">>],
			flattr = true,
			ui_req = Req},
    io:format("SessionUser: ~p\tUserName: ~p~n", [SessionUser, UserName]),
    page_2column(
      Opts,
      {header, [{class, "user"}],
       render_meta(h2, UserTitle, UserImage, UserHomepage)},
      [{h2, "Feeds"} |
       lists:map(fun({Slug, _Feed, Title, Homepage, Image, Public}) ->
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
			       end,
			       case Public of
				   false ->
				       {p, [{class, "hint"}], <<"Private">>};
				   _ ->
				       []
			       end
			      ]}
			    ]}
			  ]}
		 end, UserFeeds)
      ],
      [{h2, "Recent Torrents"},
       render_downloads(Opts, UserDownloads),
       if
	   IsSelf ->
	       [?INCLUDE_JQUERY,
		?SCRIPT_TAG(<<"/static/edit-user.js">>)];
	   true ->
	       []
       end
      ]
     ).

render_user_feed(#req{session_user = SessionUser} = Req, UserName, Slug) ->
    case model_feeds:user_feed_details(UserName, Slug) of
	{error, not_found} ->
	    throw({http, 404});
	{ok, FeedURL, FeedTitle, FeedHomepage, FeedImage, _FeedPublic, FeedTorrentify} ->
	    {ok, FeedDownloads} =
		model_enclosures:feed_downloads(FeedURL, 100),
    
	    Opts = #render_opts{title = [FeedTitle, <<" on Bitlove">>],
				flattr = true,
				homepage = true,
				item_id_unique = true,
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
			    ],
			    FeedImage,
			    FeedHomepage,
			    case FeedTorrentify of
				true ->
				    [];
				_ ->
				    {p, [{class, "hint"}],
				     <<"The feed is pending operator confirmation before automatic torrentification can happen.">>}
			    end
			   )
	       },
	       render_downloads(Opts, FeedDownloads) |
	       if
		   SessionUser == UserName ->
		       [?INCLUDE_JQUERY,
			?SCRIPT_TAG(<<"/static/edit-feed.js">>)];
		   true ->
		       []
	       end
	      ])
    end.

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
					<<Name/binary>> ->
					    <<(ui_link:base())/binary,
					      (ui_link:torrent(UserName, Slug, Name))/binary>>;
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
    [integer_to_list(Size), " B"];
size_to_human(Size) ->
    size_to_human(Size / 1024, "KMGT").

size_to_human(Size, [Unit | _])
  when Size < 10 ->
    [io_lib:format("~.1f", [Size]),
     io_lib:format(" ~cB", [Unit])];
size_to_human(Size, [Unit | Units])
  when Size < 1024;
       length(Units) < 1 ->
    [integer_to_list(round(Size)),
     io_lib:format(" ~cB", [Unit])];
size_to_human(Size, [_ | Units]) ->
    size_to_human(Size / 1024, Units).

