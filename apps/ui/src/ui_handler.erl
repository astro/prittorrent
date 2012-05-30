-module(ui_handler).
-export([init/3, handle/2, terminate/2]).

-behaviour(cowboy_http_handler).

-include("../include/ui.hrl").

-define(HTML_HEADERS, [{<<"Content-Type">>, <<"text/html; charset=UTF-8">>}]).
%% TODO: make configurable
-define(COOKIE_OPTS, [%{secure, true},
		      {http_only, true},
		      {max_age, 30 * 24 * 60 * 60},
		      {path, <<"/">>}
		     ]).


init({_, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    T1 = util:get_now_us(),
    {Method, _} = cowboy_http_req:method(Req),
    {Path, _} = cowboy_http_req:path(Req),
    {RawPath, _} = cowboy_http_req:raw_path(Req),
    case (catch handle_request1(Req)) of
	{ok, Status, Headers, Cookies, Body} ->
	    Req3 = lists:foldl(
		     fun({CookieName, CookieValue}, Req2) ->
			     io:format("Set cookie ~s: ~p~n", [CookieName, CookieValue]),
			     {ok, Req3} =
				 cowboy_http_req:set_resp_cookie(
				   CookieName, CookieValue,
				   ?COOKIE_OPTS, Req2),
			     Req3
		     end, Req, Cookies),
	    {ok, Req4} = cowboy_http_req:reply(Status, Headers, Body, Req3),
	    T2 = util:get_now_us(),
	    io:format("[~.1fms] ui_handler ~s ~p~n", [(T2 - T1) / 1000, Method, RawPath]),

	    count_request(Method, Path),
	    {ok, Req4, State};
	{http, Status} ->
	    T2 = util:get_now_us(),
	    io:format("[~.1fms] ui_handler ~B ~s ~p~n", [(T2 - T1) / 1000, Status, Method, RawPath]),
	    {ok, Req2} = cowboy_http_req:reply(Status, ?HTML_HEADERS, ui_template:render_error(Status), Req),
	    {ok, Req2, State};
	E ->
	    io:format("Error handling ~s ~p:~n~p~n", [Method, RawPath, E]),
	    {ok, Req2} = cowboy_http_req:reply(500, ?HTML_HEADERS, ui_template:render_error(500), Req),
	    {ok, Req2, State}
    end.		    

terminate(_Req, _State) ->
    ok.

html_ok(Body) ->
    {ok, 200, ?HTML_HEADERS, [], Body}.

json_ok(JSON) ->
    json_ok(JSON, []).
json_ok(JSON, Cookies) ->
    Body = rfc4627:encode(JSON),
    {ok, 200, [{<<"Content-Type">>, ?MIME_JSON}], Cookies, Body}.

%% Attention: last point where Req is a cowboy_http_req, not a #req{}
handle_request1(Req) ->
    Method = case cowboy_http_req:method(Req) of
		 {'HEAD', _} ->
		     %% Implicitly support HEAD, cowboy omits the body
		     %% for us
		     'GET';
		 {Method1, _} ->
		     Method1
	     end,
    {Path, _} = cowboy_http_req:path(Req),
    {Encodings, _} = cowboy_http_req:parse_header('Accept-Encoding', Req),
    {Languages, _} = cowboy_http_req:parse_header('Accept-Language', Req),
    {HexSid, _} = cowboy_http_req:cookie(<<"sid">>, Req),
    Sid =
	case (catch util:hex_to_binary(HexSid)) of
	    <<Sid1/binary>> when size(Sid1) > 0 ->
		Sid1;
	    _ ->
		undefined
	end,
    io:format("Encodings: ~p~nLanguages: ~p~nSid: ~p~n", [Encodings,Languages,HexSid]),
    Body = if
	       Method =:= 'POST';
	       Method =:= 'PUT' ->
		   {Body1, _Req2} = cowboy_http_req:body_qs(Req),
		   io:format("BodyQS: ~p~n~p~n", [Body1, _Req2]),
		   Body1;
	       true ->
		   []
	   end,

    handle_request2(#req{method = Method,
			 path = Path,
			 encodings = Encodings,
			 languages = Languages,
			 sid = Sid,
			 body = Body
			}).

%% Redirect to static handler
handle_request2(#req{method = 'GET',
		     path = [<<"favicon.", _:3/binary>>]
		    }) ->
    {ok, 301, [{<<"Location">>, <<"/static/favicon.png">>}], [], <<>>};

%% Login page
handle_request2(#req{method = 'GET',
		     path = [<<"login">>]} = Req) ->
    html_ok(ui_template:render_login(Req));

handle_request2(#req{method = 'POST',
		     path = [<<"login">>],
		     body = Body}) ->
    UserName = proplists:get_value(<<"username">>, Body, undefined),
    HexToken = proplists:get_value(<<"token">>, Body, undefined),
    HexResponse = proplists:get_value(<<"response">>, Body, undefined),

    if
	%% Just a token request
	is_binary(UserName) ->
	    case model_users:get_salted(UserName) of
		{ok, _Salted, Salt} ->
		    {ok, Token} = model_token:generate(login, UserName),
		    SaltHex = util:binary_to_hex(Salt),
		    TokenHex = util:binary_to_hex(Token),
		    json_ok({obj, [{salt, SaltHex},
				   {token, TokenHex}
				  ]});
		{error, not_found} ->
		    json_ok({obj, [{error, <<"No such user">>}]})
	    end;
	%% Login attempt
	is_binary(HexToken),
	is_binary(HexResponse) ->
	    Token = util:hex_to_binary(HexToken),
	    case model_token:validate(login, Token) of
		{ok, UserName1} ->
		    {ok, Salted, _Salt} = model_users:get_salted(UserName1),
		    %% check challenge response
		    io:format("Salt: ~s~nSalted: ~s~nToken: ~s~n",
			      [util:binary_to_hex(_Salt), util:binary_to_hex(Salted), HexToken]),
		    ExpectedResponse = hmac(Token, util:binary_to_hex(Salted)),
		    ChallengeResponse = util:hex_to_binary(HexResponse),
		    if
			ChallengeResponse == ExpectedResponse ->
			    %% create & set Sid
			    {ok, Sid} = model_session:generate(UserName1),
			    %% reply with new cookie sid
			    HomeLink = iolist_to_binary(
					 ui_link:link_user(UserName1)),
			    io:format("Sid: ~p~n", [Sid]),
			    json_ok({obj, [{welcome, HomeLink}]},
				    [{<<"sid">>, util:binary_to_hex(Sid)}
				    ]);
			true ->
			    io:format("Wrong password for ~p~nChallengeResponse: ~s~nExpectedResponse: ~s~n", [UserName1, util:binary_to_hex(ChallengeResponse), util:binary_to_hex(ExpectedResponse)]),
			    json_ok({obj, [{error, <<"Wrong password">>}]})
		    end;
		{error, wrong_password} ->
		    json_ok({obj, [{error, <<"Invalid token, please retry">>}]})
	    end
    end;

%% Signup page
handle_request2(#req{method = 'GET',
		     path = [<<"signup">>]} = Req) ->
    html_ok(ui_template:render_signup(Req));

handle_request2(#req{method = 'POST',
		     path = [<<"signup">>],
		     body = Body} = Req) ->
    UserName = proplists:get_value(<<"username">>, Body),
    Email = proplists:get_value(<<"email">>, Body),
    TOS1 = proplists:get_value(<<"tos-1">>, Body),
    TOS2 = proplists:get_value(<<"tos-2">>, Body),
    %% Validate
    case {validate_username(UserName),
	  validate_email(Email),
	  TOS1,
	  TOS2
	 } of
	{false, _, _, _} ->
	    html_ok(ui_template:render_message(Req, <<"Invalid username :-(">>));
	{true, false, _, _} ->
	    html_ok(ui_template:render_message(Req, <<"That is not an e-mail address!">>));
	{true, true, _, _}
	  when TOS1 =/= <<"tos-1">>;
	       TOS2 =/= <<"tos-2">> ->
	    html_ok(ui_template:render_message(Req, <<"You need to agree to our terms. We do not wish to get sued.">>));
	{true, true, <<"tos-1">>, <<"tos-2">>} ->
	    case create_account(UserName, Email) of
		ok ->
		    html_ok(ui_template:render_message(Req, <<"Check your mail!">>));
		{error, Message} ->
		    html_ok(ui_template:render_message(Req, Message))
	    end;
	_ ->
	    html_ok(ui_template:render_message(Req, <<"Invalid account data">>))
    end;

%% Signup activation & password reset
handle_request2(#req{method = 'GET',
		     path = [<<"activate">>, HexToken]} = Req) ->
    Token = util:hex_to_binary(HexToken),
    case model_token:peek(activate, Token) of
	{ok, UserName} ->
	    {ok, _Salted, Salt} = model_users:get_salted(UserName),
	    HexSalt = util:binary_to_hex(Salt),
	    html_ok(ui_template:render_activate(Req, HexToken, HexSalt));
	{error, invalid_token} ->
	    %% TODO: link to pw reset form
	    html_ok(ui_template:render_message(Req, <<"Invalid activation token">>))
    end;

handle_request2(#req{method = 'POST',
		     path = [<<"activate">>, HexToken],
		     body = Body}) ->
    Token = util:hex_to_binary(HexToken),
    HexSalted = proplists:get_value(<<"salted">>, Body, undefined),
    Salted = util:hex_to_binary(HexSalted),
    case model_token:validate(activate, Token) of
	{ok, UserName} ->
	    %% Set for new password
	    model_users:set_salted(UserName, Salted),
	    %% Create session
	    {ok, Sid} = model_session:generate(UserName),
	    %% reply with new cookie sid
	    HomeLink = iolist_to_binary(
			 ui_link:link_user(UserName)),
	    io:format("Sid: ~p~n", [Sid]),
	    json_ok({obj, [{welcome, HomeLink}]},
		    %% FIXME:
		    [{<<"sid">>, util:binary_to_hex(Sid)}
		    ]);
	{error, invalid_token} ->
	    json_ok({obj, [{error, <<"Invalid activation token">>}]})
    end;

handle_request2(#req{method = 'GET',
		     path = [<<"reactivate">>]} = Req) ->
    html_ok(ui_template:render_reactivate(Req));

handle_request2(#req{method = 'POST',
		     path = [<<"reactivate">>],
		     body = Body} = Req) ->
    Email = proplists:get_value(<<"email">>, Body, undefined),
    case model_users:get_by_email(Email) of
	[] ->
	    html_ok(ui_template:render_message(Req, <<"Not found">>));
	Users ->
	    lists:foreach(fun(User) ->
				  reactivate_user(User, Email)
			  end, Users),
	    html_ok(ui_template:render_message(Req, <<"Check your mail!">>))
    end;

handle_request2(#req{method = 'GET',
		     path = [<<"logout">>],
		     sid = Sid
		    }) ->
    if
	is_binary(Sid) ->
	    model_session:invalidate(Sid);
	true ->
	    ignore
    end,
    {ok, 303, [{<<"Location">>, <<"/login">>}], [{<<"sid">>, <<>>}], <<"Bye">>};

%% Torrent download by info_hash
handle_request2(#req{method = 'GET',
		     path = [<<"t">>, <<InfoHashHex:40/binary, ".torrent">>]
		    }) ->
    InfoHash = util:hex_to_binary(InfoHashHex),
    case model_torrents:get_torrent(InfoHash) of
	{ok, Name, Torrent} ->
	    NameE = escape_bin(Name, $\"),
	    Headers =
		[{<<"Content-Type">>, ?MIME_TORRENT},
		 {<<"Content-Disposition">>,
		  <<"attachment; filename=\"", NameE/binary, ".torrent\"">>}],
	    {ok, 200, Headers, [], Torrent};
	{error, not_found} ->
	    throw({http, 404})
    end;

%% Front page
handle_request2(#req{method = 'GET',
		     path = []
		    } = Req) ->
    html_ok(ui_template:render_front(validate_session(Req)));

handle_request2(#req{method = 'GET',
		     path = [<<"new">>]
		    } = Req) ->
    html_ok(ui_template:render_new(validate_session(Req)));

handle_request2(#req{method = 'GET',
		     path = [<<"new.rss">>]
		    } = Req) ->
    {ok, Body} = ui_template:export_downloads(rss, Req, new),
    {ok, 200,
     [{<<"Content-Type">>, ?MIME_RSS}], [],
     Body};

handle_request2(#req{method = 'GET',
		     path = [<<"new.atom">>]
		    } = Req) ->
    {ok, Body} = ui_template:export_downloads(atom, Req, new),
    {ok, 200,
     [{<<"Content-Type">>, ?MIME_ATOM}], [],
     Body};

handle_request2(#req{method = 'GET',
		     path = [<<"top">>]
		    } = Req) ->
    html_ok(ui_template:render_top(validate_session(Req)));

handle_request2(#req{method = 'GET',
		     path = [<<"top.rss">>]
		    } = Req) ->
    {ok, Body} = ui_template:export_downloads(rss, Req, top),
    {ok, 200,
     [{<<"Content-Type">>, ?MIME_RSS}], [],
     Body};

handle_request2(#req{method = 'GET',
		     path = [<<"top.atom">>]
		    } = Req) ->
    {ok, Body} = ui_template:export_downloads(atom, Req, top),
    {ok, 200,
     [{<<"Content-Type">>, ?MIME_ATOM}], [],
     Body};

handle_request2(#req{method = 'GET',
		     path = [<<"directory">>]
		    } = Req) ->
    html_ok(ui_template:render_directory(validate_session(Req)));

%% User profile
handle_request2(#req{method = 'GET',
		     path = [<<UserName/binary>>]
		    } = Req) ->
    html_ok(ui_template:render_user(validate_session(Req), UserName));

handle_request2(#req{method = 'GET',
		     path = [<<UserName/binary>>, <<"downloads.rss">>]
		    } = Req) ->
    {ok, Body} = ui_template:export_downloads(rss, Req, UserName),
    {ok, 200,
     [{<<"Content-Type">>, ?MIME_RSS}], [],
     Body};

handle_request2(#req{method = 'GET',
		     path = [<<UserName/binary>>, <<"downloads.atom">>]
		    } = Req) ->
    {ok, Body} = ui_template:export_downloads(atom, Req, UserName),
    {ok, 200,
     [{<<"Content-Type">>, ?MIME_ATOM}], [],
     Body};

%% User profile as json
handle_request2(#req{method = 'GET',
		     path = [<<UserName/binary>>, <<"details.json">>]
		    } = Req) ->
    #req{session_user = SessionUser} = validate_session(Req),
    if
	SessionUser == UserName ->
	    case model_users:get_details(UserName) of
		{ok, Title, Image, Homepage} ->
		    json_ok({obj, [{title, Title},
				   {image, Image},
				   {homepage, Homepage}
				  ]});
		{error, not_found} ->
		    throw({http, 404})
	    end;
	true ->	    
	    throw({http, 403})
    end;

handle_request2(#req{method = 'POST',
		     path = [<<UserName/binary>>, <<"details.json">>],
		     body = Body
		    } = Req) ->
    #req{session_user = SessionUser} = validate_session(Req),
    if
	SessionUser == UserName ->
	    io:format("Body: ~p~n", [Body]),
	    Title = proplists:get_value(<<"title">>, Body, null),
	    Image = proplists:get_value(<<"image">>, Body, null),
	    Homepage = proplists:get_value(<<"homepage">>, Body, null),
	    model_users:set_details(UserName, Title, Image, Homepage),
	    json_ok({obj, []});
	true ->	    
	    throw({http, 403})
    end;

%% User feed
handle_request2(#req{method = 'GET',
		     path = [<<UserName/binary>>, <<Slug/binary>>]
		    } = Req) ->
    %% All hashed episodes
    html_ok(ui_template:render_user_feed(validate_session(Req), UserName, Slug));


%% Create user feed
handle_request2(#req{method = 'PUT',
		     path = [<<UserName/binary>>, <<Slug/binary>>],
		     body = Body
		    } = Req) ->
    #req{session_user = SessionUser} = validate_session(Req),
    URL = proplists:get_value(<<"url">>, Body, null),
    case {SessionUser == UserName,
	  validate_slug(Slug),
	  URL} of
	{true, true, <<"http://", _/binary>>} ->
	    {ok, BaseURL} = application:get_env(ui, base_url),
	    Link = <<BaseURL/binary, (ui_link:link_user_feed(UserName, Slug))/binary>>,
	    case model_users:add_feed(UserName, Slug, URL) of
		{ok, true = _IsNew} ->
		    %% TODO: test-fetch
		    json_ok({obj, [{link, Link}]});
		{ok, _} ->
		    json_ok({obj, [{link, Link}]});
		{error, Reason} ->
		    io:format("model_users:add_feed(~p, ~p, ~p) failed: ~p~n",
			      [UserName, Slug, URL, Reason]),
		    json_ok({obj, [{error, <<"Cannot add this feed">>}]})
	    end;
	{false, _, _} ->
	    throw({http, 403});
	{true, false, _} ->
	    json_ok({obj, [{error, <<"Malformed slug">>}]});
	{true, true, _} ->
	    json_ok({obj, [{error, <<"URI scheme not supported">>}]})
    end;

%% Delete user feed
handle_request2(#req{method = 'DELETE',
		     path = [<<UserName/binary>>, <<Slug/binary>>]
		    } = Req) ->
    #req{session_user = SessionUser} = validate_session(Req),
    if
	SessionUser == UserName ->
	    model_users:rm_feed(UserName, Slug),

	    %% Respond with a new target the client JS should navigate to:
	    Link = ui_link:link_user(UserName),
	    json_ok({obj, [{link, Link}]});
	true ->
	    throw({http, 403})
    end;

%% User feed export
%% TODO: support not modified
handle_request2(#req{method = 'GET',
		     path = [<<UserName/binary>>, <<Slug/binary>>, <<"feed">>]
		    } = Req) ->
    {ok, Type, Body} = ui_template:export_feed(Req, UserName, Slug),
    Headers =
	[{<<"Content-Type">>, case Type of
				  atom -> ?MIME_ATOM;
				  _ -> ?MIME_RSS
			      end}],
    {ok, 200, Headers, [], Body};

handle_request2(#req{method = 'GET',
		     path = [<<UserName/binary>>, <<Slug/binary>>, <<"downloads.rss">>]
		    } = Req) ->
    {ok, Body} = ui_template:export_downloads(rss, Req, UserName, Slug),
    {ok, 200,
     [{<<"Content-Type">>, ?MIME_RSS}], [],
     Body};

handle_request2(#req{method = 'GET',
		     path = [<<UserName/binary>>, <<Slug/binary>>, <<"downloads.atom">>]
		    } = Req) ->
    {ok, Body} = ui_template:export_downloads(atom, Req, UserName, Slug),
    {ok, 200,
     [{<<"Content-Type">>, ?MIME_ATOM}], [],
     Body};

%% User feed as json
handle_request2(#req{method = 'GET',
		     path = [<<UserName/binary>>, <<Slug/binary>>, <<"details.json">>]
		    } = Req) ->
    #req{session_user = SessionUser} = validate_session(Req),
    if
	SessionUser == UserName ->
	    {ok, _Feed, Public, Title} = model_users:get_user_feed(UserName, Slug),
	    io:format("Public: ~p~n", [Public]),
	    json_ok({obj, [{public, Public} |
			   if
			       is_binary(Title) ->
				   [{title, Title}];
			       true ->
				   []
			   end
			  ]});
	true ->	    
	    throw({http, 403})
    end;

handle_request2(#req{method = 'POST',
		     path = [<<UserName/binary>>, <<Slug/binary>>, <<"details.json">>],
		     body = Body
		    } = Req) ->
    #req{session_user = SessionUser} = validate_session(Req),
    if
	SessionUser == UserName ->
	    io:format("Body: ~p~n", [Body]),
	    Public = case proplists:get_value(<<"public">>, Body, <<"false">>) of
			 <<"true">> ->
			     true;
			 _ ->
			     false
		     end,
	    Title = proplists:get_value(<<"title">>, Body, null),
	    model_users:set_user_feed(UserName, Slug,
				      Public, Title),
	    json_ok({obj, []});
	true ->	    
	    throw({http, 403})
    end;

%% Torrent download by user/slug/name
handle_request2(#req{method = 'GET',
		     path = [<<UserName/binary>>, <<Slug/binary>>, <<FilePath/binary>>]
		    }) ->
    %% Parse torrent name
    case parse_torrent_name(FilePath) of
	{ok, Name} ->
	    case model_enclosures:get_torrent_by_name(UserName, Slug, Name) of
		{ok, Torrent} ->
		    {ok, 200,
		     [{<<"Content-Type">>, ?MIME_TORRENT}], [],
		     Torrent};
		{error, not_found} ->
		    throw({http, 404})
	    end;
	_ ->
	    %% FilePath does not end in ".torrent"
	    throw({http, 404})
    end;

%% Purge torrent
handle_request2(#req{method = 'DELETE',
		     path = [<<UserName/binary>>, <<Slug/binary>>, <<FilePath/binary>>]
		    }) ->
    %% Parse torrent name
    case parse_torrent_name(FilePath) of
	{ok, Name} ->
	    model_enclosures:purge(UserName, Slug, Name),
	    {ok, 200, [], [], <<>>};
	_ ->
	    %% FilePath does not end in ".torrent"
	    throw({http, 404})
    end;

%% Graphs
handle_request2(#req{method = 'GET',
		     path = [<<UserName/binary>>, <<Slug/binary>>, <<FilePath/binary>>, <<"g">>, TimeSpec, Graph]
		    }) ->
    InfoHash =
	case model_enclosures:get_info_hash_by_name(UserName, Slug, FilePath) of
	    {ok, InfoHash1} ->
		InfoHash1;
	    {error, not_found} ->
		throw({http, 404})
	end,
    {Period, Interval} =
	case TimeSpec of
	    <<"day">> ->
		{24 * 60 * 60, 60 * 60};
	    <<"week">> ->
		{7 * 24 * 60 * 60, 6 * 60 * 60};
	    <<"month">> ->
		{30 * 24 * 60 * 60, 24 * 60 * 60};
	    <<"year">> ->
		{365 * 24 * 60 * 60, 7 * 24 * 60 * 60 }
	end,
    Stop = calendar:local_time(),
    Start = calendar:gregorian_seconds_to_datetime(
	      calendar:datetime_to_gregorian_seconds(Stop) - Period),
    case Graph of
	<<"swarm.json">> ->
	    Seeders =
		model_graphs:get_gauge(seeders, InfoHash, Start, Stop, Interval),
	    Leechers =
		model_graphs:get_gauge(leechers, InfoHash, Start, Stop, Interval),
	    json_ok({obj, [{<<"seeders">>, {obj, convert_graphs_time(Seeders)}},
			   {<<"leechers">>, {obj, convert_graphs_time(Leechers)}},
			   {<<"start">>, iso8601(Start)},
			   {<<"stop">>, iso8601(Stop)},
			   {<<"interval">>, Interval}
			  ]});
	<<"traffic.json">> ->
	    Down =
		model_graphs:get_counter(down, InfoHash, Start, Stop, Interval),
	    Up =
		model_graphs:get_counter(up, InfoHash, Start, Stop, Interval),
	    UpSeeder =
		model_graphs:get_counter(up_seeder, InfoHash, Start, Stop, Interval),
	    json_ok({obj, [{<<"down">>, {obj, convert_graphs_time(Down)}},
			   {<<"up">>, {obj, convert_graphs_time(Up)}},
			   {<<"up_seeder">>, {obj, convert_graphs_time(UpSeeder)}},
			   {<<"start">>, iso8601(Start)},
			   {<<"stop">>, iso8601(Stop)},
			   {<<"interval">>, Interval}
			  ]});
	<<"downloads.json">> ->
	    Downloads =
		model_graphs:get_counter(complete, InfoHash, Start, Stop, Interval),
	    json_ok({obj, [{<<"downloads">>, {obj, convert_graphs_time(Downloads)}},
			   {<<"start">>, iso8601(Start)},
			   {<<"stop">>, iso8601(Stop)},
			   {<<"interval">>, Interval}
			  ]});
	_ ->
	    throw({http, 404})
    end;

%% 404
handle_request2(_Req) ->
    throw({http, 404}).


parse_torrent_name(Path) ->
    case (catch split_binary(Path, size(Path) - 8)) of
	{Name, <<".torrent">>} ->
	    {ok, Name};
	_ ->
	    false
    end.

%% UI stats gathering
count_request(Method, []) ->
    count_request(Method, <<"/">>);
count_request(Method, Path) when is_list(Path) ->
    count_request(Method,
		  list_to_binary([[$/, P] || P <- Path]));

count_request(Method, Path) ->
    model_stats:add_counter(
      list_to_binary(io_lib:format("~p/http", [node()])),
      list_to_binary(io_lib:format("~s ~s", [Method, Path])),
      1).

%% Complete session data where needed
validate_session(#req{sid = Sid} = Req) ->
    SessionUser =
	case model_session:validate(Sid) of
	    {ok, User} ->
		User;
	    {error, invalid_session} ->
		undefined
	end,
    Req#req{session_user = SessionUser}.

%%
%% Signup helpers
%%
validate_username(UserName) ->
    if
	size(UserName) >= 1 ->
	    validate_username1(UserName);
	true ->
	    false
    end.
validate_username1(<<>>) ->
    true;
validate_username1(<<C:8, Rest/binary>>)
  when (C >= $a andalso C =< $z);
       (C >= $0 andalso C =< $9);
       C == $-;
       C == $_ ->
    validate_username1(Rest);
validate_username1(_) ->
    false.

validate_email(Email) ->
    Ats = [C || <<C:8>> <= Email,
		C == $@],
    Spaces = [C || <<C:8>> <= Email,
		   C == $ ],
    if
	length(Ats) == 1,
	length(Spaces) == 0 ->
	    true;
	true ->
	    false
    end.


validate_slug(Slug) ->
    validate_username(Slug).


create_account(UserName, Email) ->
    %% Create Account
    model_users:register(UserName, Email),
    %% Prepare Activation
    {ok, Token} =
	model_token:generate(activate, UserName),
    TokenHex = util:binary_to_hex(Token),
    
    %% Send Email
    {ok, BaseURLSSL} = application:get_env(ui, base_url_ssl),
    {ok, SmtpOptions} = application:get_env(ui, smtp_options),
    MailFrom = <<"mail@bitlove.org">>,
    Mail =
	mimemail:encode(
	  {"text", "plain",
	   [{<<"From">>, MailFrom},
	    {<<"To">>, <<UserName/binary, " <", Email/binary, ">">>},
	    {<<"Subject">>, <<"Welcome to Bitlove">>},
	    {<<"User-Agent">>, <<"PritTorrent">>}],
	   [],
	   <<"Welcome to Bitlove!\r\n",
	     "\r\n",
	     "To complete signup visit the following link:\r\n",
	     "    ", BaseURLSSL/binary, "/activate/", TokenHex/binary, "\r\n",
	     "\r\n",
	     "\r\n",
	     "Thanks for sharing\r\n",
	     "    The Bitlove Team\r\n">>
	  }),
    case gen_smtp_client:send_blocking({MailFrom, [Email], Mail},
				       SmtpOptions) of
	Response when is_binary(Response) ->
	    %% Respond
	    ok;
	{error, Reason} ->
	    io:format("gen_smtp_client:send_blocking failed with:~n~p ~p~n~p~n",
		      [Mail, SmtpOptions, Reason]),
	    {error, <<"Cannot send mail">>}
    end.

reactivate_user(UserName, Email) ->
    %% Prepare Activation
    {ok, Token} =
	model_token:generate(activate, UserName),
    TokenHex = util:binary_to_hex(Token),
    
    %% Send Email
    {ok, BaseURLSSL} = application:get_env(ui, base_url_ssl),
    {ok, SmtpOptions} = application:get_env(ui, smtp_options),
    MailFrom = <<"mail@bitlove.org">>,
    Mail =
	mimemail:encode(
	  {"text", "plain",
	   [{<<"From">>, MailFrom},
	    {<<"To">>, <<UserName/binary, " <", Email/binary, ">">>},
	    {<<"Subject">>, <<"Password reset for your Bitlove account">>},
	    {<<"User-Agent">>, <<"PritTorrent">>}],
	   [],
	   <<"Dear user\r\n",
	     "\r\n",
	     "To set a new password for your account visit this link:\r\n",
	     "    ", BaseURLSSL/binary, "/activate/", TokenHex/binary, "\r\n",
	     "\r\n",
	     "\r\n",
	     "Enjoy the ride\r\n",
	     "    The Bitlove Team\r\n">>
	  }),
    case gen_smtp_client:send_blocking({MailFrom, [Email], Mail},
				       SmtpOptions) of
	Response when is_binary(Response) ->
	    %% Respond
	    ok;
	{error, Reason} ->
	    io:format("gen_smtp_client:send_blocking failed with:~n~p ~p~n~p~n",
		      [Mail, SmtpOptions, Reason]),
	    {error, <<"Cannot send mail">>}
    end.

%%
%% Helpers
%%

escape_bin(<<>>, _) ->
    <<>>;
escape_bin(<<C:8, Bin/binary>>, C) ->
    <<"\\", C:8, (escape_bin(Bin, C))/binary>>;
escape_bin(<<C:1/binary, Bin/binary>>, E) ->
    <<C/binary, (escape_bin(Bin, E))/binary>>.

hmac(Key, Text) ->
    io:format("hmac ~p ~p~n", [Key,Text]),
    Ctx1 = crypto:hmac_init(sha, Key),
    Ctx2 = crypto:hmac_update(Ctx1, Text),
    crypto:hmac_final(Ctx2).

iso8601({{Y, Mo, D}, {H, M, S}}) ->
    list_to_binary(
      io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B",
		    [Y, Mo, D, H, M, trunc(S)])).
    
convert_graphs_time(Data) ->
    lists:map(
      fun({Date, Value}) ->
	      {iso8601(Date), Value}
      end, Data).

