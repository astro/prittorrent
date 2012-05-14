-module(ui_handler).
-export([init/3, handle/2, terminate/2]).

-behaviour(cowboy_http_handler).

-include("../include/ui.hrl").

-define(COOKIE_OPTS, [%{secure, true},
		      {http_only, true},
		      {max_age, 30 * 24 * 60 * 60}
		     ]).


init({tcp, http}, Req, _Opts) ->
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
	    {ok, Req2} = cowboy_http_req:reply(Status, [], <<"Oops">>, Req),
	    {ok, Req2, State};
	E ->
	    io:format("Error handling ~s ~p:~n~p~n", [Method, RawPath, E]),
	    {ok, Req2} = cowboy_http_req:reply(500, [], <<"Oops">>, Req),
	    {ok, Req2, State}
    end.		    

terminate(_Req, _State) ->
    ok.

html_ok(Body) ->
    {ok, 200, [{<<"Content-Type">>, <<"text/html; charset=UTF-8">>}], [], Body}.

json_ok(JSON) ->
    json_ok(JSON, []).
json_ok(JSON, Cookies) ->
    Body = rfc4627:encode(JSON),
    {ok, 200, [{<<"Content-Type">>, <<"application/json">>}], Cookies, Body}.

%% Attention: last point where Req is a cowboy_http_req, not a #req{}
handle_request1(Req) ->
    {Method, _} = cowboy_http_req:method(Req),
    {Path, _} = cowboy_http_req:path(Req),
    {Encodings, _} = cowboy_http_req:parse_header('Accept-Encoding', Req),
    {Languages, _} = cowboy_http_req:parse_header('Accept-Language', Req),
    {HexSid, _} = cowboy_http_req:cookie(<<"sid">>, Req),
    Sid = case (catch util:hex_to_binary(HexSid)) of
	      {'EXIT', _Reason} ->
		  undefined;
	      <<Sid1/binary>> ->
		  Sid1
	  end,
    io:format("Encodings: ~p~nLanguages: ~p~nSid: ~p~n", [Encodings,Languages,HexSid]),
    Body = if
	       Method =:= 'POST';
	       Method =:= 'POST' ->
		   {Body1, _} = cowboy_http_req:body_qs(Req),
		   io:format("BodyQS: ~p~n", [Body1]),
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
    {ok, 301, [{<<"Location">>, <<"/static/favicon.png">>}], <<>>};

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
	    case model_token:generate(UserName) of
		{ok, Salt, Token} ->
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
	    case model_token:validate(Token) of
		{ok, UserName1, Salted, _Salt} ->
		    %% check challenge response
		    ExpectedResponse = hmac(Token, Salted),
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
			    io:format("Wrong password for ~p~nChallengeResponse: ~p~nExpectedResponse: ~p~n", [UserName1, ChallengeResponse, ExpectedResponse]),
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

%% Torrent download
%% TODO: 'HEAD' too
handle_request2(#req{method = 'GET',
		     path = [<<"t">>, <<InfoHashHex:40/binary, ".torrent">>]
		    }) ->
    InfoHash = util:hex_to_binary(InfoHashHex),
    case model_torrents:get_torrent(InfoHash) of
	{ok, Name, Torrent} ->
	    NameE = escape_bin(Name, $\"),
	    Headers =
		[{<<"Content-Type">>, <<"application/x-bittorrent">>},
		 {<<"Content-Disposition">>,
		  <<"attachment; filename=\"", NameE/binary, ".torrent\"">>}],
	    {ok, 200, Headers, Torrent};
	{error, not_found} ->
	    throw({http, 404})
    end;

%% Index page
handle_request2(#req{method = 'GET',
		     path = []
		    } = Req) ->
    html_ok(ui_template:render_index(Req));

%% User profile
handle_request2(#req{method = 'GET',
		     path =[<<UserName/binary>>]
		    } = Req) ->
    html_ok(ui_template:render_user(Req, UserName));

%% User feed
handle_request2(#req{method = 'GET',
		     path = [<<UserName/binary>>, <<Slug/binary>>]
		    } = Req) ->
    %% All hashed episodes
    html_ok(ui_template:render_user_feed(Req, UserName, Slug));

%% User feed export
%% TODO: support not modified
handle_request2(#req{method = 'GET',
		     path = [<<UserName/binary>>, <<Slug/binary>>, <<"feed">>]
		    } = Req) ->
    {ok, Type, Body} = ui_template:export_feed(Req, UserName, Slug),
    Headers =
	[{<<"Content-Type">>, case Type of
				  atom -> <<"application/atom+xml">>;
				  _ -> <<"application/rss+xml">>
			      end}],
    {ok, 200, Headers, Body};

%% 404
handle_request2(_Req) ->
    throw({http, 404}).


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


create_account(UserName, Email) ->
    %% Create Account
    %% Prepare Activation
    %% Send Email
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
	     "    http://bitlove.org/activate/...\r\n",
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
    io:format("ctx1 ~p~n", [Ctx1]),
    Ctx2 = crypto:hmac_update(Ctx1, Text),
    io:format("ctx2 ~p~n", [Ctx2]),
    crypto:hmac_final(Ctx2).
