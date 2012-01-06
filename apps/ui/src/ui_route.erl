-module(ui_route).

-export([handle_http/1, handle_websocket/1]).

-define(HTML_HEADERS, [{"Content-type", "text/html"}]).

handle_http(Req) ->
    {ok, PeerAddr} = Req:get(peer_addr),
    Method = Req:get(method),
    Path = Req:get(uri_unquoted),
    {VsnMaj, VsnMin} = Req:get(vsn),
    io:format("~s ~s ~s HTTP/~B.~B~n", [inet_parse:ntoa(PeerAddr), Method, Path, VsnMaj, VsnMin]),
    route_http(Req, Method, Path).

route_http(Req, 'GET', "/static/" ++ Path) ->
    case lists:member($/, Path) of
	true ->
	    %% Slashes are forbidden to defend against directory
	    %% traversal attacks.
	    Req:respond(400, ?HTML_HEADERS, "What?\n");
	false ->
	    Req:file(priv_path("static/" ++ Path))
    end;

route_http(Req, 'GET', "/signup") ->
    Req:file(priv_path("signup.html"));

route_http(Req, 'POST', "/signup") ->
    Query = Req:parse_post(unicode),
    User = Req:get_variable("user", Query),
    Email = Req:get_variable("email", Query),
    Password = Req:get_variable("password", Query),
    Password2 = Req:get_variable("password2", Query),
    if
	not is_list(User) orelse
	length(User) < 1 ->
	    Req:respond(400, ?HTML_HEADERS, "Invalid user name");
	not is_list(Email) orelse
	length(Email) < 1 ->
	    Req:respond(400, ?HTML_HEADERS, "Invalid email address");
	not is_list(Password) orelse
	length(Password) < 1 ->
	    Req:respond(400, ?HTML_HEADERS, "Invalid password");
	Password =/= Password2 ->
	    Req:respond(400, ?HTML_HEADERS, "Passwords don't match");
	true ->
	    model_users:register(User, Email, Password),
	    Req:respond(200, ?HTML_HEADERS, "Thanks, we'll be in contact.")
    end;

route_http(Req, 'GET', "/u") ->
    Req:file(priv_path("u.html"));

route_http(Req, _, _Path) ->
    Req:respond(404, ?HTML_HEADERS, "Not found\n").

handle_websocket(WS) ->
    {ok, PeerAddr} = WS:get(peer_addr),
    Path = WS:get(path),
    Vsn = WS:get(vsn),
    io:format("~s ~s WS/~s~n", [inet_parse:ntoa(PeerAddr), Path, Vsn]),
    route_websocket(WS, Path).

route_websocket(WS, "/u") ->
    ui_u:run(WS).


priv_path(Tail) ->
    filename:join([filename:dirname(?FILE),
		   "..",
		   "priv",
		   Tail]).
