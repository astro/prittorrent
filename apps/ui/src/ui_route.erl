-module(ui_route).

-export([handle_http/1, handle_websocket/1]).

-define(HTML_HEADERS, [{"Content-type", "text/html"}]).

handle_http(Req) ->
    {ok, PeerAddr} = Req:get(peer_addr),
    Method = Req:get(method),
    {abs_path, Path} = Req:get(uri_unquoted),
    {VsnMaj, VsnMin} = Req:get(vsn),
    io:format("~s ~s ~s HTTP/~B.~B~n", [inet_parse:ntoa(PeerAddr), Method, Path, VsnMaj, VsnMin]),
    route_http(Req, Path).

route_http(Req, "/static/" ++ Path) ->
    case lists:member($/, Path) of
	true ->
	    %% Slashes are forbidden to defend against directory
	    %% traversal attacks.
	    Req:respond(400, ?HTML_HEADERS, "What?\n");
	false ->
	    Req:file(Path)
    end;

route_http(Req, _Path) ->
    Req:respond(404, ?HTML_HEADERS, "Not found\n").

handle_websocket(WS) ->
    undefined.
