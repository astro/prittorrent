-module(servtorrent).

-export([start/0, start/2]).


start() ->
    lists:foreach(fun(App) ->
			  ok = application:start(App)
		  end,
		  [sasl, crypto, mnesia, inets, servtorrent]).

start(_, _) ->
    torrentdb:init(),
    piecesdb:init(),
    peerdb:init(),
    servtorrent_sup:start_link().
