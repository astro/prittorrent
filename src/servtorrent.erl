-module(servtorrent).

-export([start/0, start/2]).


start() ->
    lists:foreach(fun application:start/1,
		  [sasl, crypto, mnesia, inets, ibrowse, servtorrent]).

start(_, _) ->
    torrentdb:init(),
    piecesdb:init(),
    servtorrent_sup:start_link().
