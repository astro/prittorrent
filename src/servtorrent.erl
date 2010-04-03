-module(servtorrent).

-export([start/0, start/2]).


start() ->
    start(o, o).

start(_, _) ->
    torrentdb:init(),
    piecesdb:init(),
    servtorrent_sup:start_link(6881).
