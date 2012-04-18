-module(ui_link).

-export([torrent/1]).

torrent(InfoHash) ->
    iolist_to_binary(
      [<<"/t/">>,
       binary_to_hex(InfoHash),
       <<".torrent">>]).

binary_to_hex(<<>>) ->
    [];
binary_to_hex(<<C:8, Bin/binary>>) ->
    [io_lib:format("~2.16.0b", [C]) | binary_to_hex(Bin)].
