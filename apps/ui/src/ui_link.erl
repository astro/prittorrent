-module(ui_link).

-export([base/0, link_user/1, link_user_feed/2, torrent/1]).


base() ->
    case application:get_env(ui, base_url) of
	undefined ->
	    <<"">>;
	{ok, Base} ->
	    Base
    end.


link_user(UserName) ->
    <<"/~", UserName/binary>>.

link_user_feed(UserName, Slug) ->
    <<"/~", UserName/binary, "/", Slug/binary>>.


torrent(InfoHash) ->
    iolist_to_binary(
      [<<"/t/">>,
       binary_to_hex(InfoHash),
       <<".torrent">>]).

binary_to_hex(<<>>) ->
    [];
binary_to_hex(<<C:8, Bin/binary>>) ->
    [io_lib:format("~2.16.0b", [C]) | binary_to_hex(Bin)].
