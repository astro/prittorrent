-module(ui_link).

-export([base/0,
	 link_user/1,
	 link_user_feed/2, link_user_feed_xml/2,
	 link_downloads_feed/2, link_downloads_feed/3,
	 link_item/3,
	 torrent/1, torrent/3]).


base() ->
    case application:get_env(ui, base_url) of
	undefined ->
	    <<"http://bitlove.org">>;
	{ok, Base} ->
	    Base
    end.


link_user(UserName) ->
    <<"/", UserName/binary>>.

link_user_feed(UserName, Slug) ->
    <<"/", UserName/binary,
      "/", Slug/binary>>.

link_user_feed_xml(UserName, Slug) ->
    <<"/", UserName/binary,
      "/", Slug/binary,
      "/feed">>.


link_downloads_feed(new, Type) ->
    <<"/new.",
      (list_to_binary(atom_to_list(Type)))/binary>>;
link_downloads_feed({top, peers}, Type) ->
    <<"/top.",
      (list_to_binary(atom_to_list(Type)))/binary>>;
link_downloads_feed({top, Period}, Type) ->
    list_to_binary(
      io_lib:format("/top.~s/~p", [Type, Period]));
link_downloads_feed(UserName, Type) ->
    <<"/", UserName/binary,
      "/downloads.",
      (list_to_binary(atom_to_list(Type)))/binary>>.

link_downloads_feed(UserName, Slug, Type) ->
    <<"/", UserName/binary,
      "/", Slug/binary,
      "/downloads.",
      (list_to_binary(atom_to_list(Type)))/binary>>.

%% FIXME: Use cowboy_http:urlencode(Id) when the fragments start
%% causing problems.
link_item(UserName, Slug, Id) ->
    <<"/", UserName/binary,
      "/", Slug/binary,
      "#", Id/binary>>.
    

torrent(InfoHash) ->
    iolist_to_binary(
      [<<"/t/">>,
       util:binary_to_hex(InfoHash),
       <<".torrent">>]).

torrent(UserName, Slug, Name) ->
    <<"/", UserName/binary,
      "/", Slug/binary,
      "/", Name/binary, ".torrent">>.
