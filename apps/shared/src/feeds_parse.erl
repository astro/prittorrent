-module(feeds_parse).

-export([get_type/1, get_channel/1,
	 title/1, link/1, image/1,
	 pick_items/1, merge_items/2,
	 item_id/1, item_title/1, item_enclosures/1,
	 item_published/1, item_link/1, item_payment/1, item_image/1,
	 replace_item_enclosures/2]).

-include_lib("exmpp/include/exmpp_xml.hrl").

-define(NS_ATOM, "http://www.w3.org/2005/Atom").


get_type(Xml) ->
    case exmpp_xml:get_ns_as_list(Xml) of
	?NS_ATOM ->
	    atom;
	_ ->
	    rss
    end.

get_channel(Xml) ->
    case get_type(Xml) of
	atom ->
	    Xml;
	_ ->
	    exmpp_xml:get_element(Xml, "channel")
    end.

%% Just look for 1st title element
-spec(title/1 :: (xmlel()) -> binary() | undefined).
title(Xml) ->
    case exmpp_xml:get_name_as_list(Xml) of
	"title" ->
	    exmpp_xml:get_cdata(Xml);
	_ ->
	    lists:foldl(fun(Child, undefined) ->
				title(Child);
			   (_, Title) ->
				Title
			end, undefined, exmpp_xml:get_child_elements(Xml))
    end.


-spec(image/1 :: (xmlel()) -> binary() | undefined).
image(Xml) ->
    image1(Xml, ["image", "logo", "icon"]).

image1(_, []) ->
    undefined;
image1(Xml, [ChildName | ChildNames]) ->
    R =
	lists:foldl(
	  fun(Child, undefined) ->
		  URL1 =
		      case exmpp_xml:get_elements(Child, "url") of
			  [UrlEl | _] ->
			      exmpp_xml:get_cdata(UrlEl);
			  _ ->
			      undefined
		      end,
		  if
		      is_binary(URL1),
		      size(URL1) > 0 ->
			  URL1;
		      true ->
			  case exmpp_xml:get_cdata(Child) of
			      Cdata
				when is_binary(Cdata),
				     size(Cdata) > 0 ->
				  Cdata;
			      _ ->
				  undefined
			  end
		  end;
	     (_, R) ->
		  R
	  end, undefined, exmpp_xml:get_elements(Xml, ChildName)),
    case R of
	undefined ->
	    image1(Xml, ChildNames);
	_ ->
	    R
    end.



%% Separates items from the feed metadata
-spec(pick_items/1 :: (xmlel()) -> {ok, xmlel(), [xmlel()]}).
pick_items(#xmlel{} = RootEl) ->
    case exmpp_xml:get_name_as_list(RootEl) of
	%% Handle ATOM
	"feed" ->
	    {Entries, FeedChildren} =
		lists:partition(
		  fun(#xmlel{name="entry"}) ->
			  true;
		     (_) ->
			  false
		  end, RootEl#xmlel.children),
	    {ok,
	     RootEl#xmlel{children = lists:reverse(FeedChildren)},
	     lists:reverse(Entries)};

	%% Assume RSS
	_ ->
	    {Items, RootChildren} =
		lists:foldl(
		  fun(#xmlel{name="channel"}=Channel,
		      {Items, RootChildren}) ->
			  {Items1, ChannelChildren} =
			      lists:partition(
				fun(#xmlel{name="item"}) ->
					true;
				   (_) ->
					false
				end, Channel#xmlel.children),
			  {Items1 ++ Items,
			   [Channel#xmlel{children = ChannelChildren}
			    | RootChildren]};
		     (Child,
		      {Items, RootChildren}) ->
			  {Items,
			   [Child
			    | RootChildren]}
		  end, {[], []}, RootEl#xmlel.children),
	    {ok,
	     RootEl#xmlel{children = lists:reverse(RootChildren)},
	     lists:reverse(Items)}
    end.

%% Opposite of pick_items/1
merge_items(FeedEl, ItemEls) ->
    ChannelEl1 = get_channel(FeedEl),
    ChannelEl2 = exmpp_xml:append_children(ChannelEl1, ItemEls),
    if
	%% ATOM
	ChannelEl1 =:= FeedEl ->
	    ChannelEl2;
	%% RSS
	true ->
	    exmpp_xml:set_children(
	      FeedEl,
	      lists:map(fun(Child) when Child =:= ChannelEl1 ->
				ChannelEl2;
			   (Child) ->
				Child
			end,
			exmpp_xml:get_child_elements(FeedEl)))
    end.


item_title(ItemEl) ->
    %% Works the same as for the feed in RSS & ATOM
    title(ItemEl).

item_id(ItemEl) ->
    item_id1(ItemEl, ["id", "guid", "link"]).

item_id1(_ItemEl, []) ->
    undefined;
item_id1(ItemEl, [ChildName | ChildNames]) ->
    R =
	lists:foldl(
	  fun(Child, undefined) ->
		  case exmpp_xml:get_cdata(Child) of
		      Cdata
			when is_binary(Cdata),
			     size(Cdata) > 0 ->
			  Cdata;
		      _ ->
			  case {exmpp_xml:get_attribute_as_binary(
				  Child, <<"rel">>, undefined),
				exmpp_xml:get_attribute_as_binary(
				  Child, <<"href">>, undefined)} of
			      {<<"alternate">>, Href} ->
				  Href;
			      _ ->
				  undefined
			  end
		  end;
	     (_, R) ->
		  R
	  end, undefined, exmpp_xml:get_elements(ItemEl, ChildName)),
    case R of
	undefined ->
	    item_id1(ItemEl, ChildNames);
	_ ->
	    R
    end.

link(Xml) ->
    %% Handle ATOM
    case exmpp_xml:get_ns_as_list(Xml) of
	?NS_ATOM ->
	    lists:foldl(
	      fun(LinkEl, undefined) ->
		      case {exmpp_xml:get_attribute_as_binary(
			      LinkEl, <<"rel">>, undefined),
			    exmpp_xml:get_attribute_as_binary(
			      LinkEl, <<"href">>, undefined)} of
			  {<<"alternate">>, Href} ->
			      Href;
			  _ ->
			      undefined
		      end;
		 (_, Href) ->
		      Href
	      end, undefined, exmpp_xml:get_elements(Xml, "link"));
	_ ->
	    link1(Xml, ["link", "url"])
    end.

link1(_Xml, []) ->
    undefined;
link1(Xml, [ChildName | ChildNames]) ->
    R =
	lists:foldl(
	  fun(Child, undefined) ->
		  case exmpp_xml:get_cdata(Child) of
		      Cdata
			when is_binary(Cdata),
			     size(Cdata) > 0 ->
			  Cdata;
		      _ ->
			  undefined
		  end;
	     (_, R) ->
		  R
	  end, undefined, exmpp_xml:get_elements(Xml, ChildName)),
    case R of
	undefined ->
	    link1(Xml, ChildNames);
	_ ->
	    R
    end.

item_published(ItemEl) ->
    item_published1(ItemEl, ["pubDate", "published", "updated"]).

item_published1(_ItemEl, []) ->
    undefined;
item_published1(ItemEl, [ChildName | ChildNames]) ->
    R =
	lists:foldl(
	  fun(Child, undefined) ->
		  case exmpp_xml:get_cdata(Child) of
		      Cdata
			when is_binary(Cdata),
			     size(Cdata) > 0 ->
			  Cdata;
		      _ ->
			  undefined
		  end;
	     (_, R) ->
		  R
	  end, undefined, exmpp_xml:get_elements(ItemEl, ChildName)),
    case R of
	undefined ->
	    item_published1(ItemEl, ChildNames);
	_ ->
	    R
    end.

item_link(ItemEl) ->
    ?MODULE:link(ItemEl).

item_payment(ItemEl) ->
    lists:foldl(
      fun(LinkEl, undefined) ->
	      case {exmpp_xml:get_attribute_as_binary(
		      LinkEl, <<"rel">>, undefined),
		    exmpp_xml:get_attribute_as_binary(
		      LinkEl, <<"href">>, undefined)} of
		  {<<"payment">>, Href} ->
		      Href;
		  _ ->
		      undefined
	      end;
	 (_, Href) ->
	      Href
      end, undefined, exmpp_xml:get_elements(ItemEl, "link")).


item_image(ItemEl) ->
    image(ItemEl).

-spec(item_enclosures/1 :: (xmlel()) -> [binary()]).
item_enclosures(ItemEl) ->
    lists:reverse(
	lists:foldl(fun(#xmlel{name="enclosure"}=El, URLs) ->
			    case exmpp_xml:get_attribute_as_binary(El, <<"url">>, undefined) of
				URL when is_binary(URL) ->
				    [URL | URLs];
				_ ->
				    case exmpp_xml:get_cdata(El) of
					URL when is_binary(URL), size(URL) > 6 ->
					    [URL | URLs];
					_ ->
					    URLs
				    end
			    end;
		       (#xmlel{name="link"}=El, URLs) ->
			    case exmpp_xml:get_attribute_as_binary(El, <<"rel">>, undefined) of
				<<"enclosure">> ->
				    case exmpp_xml:get_attribute_as_binary(El, <<"href">>, undefined) of
					URL when is_binary(URL), size(URL) > 6 ->
					    [URL | URLs];
					_ ->
					    URLs
				    end;
				_Rel ->
				    URLs
			    end;
		       (_, URLs) ->
			    URLs
		    end, [], exmpp_xml:get_child_elements(ItemEl))).

replace_item_enclosures(ItemEl, MapFun) ->
    exmpp_xml:set_children(
      ItemEl,
      lists:map(
	fun(#xmlel{name="link"} = LinkEl) ->
		case {exmpp_xml:get_attribute_as_binary(
			LinkEl, <<"rel">>, undefined),
		      exmpp_xml:get_attribute_as_binary(
			LinkEl, <<"href">>, undefined)} of
		    {<<"enclosure">>, Href} ->
			NewHref = MapFun(Href),
			LinkEl2 =
			    exmpp_xml:set_attribute(
			      LinkEl , <<"href">>, NewHref),
			LinkEl3 =
			    exmpp_xml:set_attribute(
			      LinkEl2 , <<"type">>, <<"application/x-bittorrent">>),
			LinkEl3;
		    {_, _} ->
			LinkEl
		end;
	   (#xmlel{name="enclosure"} = EnclosureEl) ->
		URL = exmpp_xml:get_attribute_as_binary(
			EnclosureEl, <<"url">>, undefined),
		NewURL = MapFun(URL),
		EnclosureEl2 =
		    exmpp_xml:set_attribute(
		      EnclosureEl, <<"url">>, NewURL),
		EnclosureEl3 =
		    exmpp_xml:set_attribute(
		      EnclosureEl2, <<"type">>, <<"application/x-bittorrent">>),
		EnclosureEl3;
	   (Child) ->
		Child
	end,
	exmpp_xml:get_child_elements(ItemEl))).

