-module(feeds_parse).

-export([title/1, image/1,
	 pick_items/1,
	 item_id/1, item_title/1, item_enclosures/1,
	 item_published/1,
	 replace_item_enclosures/2]).

-include_lib("exmpp/include/exmpp_xml.hrl").

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
image(_Xml) ->
    undefined.


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
			  case {exmpp:get_attribute_as_binary(
				  Child, "rel", undefined),
				exmpp:get_attribute_as_binary(
				  Child, "href", undefined)} of
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

replace_item_enclosures(_ItemEl, _MapFun) ->
    undefined.
