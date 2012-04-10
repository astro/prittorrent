-module(feeds_parse).

-export([title/1, image/1,
	 pick_items/1,
	 item_title/1, item_enclosures/1,
	 replace_item_enclosures/2]).

-include("deps/exmpp/include/exmpp.hrl").

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
	    IsEntryEl = fun(#xmlel{name="entry"}) ->
				true;
			   (_) ->
				false
			end;
	%% Assume RSS
	%% FIXME: rss/channel/item
	_ ->
	    IsEntryEl = fun(#xmlel{name="item"}) ->
				true;
			   (_) ->
				false
			end
    end,

    {Entries, FeedChildren} =
	lists:partition(IsEntryEl, RootEl#xmlel.children),
    {ok,
     RootEl#xmlel{children = lists:reverse(FeedChildren)},
     lists:reverse(Entries)}.


item_title(ItemEl) ->
    %% Works the same as for the feed in RSS & ATOM
    title(ItemEl).

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
				T ->
				    URLs
			    end;
		       (_, URLs) ->
			    URLs
		    end, [], exmpp_xml:get_child_elements(ItemEl))).

replace_item_enclosures(_ItemEl, _MapFun) ->
    undefined.
