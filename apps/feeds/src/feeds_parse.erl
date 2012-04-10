-module(feeds_parse).

-export([title/1, pick_items/1, item_enclosures/1]).

-include("deps/exmpp/include/exmpp.hrl").

%% Just look for 1st title element
-spec(title/1 :: (xmlel()) -> binary() | undefined).
title(Xml) ->
    case exmpp_xml:get_name_as_atom(Xml) of
	title ->
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
    undefined.


%% Separates items from the feed metadata
-spec(pick_items/1 :: (xmlel()) -> {ok, xmlel(), [xmlel()]}).
pick_items(RootEl) ->
    undefined.


-spec(item_enclosures/1 :: (xmlel()) -> [binary()]).
item_enclosures(ItemEl) ->
    URLs =
	lists:foldl(fun(El, URLs) ->
			    case exmpp_xml:get_name_as_atom(El) of
				enclosure ->
				    case exmpp_xml:get_attribute_as_binary(El, <<"url">>) of
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
				link ->
				    case exmpp_xml:get_attribute_as_binary(El, <<"type">>) of
					<<"enclosure">> ->
					    case exmpp_xml:get_attribute_as_binary(El, <<"href">>) of
						URL when is_binary(URL), size(URL) > 6 ->
						    [URL | URLs];
						_ ->
						    URLs
					    end;
					T ->
					    io:format("Ignore link type: ~p~n", [T])
				    end
			    end
		    end, [], exmpp_xml:get_child_elements(ItemEl)),
    lists:reverse(URLs).
