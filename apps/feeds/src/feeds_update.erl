-module(feeds_update).

-export([start_link/0, update_loop/0, update/1]).

-include_lib("model/include/model.hrl").

-define(INTERVAL, 600).

start_link() ->
    {ok, spawn_link(fun update_loop/0)}.

update_loop() ->
    case model_feeds:to_update(?INTERVAL) of
	{ok, {URL, Delay}}
	  when Delay =< 0 ->
	    io:format("Update ~s (scheduled in ~.2fs)~n", [URL, Delay]),
	    case (catch update(URL)) of
		{'EXIT', Reason} ->
		    io:format("Error updating ~s:~n~p~n", [URL, Reason]);
		_ ->
		    ok
	    end;
	{ok, {_, Delay}} ->
	    io:format("Nothing to update, sleeping ~.2f...~n", [Delay]),
	    receive
	    after trunc(Delay * 1000) ->
		    ok
	    end
    end,
    
    ?MODULE:update_loop().


update(URL) when is_binary(URL) ->
    update(binary_to_list(URL));
update(URL) ->
    {ok, Etag1, LastModified1} = model_feeds:prepare_update(URL),
    spawn(fun() ->
		  update1(URL, Etag1, LastModified1)
	  end),
    ok.

update1(URL, Etag1, LastModified1) ->
    R1 =
	try feeds_fetch:fetch(URL, Etag1, LastModified1) of
	    {ok, {Etag, LastModified}, RootEl} ->
		try
		    {ok, FeedEl, Items1} =
			feeds_parse:pick_items(RootEl),
		    io:format("Picked ~b items from feed ~s~n",
			      [length(Items1), URL]),
		    FeedXml1 = exmpp_xml:document_to_binary(FeedEl),
		    Items2 =
			lists:foldl(
			  fun(ItemXml, Items2) ->
				  try xml_to_feed_item(URL, ItemXml) of
				      #feed_item{} = Item ->
					  [Item | Items2];
				      _ ->
					  %%io:format("Malformed item: ~s~n", [exmpp_xml:document_to_binary(ItemXml)]),
					  Items2
				  catch exit:Reason ->
					  io:format("Cannot extract from feed item: ~s~n~p~n~s~n", [URL, Reason, ItemXml]),
					  Items2
				  end
			  end, [], Items1),
		    if
			length(Items2) < length(Items1) ->
			    io:format("Lost ~B of ~B items of ~s~n",
				      [length(Items1) - length(Items2), length(Items1), URL]);
			true ->
			    ok
		    end,
		    {ok, {Etag, LastModified},
		     FeedXml1, lists:reverse(Items2)}
		catch exit:Reason1 ->
			{error, {Etag, LastModified}, Reason1}
		end;
	    not_modified ->
		%% Well, show it to the user... (in green :)
		{error, {Etag1, LastModified1}, not_modified};
	    {error, Reason1} ->
		{error, {Etag1, LastModified1}, Reason1}
	catch exit:Reason1 ->
		{error, {Etag1, LastModified1}, Reason1}
	end,

    case R1 of
	{ok, {Etag2, LastModified2}, FeedXml, Items3} ->
	    io:format("model_feeds:write_update(~p, ~p, ~p, ~p, ~p)~n", [URL, {Etag2, LastModified2}, null, size(FeedXml), length(Items3)]),
	    model_feeds:write_update(URL, {Etag2, LastModified2}, null, FeedXml, Items3),
	    ok;
	{error, {Etag2, LastModified2}, Reason} ->
	    Error = case Reason of
		undefined -> null;
		_ -> list_to_binary(io_lib:format("~p",[Reason]))
	    end,
	    model_feeds:write_update(URL, {Etag2, LastModified2}, Error, null, []),
	    {error, Reason}
    end.

xml_to_feed_item(Feed, Xml) ->
    Id = feeds_parse:item_id(Xml),
    Title = feeds_parse:item_title(Xml),
    Published = feeds_parse:item_published(Xml),
    Homepage = feeds_parse:item_link(Xml),
    Payment = feeds_parse:item_payment(Xml),
    XmlSerialized = exmpp_xml:document_to_binary(Xml),
    Enclosures = feeds_parse:item_enclosures(Xml),
    if
	is_binary(Id),
	is_binary(Title),
	is_binary(Published),
	is_binary(XmlSerialized) ->
	    #feed_item{feed = Feed,
		       id = Id,
		       title = Title,
		       published = Published,
		       homepage = Homepage,
		       payment = Payment,
		       xml = XmlSerialized,
		       enclosures = Enclosures};
	true ->
	    %% Drop this
	    null
    end.
