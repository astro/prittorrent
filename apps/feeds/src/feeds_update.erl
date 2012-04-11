-module(feeds_update).

-export([update/1]).



update(URL) ->
    {ok, Etag1, LastModified1} = model_feeds:prepare_update(URL),
    R1 =
	try feeds_fetch:fetch(URL, Etag1, LastModified1) of
	    {ok, {Etag, LastModified}, RootEl} ->
		try feeds_parse:pick_items(RootEl) of
		    {ok, FeedEl, Items1} ->
			FeedXml1 = exmpp_xml:document_to_binary(FeedEl),
			{ok, {Etag, LastModified},
			 FeedXml1, Items1}
		catch exit:Reason1 ->
			{error, {Etag, LastModified}, Reason1}
		end;
	    not_modified ->
		%% Well, show it to the user... (in green :)
		{error, {Etag1, LastModified1}, not_modified}
	catch exit:Reason1 ->
		{error, {Etag1, LastModified1}, Reason1}
	end,

    case R1 of
	{ok, {Etag2, LastModified2}, FeedXml, Items} ->
	    model_feeds:write_update(URL, {Etag2, LastModified2}, null, FeedXml),
	    lists:foreach(
	      fun(_Item) ->
		      %%model_feeds:update_item(URL
		      todo
	      end, Items),
	    ok;
	{error, {Etag2, LastModified2}, Reason} ->
	    Error = case Reason of
		undefined -> null;
		_ -> list_to_binary(io_lib:format("~p",[Reason]))
	    end,
	    model_feeds:write_update(URL, {Etag2, LastModified2}, Error, null),
	    {error, Reason}
    end.
