-module(feeds_update).

-export([update_loop/0, update/1]).


-define(INTERVAL, 600).

update_loop() ->
    MinUpdate = calendar:datetime_to_gregorian_seconds(erlang:localtime()) - ?INTERVAL,
    case model_feeds:to_update(1) of
	{ok, [{URL, LastUpdate}]} ->
	    io:format("UL: ~p ~p~n",[URL, LastUpdate]),
	    LastUpdate1 = calendar:datetime_to_gregorian_seconds(LastUpdate),
	    if
		LastUpdate1 =< MinUpdate ->
		    io:format("to_update: ~p ~p~n", [URL, LastUpdate]),
		    try update(URL)
		    catch exit:Reason ->
			    io:format("Error updating ~s:~n~p~n", [URL, Reason])
		    end;
		true ->
		    Sleep = LastUpdate1 - MinUpdate,
		    io:format("Sleep ~B before updating ~s~n", [Sleep, URL]),
		    receive
		    after Sleep * 1000 ->
			    ok
		    end
	    end;
	{ok, []} ->
	    io:format("Nothing to update~n"),
	    receive
	    after ?INTERVAL * 1000 ->
		    ok
	    end
    end.
%%?MODULE:update_loop().


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
		{error, {Etag1, LastModified1}, not_modified};
	    {error, Reason1} ->
		{error, {Etag1, LastModified1}, Reason1}
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
