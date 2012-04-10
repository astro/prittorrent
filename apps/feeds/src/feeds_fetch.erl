%% TODO: etags/last-modified support
-module(feeds_fetch).

-export([fetch/1]).


fetch(Url) ->
    Parser = exmpp_xml:start_parser([{max_size, 30 * 1024 * 1024},
					{names_as_atom, false},
					{engine, expat}]),
    Els1 =
	http_fold(Url, fun(Chunk, Els) ->
			       case exmpp_xml:parse(Parser, Chunk) of
				   continue ->
				       Els;
				   Els1 when is_list(Els1) ->
				       Els1 ++ Els
			       end
		       end, []),
    Els2 = case exmpp_xml:parse_final(Parser, <<"">>) of
	       done ->
		   [];
	       Els3 when is_list(Els3) ->
		   Els3
	   end,
    %% At least one:
    Els = [RootEl | _] = Els1 ++ Els2,
    ok = exmpp_xml:stop_parser(Parser),
    io:format("Downloaded ~s - ~p~n",[Url,length(Els)]),

    io:format("Title: ~p~n", [feeds_parse:title(RootEl)]),	    
    {ok, FeedEl, ItemEls} = feeds_parse:pick_items(RootEl),
    %%io:format("FeedEl: ~p~n", [FeedEl]),
    io:format("ItemEls: ~p~n", [length(ItemEls)]),
    lists:foreach(
      fun(ItemEl) ->
	      io:format("== ~s ==~n", [feeds_parse:item_title(ItemEl)]),
	      lists:foreach(
		fun(Enclosure) ->
			io:format("* ~s~n", [Enclosure])
		end, feeds_parse:item_enclosures(ItemEl))
      end, ItemEls).

%% TODO: use storage, handle pcast://
http_fold(Url, Fold, AccIn) ->
    Headers =
	[{"User-Agent", "PritTorrent/0.1"}],
    {ibrowse_req_id, ReqId} =
	ibrowse:send_req(Url, Headers, get, [],
			 [{stream_to, {self(), once}}], 10000),
    http_fold1(ReqId, Fold, AccIn).

http_fold1(ReqId, Fold, AccIn) ->
    ok = ibrowse:stream_next(ReqId),
    receive
	{ibrowse_async_headers, ReqId, [$2, _, _], _Headers} ->
	    http_fold1(ReqId, Fold, AccIn);
	{ibrowse_async_headers, ReqId, StatusS, _Headers} ->
	    {Status, _} = string:to_integer(StatusS),
	    exit({http, Status});
	{ibrowse_async_response, ReqId, {error, Err}} ->
	    exit(Err);
	{ibrowse_async_response, ReqId, Data} ->
	    AccOut = Fold(list_to_binary(Data), AccIn),
	    http_fold1(ReqId, Fold, AccOut);
	{ibrowse_async_response_end, ReqId} ->
	    AccIn
    end.
