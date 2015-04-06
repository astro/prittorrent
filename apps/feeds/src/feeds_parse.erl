-module(feeds_parse).

-export([serialize/1,
	 get_type/1, get_channel/1,
	 title/1, lang/1, summary/1, link/1, image/1,
	 pick_items/1,
	 item_id/1, item_title/1, item_lang/1, item_summary/1, item_enclosures/1,
	 item_published/1, item_link/1, item_payment/1, item_image/1]).

-include_lib("exmpp/include/exmpp_xml.hrl").

-define(NS_ATOM, "http://www.w3.org/2005/Atom").
-define(NS_BITLOVE, "http://bitlove.org").

-define(DEFAULT_XMLNS, [{"http://www.w3.org/XML/1998/namespace", "xml"}]).

serialize(Xml) ->
    exmpp_xml:node_to_binary(Xml, [], ?DEFAULT_XMLNS).

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

lang(Xml) ->
    case exmpp_xml:get_name_as_list(Xml) of
	"language" ->
	    normalize_language(exmpp_xml:get_cdata(Xml));
	_ ->
	    case normalize_language(
		   exmpp_xml:get_attribute_as_binary(Xml, <<"lang">>, undefined)) of
		Lang when is_binary(Lang) ->
		    Lang;
		_ ->
		    lists:foldl(fun(Child, undefined) ->
					lang(Child);
				   (_, Lang) ->
					Lang
				end, undefined, exmpp_xml:get_child_elements(Xml))
	    end
    end.

normalize_language(Lang1) when is_binary(Lang1) ->
    Lang2 = list_to_binary(
	      string:to_lower(
		binary_to_list(Lang1))),
    normalize_language1(Lang2);
normalize_language(_) ->
    undefined.

normalize_language1(<<Lang:2/binary>>) ->
    Lang;
normalize_language1(<<Lang:2/binary, "-", _/binary>>) ->
    Lang;
normalize_language1(<<Lang:2/binary, "_", _/binary>>) ->
    Lang;
normalize_language1(<<"english">>) ->
    <<"en">>;
normalize_language1(<<"german">>) ->
    <<"de">>;
normalize_language1(<<"deutsch">>) ->
    <<"de">>;
normalize_language1(<<"russian">>) ->
    <<"ru">>;
normalize_language1(<<"espa", _/binary>>) ->
    <<"es">>;
normalize_language1(<<"spanish">>) ->
    <<"es">>;
normalize_language1(<<"ital", _/binary>>) ->
    <<"it">>;
normalize_language1(<<"fran", _/binary>>) ->
    <<"fr">>;
normalize_language1(<<"french">>) ->
    <<"fr">>;
normalize_language1(_) ->
    undefined.


%% We name it `summary', but we really want just the briefest description.
summary(Xml) ->
    case exmpp_xml:get_elements(Xml, "summary") of
	[SummaryEl | _] ->
	    exmpp_xml:get_cdata(SummaryEl);
	_ ->
	    case exmpp_xml:get_element(Xml, "subtitle") of
		[SubtitleEl | _] ->
		    exmpp_xml:get_cdata(SubtitleEl);
		_ ->
		    undefined
	    end
    end.

-spec(image/1 :: (xmlel()) -> binary() | undefined).
image(Xml) ->
    image1(Xml, ["image", "logo", "icon", "itunes:image"]).
%% TODO: "itunes:"? What happened to XML namespaces?

image1(_, []) ->
    undefined;
image1(Xml, [ChildName | ChildNames]) ->
    R =
	lists:foldl(
	  fun(Child, undefined) ->
		  %% Default: @url
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
			  %% Fallback 1: text()
			  case exmpp_xml:get_cdata(Child) of
			      Cdata
				when is_binary(Cdata),
				     size(Cdata) > 0 ->
				  Cdata;
			      _ ->
				  %% Fallback 2: @href
				  exmpp_xml:get_attribute_as_binary(Child, <<"href">>, undefined)
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
	    Entries =
		lists:filter(
		  fun(#xmlel{name="entry"}) ->
			  true;
		     (_) ->
			  false
		  end, RootEl#xmlel.children),
	    {ok, Entries};

	%% Assume RSS
	_ ->
	    Items =
		lists:map(
		  fun(#xmlel{name="channel"}=Channel) ->
			  lists:filter(
			    fun(#xmlel{name="item"}) ->
				    true;
			       (_) ->
				    false
			    end, Channel#xmlel.children);
		     (_) ->
			  []
		  end, RootEl#xmlel.children),
	    {ok, lists:append(Items)}
    end.


item_title(ItemEl) ->
    %% Works the same as for the feed in RSS & ATOM
    title(ItemEl).

item_lang(ItemEl) ->
    lang(ItemEl).

item_summary(ItemEl) ->
    summary(ItemEl).

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
    Published =
	item_published1(ItemEl, ["pubDate", "published", "updated"]),
    try fix_timestamp(Published) of
	<<Result/binary>> ->
	    Result
    catch exit:_Reason ->
	    Published
    end.

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

%% Some date parsing for item_published1/2
fix_timestamp(undefined) ->
    util:iso8601(calendar:local_time(), local);

fix_timestamp(S) ->
    %% Tue, 12 Jun 2012 22:43:48 +0200
    case run_re(S, "(\\d+) (\\S+) (\\d+) (\\d+):(\\d+):(\\d+)\\s*([0-9:+\\-]*)") of
	{match, [Day, <<Mon:3/binary, _/binary>>, Year, Hour, Min, Sec, Tz]} ->
	    Date = {bin_to_int(Year), month_to_int(Mon), bin_to_int(Day)},
	    Time = {bin_to_int(Hour), bin_to_int(Min), bin_to_int(Sec)},
	    case run_re(Tz, "([+\\-])(\\d{1,2}):?(\\d*)") of
		{match, [Sig, TzH1, TzM1]} ->
		    TzH =
			case Sig of
			    <<"+">> -> 1;
			    <<"-">> -> -1
			end *
			bin_to_int(TzH1),
		    TzM =
			case TzM1 of
			    <<>> ->
				0;
			    _ ->
				bin_to_int(TzM1)
			end;
		nomatch ->
		    TzH = 0,
		    TzM = 0
	    end,
	    util:iso8601({Date, Time}, {TzH, TzM});
	nomatch ->
	    S
    end.

run_re(S, RE) ->
    case get({?MODULE, cached_re, RE}) of
	undefined ->
	    {ok, CompiledRE} = re:compile(RE),
	    put({?MODULE, cached_re, RE}, CompiledRE);
	CompiledRE ->
	    ok
    end,

    case re:run(S, CompiledRE) of
	{match, [_All | Captures]} ->
	    {match,
	     lists:map(fun({CapStart, CapLength}) ->
			       {_, S1} = split_binary(S, CapStart),
			       {S2, _} = split_binary(S1, CapLength),
			       S2
		       end, Captures)};
	nomatch ->
	    nomatch
    end.

bin_to_int(B) ->
    list_to_integer(
      binary_to_list(B)).

month_to_int(M) ->
    month_to_int1(string:to_lower(binary_to_list(M))).

month_to_int1("jan") -> 1;
month_to_int1("feb") -> 2;
month_to_int1("mar") -> 3;
month_to_int1("apr") -> 4;
month_to_int1("may") -> 5;
month_to_int1("jun") -> 6;
month_to_int1("jul") -> 7;
month_to_int1("aug") -> 8;
month_to_int1("sep") -> 9;
month_to_int1("oct") -> 10;
month_to_int1("nov") -> 11;
month_to_int1("dec") -> 12.


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

-spec(item_enclosures/1 :: (xmlel())
			   -> [{binary(),
				(binary() | undefined),
				(binary() | undefined)
			       }]).
item_enclosures(ItemEl) ->
    lists:reverse(
	lists:foldl(fun(#xmlel{name="enclosure"}=El, URLs) ->
			    Title = exmpp_xml:get_attribute_as_binary(El, <<"title">>, undefined),
			    Type = normalize_type(
				     exmpp_xml:get_attribute_as_binary(El, <<"type">>, undefined)),
			    GUID = exmpp_xml:get_attribute_as_binary(El, ?NS_BITLOVE, <<"guid">>, undefined),
			    case exmpp_xml:get_attribute_as_binary(El, <<"url">>, undefined) of
				URL when is_binary(URL) ->
				    [{URL, Type, Title, GUID} | URLs];
				_ ->
				    case exmpp_xml:get_cdata(El) of
					URL when is_binary(URL), size(URL) > 6 ->
					    [{URL, Type, Title, GUID} | URLs];
					_ ->
					    URLs
				    end
			    end;
		       (#xmlel{name="link"}=El, URLs) ->
			    case exmpp_xml:get_attribute_as_binary(El, <<"rel">>, undefined) of
				<<"enclosure">> ->
				    case exmpp_xml:get_attribute_as_binary(El, <<"href">>, undefined) of
					URL when is_binary(URL), size(URL) > 6 ->
					    Title = exmpp_xml:get_attribute_as_binary(El, <<"title">>, undefined),
					    Type = normalize_type(
						     exmpp_xml:get_attribute_as_binary(El, <<"type">>, undefined)),
					    GUID = exmpp_xml:get_attribute_as_binary(El, ?NS_BITLOVE, <<"guid">>, undefined),
					    [{URL, Type, Title, GUID} | URLs];
					_ ->
					    URLs
				    end;
				_Rel ->
				    URLs
			    end;
		       (_, URLs) ->
			    URLs
		    end, [], exmpp_xml:get_child_elements(ItemEl))).


%% Drop MIME Type parameters ";..."
normalize_type(B) when is_binary(B) ->
    L1 = binary_to_list(B),
    case lists:takewhile(fun(C) ->
				 C =/= $;
			 end, L1) of
	L2 when L1 == L2 ->
	    B;
	L2 ->
	    list_to_binary(L2)
    end;
normalize_type(A) ->
    A.
