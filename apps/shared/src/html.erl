%% This actually produces XHTML. Don't forget the <?xml ...?>!
-module(html).

-export([to_iolist/1]).

to_iolist(S) ->
    Xml1 = to_exmpp_xml(S),
    exmpp_xml:document_to_iolist(Xml1).

to_exmpp_xml(S) when is_atom(S) ->
    exmpp_xml:cdata(S);

to_exmpp_xml(S1) when is_list(S1) ->
    {String, S2} =
	lists:splitwith(fun(C) ->
				is_integer(C)
			end, S1),
    case String of
	[] ->
	    [];
	_ ->
	    [exmpp_xml:cdata(String)]
    end ++
	case S2 of
	    [] ->
		[];
	    [L | S3] when is_list(L) ->
		to_exmpp_xml(L) ++ to_exmpp_xml(S3);
	    [El | S3] ->
		[to_exmpp_xml(El) | to_exmpp_xml(S3)]
	end;

to_exmpp_xml(S) when is_binary(S) ->
    exmpp_xml:cdata(S);

to_exmpp_xml(S) when is_integer(S) ->
    exmpp_xml:cdata([S]);

to_exmpp_xml({El, Children}) when is_atom(El) ->
    exmpp_xml:element(undefined, El, [],
		      to_exmpp_xml(Children));

to_exmpp_xml({El, Attrs, Children}) ->
    exmpp_xml:element(undefined, El,
		      attributes_to_exmpp_xml(Attrs),
		      to_exmpp_xml(Children));

to_exmpp_xml(X) ->
    exit({invalid_html, X}).


attributes_to_exmpp_xml(Attrs) ->
    [exmpp_xml:attribute(exmpp_utils:any_to_binary(N), V)
     || {N, V} <- Attrs].
