-module(html).

-export([to_iolist/1]).

to_iolist(S) when is_atom(S) ->
    to_iolist(atom_to_list(S));

to_iolist(S) when is_list(S) ->
    %% Recurse
    lists:map(fun to_iolist/1, S);

to_iolist(S) when is_binary(S) ->
    escape(S);

to_iolist(S) when is_integer(S) ->
    case escape([S]) of
	[R] when is_integer(R) ->
	    R;
	R ->
	    R
    end;

to_iolist({'!CDATA', S}) ->
    [<<"<![CDATA[">>, S, <<"]]>">>];

to_iolist({El, Children}) when is_atom(El) ->
    to_iolist({atom_to_list(El), Children});

to_iolist({El, Children}) ->
    [<<"<">>, El,
     case Children of
	 [] -> <<"/>">>;
	 _ -> [<<">">>,
	       to_iolist(Children),
	       <<"</">>, El, <<">">>]
     end];

to_iolist({El, Attrs, Children}) when is_atom(El) ->
    to_iolist({atom_to_list(El), Attrs, Children});

to_iolist({El, Attrs, Children}) ->
    [<<"<">>, El,
     [[<<" ">>, 
       if
	   is_binary(K);
	   is_list(K) ->
	       K;
	   is_atom(K) ->
	       atom_to_list(K);
	   true ->
	       throw({invalid_attr, K})
       end, <<"=\"">>, escape_attr(V), <<"\"">>]
      || {K, V} <- Attrs],
     case Children of
	 [] -> <<"/>">>;
	 _ ->
	     [<<">">>,
	      to_iolist(Children),
	      <<"</">>, El, <<">">>]
     end].


escape(Bin) when is_binary(Bin) ->
    escape(binary_to_list(Bin));
escape(L) when is_list(L) ->
    lists:map(fun escape/1, L);
escape($&) ->
    <<"&amp;">>;
escape($<) ->
    <<"&lt;">>;
escape($>) ->
    <<"&gt;">>;
escape(C) when is_integer(C) ->
    C.

escape_attr(Bin) when is_binary(Bin) ->
    escape_attr(binary_to_list(Bin));
escape_attr(L) when is_list(L) ->
    lists:map(fun escape_attr/1, L);
escape_attr($&) ->
    <<"&amp;">>;
escape_attr($<) ->
    <<"&lt;">>;
escape_attr($>) ->
    <<"&gt;">>;
escape_attr($') ->
    <<"&apos;">>;
escape_attr($") ->
    <<"&quot;">>;
escape_attr(C) when is_integer(C) ->
    C.
