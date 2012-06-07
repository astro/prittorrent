-module(model_graphs).

-export([get_counter/5, get_gauge/5]).

-define(POOL, pool_graphs).
-define(Q(Stmt, Params), model_sup:equery(?POOL, Stmt, Params)).
-define(C(Stmt, Params),
	(case (catch ?Q(Stmt, Params)) of
	     {ok, _, _} ->
		 ok;
	     E ->
		 io:format("model_stats query: ~p~n", [E]),
		 error
	 end)).


get_counter(Kind, InfoHash, Start, Stop, Interval) ->
    {ok, _, Rows} =
	?Q("SELECT TO_TIMESTAMP(FLOOR(EXTRACT(EPOCH FROM \"time\") / $5) * $5) AS t, SUM(\"value\") FROM counters WHERE \"kind\"=$1 AND \"info_hash\"=$2 AND \"time\">=$3 AND \"time\"<=$4 GROUP BY t ORDER BY t ASC",
	   [Kind, InfoHash, Start, Stop, Interval]),
    [{Date, list_to_integer(binary_to_list(Sum))}
     || {Date, Sum} <- Rows].

get_gauge(Kind, InfoHash, Start, Stop, Interval) ->
    Agg = case Kind of
	      leechers -> "MIN";
	      _ -> "MAX"
	  end,
    {ok, _, Rows} =
	?Q("SELECT TO_TIMESTAMP(FLOOR(EXTRACT(EPOCH FROM \"time\") / $5) * $5) AS t, " ++ Agg ++ "(\"value\") FROM gauges WHERE \"kind\"=$1 AND \"info_hash\"=$2 AND \"time\">=$3 AND \"time\"<=$4 GROUP BY t ORDER BY t ASC",
	   [Kind, InfoHash, Start, Stop, Interval]),
    Rows.
