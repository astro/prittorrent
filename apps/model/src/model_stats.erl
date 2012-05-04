-module(model_stats).

-export([set_gauge/3,
	 add_counter/3, do_add_counter/3]).

-define(POOL, pool_stats).
-define(Q(Stmt, Params), model_sup:equery(?POOL, Stmt, Params)).
-define(C(Stmt, Params),
	(case (catch ?Q(Stmt, Params)) of
	     {ok, _, _} ->
		 ok;
	     E ->
		 io:format("model_stats query: ~p~n", [E]),
		 error
	 end)).

%%
%% Gauge w/o caching for now
%%

set_gauge(Kind, InfoHash, Value) when is_atom(Kind) ->
    set_gauge(
      list_to_binary(atom_to_list(Kind)),
      InfoHash, Value);

set_gauge(Kind, InfoHash, Value)
  when is_binary(Kind),
       is_binary(InfoHash),
       is_integer(Value) ->
    ?C("SELECT * FROM set_gauge($1, $2, $3);",
       [Kind, InfoHash, Value]);
  
set_gauge(Kind, _InfoHash, Value)
  when is_binary(Kind),
       is_integer(Value) ->
    ?C("SELECT * FROM set_gauge($1, NULL, $3);",
       [Kind, Value]).

%%
%% Caching proxy
%%

-define(STATS_CACHE, model_stats_cache).

add_counter(_Kind, _InfoHash, 0) ->
    %% Nothing to do
    ok;

add_counter(Kind, InfoHash, Increment) when is_atom(Kind) ->
    add_counter(
      list_to_binary(atom_to_list(Kind)),
      InfoHash, Increment);

add_counter(Kind, InfoHash, Increment) ->
    gen_server:cast(?STATS_CACHE, {add_counter, Kind, InfoHash, Increment}).


%%
%% Actual functionality
%%


do_add_counter(Kind, InfoHash, Increment)
  when is_binary(Kind),
       is_binary(InfoHash),
       is_integer(Increment) ->
    ?C("SELECT * FROM add_counter($1, $2, $3);",
       [Kind, InfoHash, Increment]);

do_add_counter(Kind, _InfoHash, Increment)
  when is_binary(Kind),
       is_integer(Increment) ->
    ?C("SELECT * FROM add_counter($1, NULL, $3);",
       [Kind, Increment]).
