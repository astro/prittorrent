-module(model_sup).

-behaviour(supervisor).

-define(TIMEOUT, 120000).

%% API
-export([start_link/1, equery/3, transaction/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Pools) ->
    supervisor:start_link(?MODULE, [Pools]).

equery(PoolId, Stmt, Params) ->
    Worker = poolboy:checkout(PoolId),
    Reply = gen_server:call(Worker, {equery, Stmt, Params}, ?TIMEOUT),
    poolboy:checkin(PoolId, Worker),
    Reply.

transaction(PoolId, Fun) ->
    Worker = poolboy:checkout(PoolId),
    Reply = gen_server:call(Worker, {transaction, Fun}, ?TIMEOUT),
    poolboy:checkin(PoolId, Worker),
    case Reply of
        {rollback, Why} -> exit(Why);
	_ -> Reply
    end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Pools]) ->
    ChildSpecs =
	[{model_stats_cache, {model_stats_cache, start_link, []},
	  permanent, 2000, worker, [model_stats_cache, model_stats]},
	 {model_scrape_queue, {model_scrape_queue, start_link, []},
	  permanent, 2000, worker, [model_scrape_queue]} |
	 [{Id, {poolboy, start_link, [[{name, {local, Id}},
				       {worker_module, model_worker}]
				      ++ Options]},
	   permanent, 2000, worker, [model_pool]}
	  || {Id, Options} <- Pools]],
    {ok, { {one_for_one, 5, 10}, ChildSpecs} }.

