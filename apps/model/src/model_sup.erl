
-module(model_sup).

-behaviour(supervisor).

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
	%% TODO: catch?
	Reply = gen_server:call(Worker, {equery, Stmt, Params}),
	poolboy:checkin(PoolId, Worker),
	Reply.

transaction(PoolId, Fun) ->
	Worker = poolboy:checkout(PoolId),
	%% TODO: catch?
	Reply = gen_server:call(Worker, {transaction, Fun}),
	poolboy:checkin(PoolId, Worker),
	Reply.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Pools]) ->
    ChildSpecs =
	[{Id, {poolboy, start_link, [[{name, {local, Id}},
				      {worker_module, model_worker}]
				     ++ Options]},
	  permanent, 2000, worker, [model_pool]}
	 || {Id, Options} <- Pools],
    {ok, { {one_for_one, 5, 10}, ChildSpecs} }.

