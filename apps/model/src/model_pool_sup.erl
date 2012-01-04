-module(model_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(PoolSize, Options, PoolPid) ->
    supervisor:start_link(?MODULE, [PoolSize, Options, PoolPid]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([PoolSize, Options, PoolPid]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    ChildSpecs =
	[{N, {model_pool, start_pgsql_connection, [Options, PoolPid]},
	  permanent, 1000, worker, [model_pool]}
	 || N <- lists:seq(1, PoolSize)],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
