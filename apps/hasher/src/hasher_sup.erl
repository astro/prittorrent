
-module(hasher_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    NWorkers = 2,
    Workers = [{{worker, N}, {hasher_worker, start_link, []},
		permanent, 5000, worker, [hasher_worker, hasher_hash]}
	       || N <- lists:seq(1, NWorkers)],
    {ok, { {one_for_one, 5, 10}, Workers} }.

