
-module(model_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Pools) ->
    supervisor:start_link(?MODULE, [Pools]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Pools]) ->
    ChildSpecs =
	[{Id, {model_pool, start_link, [{Id, Options}]},
	  permanent, 2000, worker, [model_pool]}
	 || {Id, Options} <- Pools],
    {ok, { {one_for_one, 5, 10}, ChildSpecs} }.

