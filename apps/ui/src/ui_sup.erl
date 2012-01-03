
-module(ui_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1, start_misultin/0]).

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
    Misultin = {misultin, {?MODULE, start_misultin, []},
		permanent, 2000, worker, [misultin]},
    {ok, { {one_for_one, 5, 10}, [Misultin]} }.


start_misultin() ->
    Options = [{ip, {0, 0, 0, 0, 0, 0, 0, 0}},
	       {port, 8080},
	       {loop, fun ui_route:handle_http/1},
	       {ws_loop, fun ui_route:handle_websocket/1}
	      ],
    misultin:start_link(Options).
