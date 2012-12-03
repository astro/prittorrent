
-module(hasher_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, recheck/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

pick_any_worker() ->
    Pids = [Pid
	    || {{worker, _}, Pid, _, _} <- supervisor:which_children(?MODULE)],
    random:seed(erlang:now()),
    lists:nth(random:uniform(length(Pids)), Pids).

recheck(URL) ->
    Pid = pick_any_worker(),
    link(Pid),
    Pid ! {recheck, URL, self()},
    receive
	recheck_done ->
	    unlink(Pid),
	    ok
    end.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    NWorkers = 2,
    Workers = [{{worker, N}, {hasher_worker, start_link, []},
		permanent, 5000, worker, [hasher_worker, hasher_hash]}
	       || N <- lists:seq(1, NWorkers)],
    NRecheckers = 5,
    Recheckers = [{{checker, N}, {hasher_recheck, start_link, []},
		   permanent, 5000, worker, [hasher_recheck]}
		  || N <- lists:seq(1, NRecheckers)],
    {ok, { {one_for_one, 5, 10}, Workers ++ Recheckers} }.

