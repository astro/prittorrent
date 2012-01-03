-module(tracker_manager).
-behaviour(gen_server).

%gen server stuff
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).

% actual api
-export([check_timeout/0, set/2, get/1]).
 
-define(SERVER, ?MODULE).


-record(state,{timers=[], interval = 10*16}).
 
%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

check_timeout() ->
	gen_server:call(?SERVER, check_timeout, 15 * 60 * 1000).


% currently supported: interval
get(Atom) ->
	gen_server:call(?SERVER, {get, Atom}).
	
set(Atom, Value) ->
	gen_server:call(?SERVER, {set, Atom, Value}).


%% gen_server callbacks  
init([]) ->
    {ok, #state{timers = init_timers()}}.

init_timers() ->
	{ok, Timer} = timer:apply_interval(5 * 60 * 1000, ?MODULE, check_timeout, []),
	[Timer].

handle_call({set, interval, Value}, _From, State) ->
	NewState = State#state{interval = Value},
	Result = ok,
	{reply, Result, NewState};

handle_call({get, interval}, _From, State) ->
	Result = {ok, State#state.interval},
	{reply, Result, State};
 
handle_call(check_timeout, _From, State) ->
	% check timeouts on registered peers
	trackerdb:remove_peers_with_timeout_in_seconds( State#state.interval + State#state.interval / 4),
	{reply, ok, State}.

  
% handle death and cleanup of logged in processes
handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
	lists:foreach(fun timer:cancel/1,State#state.timers), % cancel all timers
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    