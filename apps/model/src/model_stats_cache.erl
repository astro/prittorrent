-module(model_stats_cache).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {counters = gb_trees:empty()}).

-define(CACHE_FLUSH, 10000).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({add_counter, Kind, InfoHash, Increment},
	    #state{counters = Counters1} = State) ->
    Counters2 = counters_add_counter(
		  Counters1, {Kind, InfoHash}, Increment),
    {noreply, State#state{counters = Counters2}}.

handle_info({flush, {Kind, InfoHash} = Key},
	    #state{counters = Counters1} = State) ->
    {ok, Value, Counters2} =
	counters_flush_counter(Counters1, Key),
    model_stats:do_add_counter(Kind, InfoHash, Value),
    {noreply, State#state{counters = Counters2}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{counters = Counters}) ->
    lists:foreach(
      fun({{Kind, InfoHash}, Value}) ->
	      model_stats:do_add_counter(Kind, InfoHash, Value)
      end, gb_trees:to_list(Counters)),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

counters_add_counter(Counters, Key, Increment) ->
    Value2 =
	case gb_trees:lookup(Key, Counters) of
	    none ->
		timer:send_after(?CACHE_FLUSH, {flush, Key}),
		0;
	    {value, Value1} ->
		Value1
	end,
    Value3 = Value2 + Increment,
    gb_trees:enter(Key, Value3, Counters).

counters_flush_counter(Counters1, Key) ->
    Value = gb_trees:get(Key, Counters1),
    Counters2 = gb_trees:delete(Key, Counters1),
    {ok, Value, Counters2}.
