-module(model_scrape_queue).

-behaviour(gen_server).

%% API
-export([update_scraped/1, start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {queue = queue:new(),
		queued = sets:new(),
		worker
	       }).

-define(POOL, pool_scrape_queue).
-define(Q(Stmt, Params), model_sup:equery(?POOL, Stmt, Params)).

%%%===================================================================
%%% API
%%%===================================================================
update_scraped(InfoHash) ->
    gen_server:cast(?SERVER, {update_scraped, InfoHash}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, invalid, State}.

handle_cast({worker_done, Pid}, #state{worker = Pid} = State1) ->
    State2 = may_work(State1#state{worker = undefined}),
    {noreply, State2};

handle_cast({update_scraped, InfoHash},
	    #state{queue = Queue1,
		   queued = Queued1} = State1) ->
    case sets:is_element(InfoHash, Queued1) of
	true ->
	    %% Already queued
	    Queue2 = Queue1,
	    Queued2 = Queued1;
	false ->
	    %% Enqueue
	    Queue2 = queue:in(InfoHash, Queue1),
	    Queued2 = sets:add_element(InfoHash, Queued1)
    end,
    State2 = State1#state{queue = Queue2,
			  queued = Queued2},

    State3 = may_work(State2),
    {noreply, State3}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{queue = Queue1} = State) ->
    case queue:out(Queue1) of
	{empty, _} ->
	    ok;
	{{value, InfoHash}, Queue2} ->
	    catch do_work(InfoHash),
	    terminate(Reason, State#state{queue = Queue2})
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

may_work(#state{worker = Worker} = State) when is_pid(Worker) ->
    %% A worker is still running
    State;

may_work(#state{queue = Queue1,
		queued = Queued1} = State) ->
    case queue:out(Queue1) of
	{empty, _} ->
	    %% Nothing to do
	    State;
	{{value, InfoHash}, Queue2} ->
	    Queued2 = sets:del_element(InfoHash, Queued1),
	    I = self(),
	    Worker = spawn_link(
		       fun() ->
			       do_work(InfoHash),
			       gen_server:cast(I, {worker_done, self()})
		       end),
	    State#state{queue = Queue2,
			queued = Queued2,
			worker = Worker}
    end.

do_work(InfoHash) ->
    {ok, _, _} =
	?Q("SELECT * FROM update_scraped($1)", [InfoHash]).

