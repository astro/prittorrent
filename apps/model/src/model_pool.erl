-module(model_pool).

-behaviour(gen_server).

%% API
-export([start_link/1, request/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, start_pgsql_connection/2]).

-record(state, {idle_conns = queue:new(),
		queued_reqs = queue:new()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link({Name, Options}) ->
    gen_server:start_link({local, Name}, ?MODULE, [Options], []).

request(Name, Fun) ->
    {ok, Result} = gen_server:call(Name, {request, Fun}),
    Result.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Options]) ->
    PoolSize = proplists:get_value(pool_size, Options, 8),
    {ok, _} = model_pool_sup:start_link(PoolSize, Options, self()),
    {ok, #state{}}.

start_pgsql_connection(Options, PoolPid) ->
    Host = proplists:get_value(host, Options, "localhost"),
    User = proplists:get_value(user, Options, "prittorrent"),
    Password = proplists:get_value(password, Options),
    {ok, Conn} = pgsql:connect(Host, User, Password, Options),
    gen_server:cast(PoolPid, {release, Conn}),
    {ok, Conn}.

handle_call({request, Fun}, From, #state{queued_reqs = QueuedReqs,
					 idle_conns = IdleConns} = State) ->
    Queued = {queued, Fun, From},
    case queue:out(IdleConns) of
	{{value, Conn}, IdleConns2} ->
	    run(Queued, Conn),
	    {noreply, State#state{idle_conns = IdleConns2}};
	{empty, _} ->
	    QueuedReqs2 = queue:in(Queued, QueuedReqs),
	    {noreply, State#state{queued_reqs = QueuedReqs2}}
    end.

handle_cast({release, Conn}, #state{queued_reqs = QueuedReqs,
				    idle_conns = IdleConns} = State) ->
    case queue:out(QueuedReqs) of
	{{value, Queued}, QueuedReqs2} ->
	    run(Queued, Conn),
	    {noreply, State#state{queued_reqs = QueuedReqs2}};
	{empty, _} ->
	    IdleConns2 = queue:in(Conn, IdleConns),
	    {noreply, State#state{idle_conns = IdleConns2}}
    end.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

run({queued, From, Fun}, Conn) ->
    I = self(),
    spawn_link(fun() ->
		       try Fun(Conn) of
			   Result ->
			       gen_server:reply(From, {ok, Result})
		       catch E ->
			       gen_server:reply(From, {error, E})
		       end,
		       gen_server:cast(I, {release, Conn})
	       end).
