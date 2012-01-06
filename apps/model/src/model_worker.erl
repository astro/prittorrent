-module(model_worker).
-behaviour(gen_server).

-export([start_link/1, stop/0, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {conn}).

start_link(Args) -> gen_server:start_link(?MODULE, Args, []).
stop() -> gen_server:cast(?MODULE, stop).

init(Args) ->
    Host = proplists:get_value(host, Args),
    Database = proplists:get_value(database, Args),
    User = proplists:get_value(user, Args),
    Password = proplists:get_value(password, Args),
    {ok, Conn} = pgsql:connect(Host, User, Password, [
        {database, Database}
    ]),
    {ok, #state{conn=Conn}}.

handle_call({equery, Stmt, Params}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:equery(Conn, Stmt, Params), State};
handle_call({transaction, Fun}, _From, #state{conn=Conn}=State) ->
    {reply,
     pgsql:with_transaction(Conn,
			    fun(Conn2) ->
				    Fun(fun(Stmt, Params) ->
						pgsql:equery(Conn2, Stmt, Params)
					end)
			    end), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(stop, State) ->
    {stop, shutdown, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn=Conn}) ->
    pgsql:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
