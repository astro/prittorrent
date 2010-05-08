-module(wire_listener).


%% API
-export([get_port/0, start_link/0, loop/1]).


-define(SERVER, ?MODULE). 

-record(state, {sock}).

%%%===================================================================
%%% API
%%%===================================================================

get_port() ->
    application:get_env(servtorrent, wire_port).
    

start_link() ->
    I = self(),
    Pid = spawn_link(fun() ->
			     {ok, State} = init(),
			     I ! {ok, self()},
			     loop(State)
		     end),
    receive
	{ok, Pid1} when Pid == Pid1 ->
	    register(?SERVER, Pid),
	    {ok, Pid}
    after 1000 ->
	    {error, timeout}
    end.

%%%===================================================================
%%%===================================================================

init() ->
    %% TODO: inet6
    {ok, Port} = get_port(),
    {ok, Sock} = gen_tcp:listen(Port, [binary, inet]),
    application:set_env(servtorrent, wire_port, Port),
    logger:log(wire, info,
	       "Listening on port ~B", [Port]),
    {ok, #state{sock = Sock}}.


loop(#state{sock = Sock} = State) ->
    {ok, Client} = gen_tcp:accept(Sock),
    case (catch start_connection(Client)) of
	{'EXIT', _} ->
	    logger:log(wire, error,
		       "Cannot start connection on accepted socket ~p", [Client]);
	_ -> ok
    end,
    ?MODULE:loop(State).

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_connection(Client) ->
    {ok, {Address, Port}} = inet:peername(Client),
    logger:log(wire, info,
	       "Accepted connection from ~p:~B", [Address, Port]),
    {ok, Pid} = peer_sup:start_peer(Client),
    ok = gen_tcp:controlling_process(Client, Pid),
    gen_server:cast(Pid, go),
    unlink(Client).
