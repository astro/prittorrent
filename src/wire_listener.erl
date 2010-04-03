-module(wire_listener).


%% API
-export([get_port/0, start_link/1, loop/1]).


-define(SERVER, ?MODULE). 

-record(state, {sock}).

%%%===================================================================
%%% API
%%%===================================================================

get_port() ->
    application:get_env(servtorrent, wire_port).
    

start_link(Port) ->
    io:format("start_link~n"),
    I = self(),
    Pid = spawn_link(fun() ->
			     {ok, State} = init(Port),
			     I ! {ok, self()},
			     loop(State)
		     end),
    receive
	{ok, Pid1} when Pid == Pid1 ->
	    register(?SERVER, Pid),
	    {ok, Pid}
    after 5000 ->
	    exit(timeout)
    end.

%%%===================================================================
%%%===================================================================

init(Port) ->
    io:format("init~n"),
    %% TODO: inet6
    {ok, Sock} = gen_tcp:listen(Port, [binary, inet]),
    application:set_env(servtorrent, wire_port, Port),
    {ok, #state{sock = Sock}}.


loop(#state{sock = Sock} = State) ->
    io:format("loop~n"),
    {ok, Client} = gen_tcp:accept(Sock),
    start_connection(Client),
    ?MODULE:loop(State).

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_connection(Client) ->
    io:format("start_connection~n"),
    {ok, Pid} = peer_sup:start_peer(Client),
    ok = gen_tcp:controlling_process(Client, Pid),
    gen_server:cast(Pid, go),
    unlink(Client).
