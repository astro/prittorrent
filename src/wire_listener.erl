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
    io:format("start_link~n"),
    I = self(),
    Pid = spawn_link(fun() ->
			     {ok, State} = init(),
			     I ! {ok, self()},
			     io:format("ok~n"),
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
    io:format("init~n"),
    %% TODO: inet6
    {ok, Port} = get_port(),
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
