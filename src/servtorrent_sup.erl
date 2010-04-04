-module(servtorrent_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([Port]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    WireListener = {wire_listener,
		    {wire_listener, start_link, [Port]},
		    permanent, brutal_kill, worker, [wire_listener]},
    PeerSup = {peer_sup,
	       {peer_sup, start_link, []},
	       permanent, 2000, supervisor, [peer_sup]},
    SeedList = {seedlist,
		{seedlist, start_link, ["seeds.xml"]},
		permanent, 2000, worker, [seedlist]},

    {ok, {SupFlags, [WireListener, PeerSup, SeedList]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
