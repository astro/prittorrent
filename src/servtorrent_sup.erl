-module(servtorrent_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Logger = {logger,
	      {logger, start_link, []},
	      permanent, 2000, worker, [logger]},
    WireListener = {wire_listener,
		    {wire_listener, start_link, []},
		    permanent, brutal_kill, worker, [wire_listener]},
    PeerSup = {peer_sup,
	       {peer_sup, start_link, []},
	       permanent, 2000, supervisor, [peer_sup]},
    SeedList = {seedlist,
		{seedlist, start_link, []},
		permanent, 2000, worker, [seedlist]},

    {ok, {SupFlags, [Logger, WireListener, PeerSup, SeedList]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
