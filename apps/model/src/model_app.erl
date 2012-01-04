-module(model_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format("start~n"),
    {ok, Pools} = application:get_env(pools),
    {ok, Sup} = model_sup:start_link(Pools).

stop(_State) ->
    ok.
