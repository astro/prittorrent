-module(ui_app).

-behaviour(application).

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch =
	[
	 %% {Host, list({Path, Handler, Opts})}
	 {'_', [{[<<"static">>, '...'],
		 cowboy_http_static,
		 [{directory, {priv_dir, ui, [<<"static">>]}},
		  {etag, {attributes, [filepath, filesize, inode, mtime]}},
		  {mimetypes, [{<<".css">>, [<<"text/css">>]},
			       {<<".js">>, [<<"application/javascript">>]},
			       {<<".png">>, [<<"image/png">>]},
			       {<<".gif">>, [<<"image/gif">>]}
			      ]}
		 ]},
		{'_', ui_handler, []}
	       ]
	 }
	],
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    cowboy:start_listener(
      ui_http_listener, 32,
      cowboy_tcp_transport, [{port, 8080}],
      cowboy_http_protocol, [{dispatch, Dispatch}]
     ).

prep_stop(_State) ->
    cowboy:stop_listener(ui_http_listener).

stop(_State) ->
    ok.
