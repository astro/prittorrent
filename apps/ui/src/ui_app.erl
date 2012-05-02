-module(ui_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

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
			       {<<".gif">>, [<<"image/gif">>]},
			       {<<".svg">>, [<<"image/svg+xml">>]}
			      ]}
		 ]},
		{[<<"announce">>], ui_tracker_handler, []},
		{[<<"scrape">>], ui_tracker_handler, []},
		{'_', ui_handler, []}
	       ]
	 }
	],

    {IP, Port} =
	case os:getenv("BIND_IP") of
	    false ->
		%% Development
		{{0, 0, 0, 0, 0, 0, 0, 0}, 8080};
	    IP1 ->
		%% Production
		{ok, IP2} = inet_parse:address(IP1),
		{IP2, 80}
	end,

    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    cowboy:start_listener(
      ui_http_listener, 32,
      cowboy_tcp_transport, [{ip, IP},
			     {port, Port}],
      cowboy_http_protocol, [{dispatch, Dispatch}]
     ).

stop(_State) ->
    cowboy:stop_listener(ui_http_listener).
