-module(ui_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% Cowboy web server
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

    IP = case os:getenv("BIND_IP") of
	     false ->
		 %% Development
		 {0, 0, 0, 0, 0, 0, 0, 0};
	     IP1 ->
		 %% Production
		 {ok, IP2} = inet_parse:address(IP1),
		 IP2
	 end,
    Port = case os:getenv("BIND_PORT") of
	       false ->
		   %% Development
		   8080;
	       Port1 ->
		   list_to_integer(Port1)
	   end,
    SSLOpts =
	case os:getenv("SSL_CERT") of
	    false -> [];
	    SSLCert -> [{certfile, SSLCert}]
	end ++
	case os:getenv("SSL_KEY") of
	    false -> [];
	    SSLKey -> [{keyfile, SSLKey}]
	end ++
	case os:getenv("SSL_CACERT") of
	    false -> [];
	    SSLCACert -> [{cacertfile, SSLCACert}]
	end,

    {Transport, TransportOpts} =
	case lists:keymember(certfile, 1, SSLOpts) of
	    true ->
		Ciphers = [CipherSuite
			   || {_Kex, Cipher, Hash} = CipherSuite <- ssl:cipher_suites(),
			      Cipher =/= des_cbc,
			      Cipher =/= rc4_128,
			      Hash =/= md5
			  ],
		{cowboy_ssl_transport, [{ciphers, Ciphers} |
					SSLOpts]};
	    false ->
		{cowboy_tcp_transport, []}
	end,

    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    cowboy:start_listener(
      ui_http_listener, 32,
      Transport, [{ip, IP},
		  {port, Port} |
		  TransportOpts
		 ],
      cowboy_http_protocol, [{dispatch, Dispatch}]
     ).

stop(_State) ->
    cowboy:stop_listener(ui_http_listener).
