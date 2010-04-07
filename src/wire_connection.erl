-module(wire_connection).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {sock, buffer = <<>>,
		mode, step = handshake, info_hash,
		choked = true, interested = false,
		queue = []}).
-record(queued, {piece, offset, length}).

-define(CHOKE, 0).
-define(UNCHOKE, 1).
-define(INTERESTED, 2).
-define(NOT_INTERESTED, 3).
-define(HAVE, 4).
-define(BITFIELD, 5).
-define(REQUEST, 6).
-define(PIECE, 7).
-define(CANCEL, 8).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Param) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Param], []),
    {ok, Pid}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([{InfoHash, IP, Port}]) ->
    {ok, TorrentFile} = torrentdb:get_torrent_file(InfoHash),
    io:format("Connecting to ~p:~p for ~s~n", [IP, Port, TorrentFile]),
    Opts = case IP of
	       {_, _, _, _, _, _, _, _} -> [inet6];
	       _ -> []
	   end,
    {ok, Sock} = gen_tcp:connect(IP, Port, [binary,
					    {active, true}
					    | Opts]),
    State = #state{sock = Sock,
		   info_hash = InfoHash,
		   mode = client},
    send_handshake(State),
    send_extensions(State),
    gen_tcp:send(Sock, InfoHash),
    {ok, MyPeerId} = torrentdb:peer_id(),
    gen_tcp:send(Sock, MyPeerId),
    send_bitfield(State),
    
    {ok, State};
init([Sock]) ->
    link(Sock),
    {ok, #state{sock = Sock,
		mode = server}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(go, #state{sock = Sock} = State) ->
    ok = inet:setopts(Sock, [{active, true}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, Sock, Data}, #state{sock = Sock,
				      buffer = Buffer} = State1) ->
    State2 = State1#state{buffer = list_to_binary([Buffer, Data])},
    State3 = process_input(State2),

    State4 = case {State3#state.step,
		   State3#state.choked} of
		 {run, true} ->
		     send_message(Sock, <<?UNCHOKE>>),
		     State3#state{choked = false};
		 _ ->
		     State3
	     end,
    Queue =
	lists:filter(fun(Queued) ->
			     case (catch send_queued(Queued, State4)) of
				 {'EXIT', Reason} ->
				     io:format("Cannot send ~p: ~p~n", [Queued, Reason]),
				     %% Keep:
				     true;
				 (_) ->
				     %% Ok, remove from queue
				     false
			     end
		     end, State4#state.queue),
    {noreply, State4#state{queue = Queue}};
handle_info({tcp_closed, Sock}, #state{sock = Sock} = State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{info_hash = InfoHash, sock = Sock}) ->
    peerdb:peer_died(InfoHash),
    gen_tcp:close(Sock),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Server mode
%%%===================================================================

%% Waiting for handshake

process_input(#state{mode = server,
		     step = handshake,
		     buffer = <<19,
				"BitTorrent protocol",
				Buffer/binary>>
		    } = State) ->
    send_handshake(State),
    process_input(State#state{step = extensions,
			      buffer = Buffer});

process_input(#state{mode = server,
		     step = handshake,
		     buffer = Buffer} = State)
  when size(Buffer) < 20 ->
    State;

%% Waiting for extensions

process_input(#state{mode = server,
		     step = extensions,
		     buffer = Buffer} = State)
  when size(Buffer) >= 8 ->
    {_, Rest} = split_binary(Buffer, 8),
    send_extensions(State),
    process_input(State#state{step = info_hash,
			      buffer = Rest});

process_input(#state{mode = server,
		     step = extensions} = State) ->
    State;

%% Waiting for info_hash

process_input(#state{mode = server,
		     step = info_hash,
		     buffer = Buffer,
		     sock = Sock} = State)
  when size(Buffer) >= 20 ->
    {InfoHash, Rest} = split_binary(Buffer, 20),

    {ok, TorrentFile} = torrentdb:get_torrent_file(InfoHash),
    {ok, {IP, Port}} = inet:peername(Sock),
    io:format("Got connection from ~p:~p for ~s~n", [IP, Port, TorrentFile]),

    gen_tcp:send(Sock, InfoHash),
    process_input(State#state{step = peer_id,
			      buffer = Rest,
			      info_hash = InfoHash});

process_input(#state{mode = server,
		     step = info_hash} = State) ->
    State;

%% Waiting for peer_id

process_input(#state{mode = server,
		     step = peer_id,
		     buffer = Buffer,
		     sock = Sock,
		     info_hash = InfoHash} = State)
  when size(Buffer) >= 20 ->
    {PeerId, Rest} = split_binary(Buffer, 20),
    {ok, {IP, Port}} = inet:peername(Sock),
    peerdb:register_peer(InfoHash,
			 PeerId,
			 IP, Port),

    {ok, MyPeerId} = torrentdb:peer_id(),
    gen_tcp:send(Sock, MyPeerId),
    send_bitfield(State),
    process_input(State#state{step = run,
			      buffer = Rest});

process_input(#state{mode = server,
		     step = peer_id} = State) ->
    State;

%%%===================================================================
%%% Client mode
%%%===================================================================

%% Waiting for handshake

process_input(#state{mode = client,
		     step = handshake,
		     buffer = <<19,
				"BitTorrent protocol",
				Buffer/binary>>
		    } = State) ->
    process_input(State#state{step = extensions,
			      buffer = Buffer});

process_input(#state{mode = client,
		     step = handshake,
		     buffer = Buffer} = State)
  when size(Buffer) < 20 ->
    State;

%% Waiting for extensions

process_input(#state{mode = client,
		     step = extensions,
		     buffer = Buffer} = State)
  when size(Buffer) >= 8 ->
    {_, Rest} = split_binary(Buffer, 8),
    process_input(State#state{step = info_hash,
			      buffer = Rest});

process_input(#state{mode = client,
		     step = extensions} = State) ->
    State;

%% Waiting for info_hash

process_input(#state{mode = client,
		     step = info_hash,
		     info_hash = InfoHash,
		     buffer = Buffer} = State)
  when size(Buffer) >= 20 ->
    {PeerInfoHash, Rest} = split_binary(Buffer, 20),
    if
	InfoHash =/= PeerInfoHash ->
	    exit(info_hashes_differ);
	true -> ok
    end,
    process_input(State#state{step = peer_id,
			      buffer = Rest,
			      info_hash = InfoHash});

process_input(#state{mode = client,
		     step = info_hash} = State) ->
    State;

%% Waiting for peer_id

process_input(#state{mode = client,
		     step = peer_id,
		     sock = Sock,
		     buffer = Buffer,
		     info_hash = InfoHash} = State)
  when size(Buffer) >= 20 ->
    {PeerId, Rest} = split_binary(Buffer, 20),
    {ok, {IP, Port}} = inet:peername(Sock),
    peerdb:register_peer(InfoHash,
			 PeerId,
			 IP, Port),

    process_input(State#state{step = run,
			      buffer = Rest});

process_input(#state{mode = client,
		     step = peer_id} = State) ->
    State;

%%%===================================================================
%%% Server & client mode
%%%===================================================================

%% Waiting for any message

process_input(#state{step = run,
		     buffer = <<Len:32/big, Buffer/binary>>
		    } = State1)
  when size(Buffer) >= Len ->
    %% Read enough
    {Message, Rest} = split_binary(Buffer, Len),
    State2 = State1#state{buffer = Rest},
    State3 = process_message(Message, State2),
    process_input(State3);

process_input(#state{step = run} = State) ->
    %% Read not enough or processed all so far
    State.

%% Handling messages

process_message(<<>>, State) ->
    %% Keep-alive
    State;

process_message(<<?INTERESTED>>, State) ->
    State#state{interested = true};

process_message(<<?NOT_INTERESTED>>, State) ->
    State#state{interested = false};

process_message(<<?REQUEST, Piece:32/big,
		  Offset:32/big, Length:32/big>>,
		#state{queue = Queue} = State) ->
    State#state{queue = Queue ++ [#queued{piece = Piece,
					  offset = Offset,
					  length = Length}]};

process_message(<<?CANCEL, Piece:32/big,
		  Offset:32/big, Length:32/big>>,
		#state{queue = Queue} = State) ->
    State#state{queue = lists:filter(fun(Queued) ->
					     Piece == Queued#queued.piece andalso
						 Offset == Queued#queued.offset andalso
						 Length == Queued#queued.length
				     end, Queue)};

process_message(_Msg, State) ->
    State.

send_message(Sock, Msg) ->
    Len = size(Msg),
    ok = gen_tcp:send(Sock,
		      <<Len:32/big,
			Msg/binary>>).

send_handshake(#state{sock = Sock}) ->
    ok = gen_tcp:send(Sock, <<19, "BitTorrent protocol">>).

send_extensions(#state{sock = Sock}) ->
    ok = gen_tcp:send(Sock, <<0, 0, 0, 0, 0, 0, 0, 0>>).


send_bitfield(#state{sock = Sock,
		     info_hash = InfoHash}) ->
    PieceCount = piecesdb:piece_count(InfoHash),
    Msg = list_to_binary(
	    [?BITFIELD |
	     build_bitfield(PieceCount)]),
    send_message(Sock, Msg).

build_bitfield(N) when N >= 8 ->
    [16#FF | build_bitfield(N - 8)];
build_bitfield(N) when N < 8 ->
    [lists:foldl(fun(I, R) ->
			 (R bsl 1) bor
			     (if N >= I -> 1;
				 true -> 0
			      end)
		 end, 0, lists:seq(1, 8))];
build_bitfield(0) ->
    %% case for byte alignedness
    [].

send_queued(#queued{piece = Piece,
		    offset = Offset,
		    length = Length},
	    #state{sock = Sock,
		   info_hash = InfoHash} = State) ->
    FileRanges = piecesdb:map_files(InfoHash, Piece, Offset, Length),
    MessageLength = 1 + 4 + 4 + Length,
    gen_tcp:send(Sock, <<MessageLength:32/big, ?PIECE,
			 Piece:32/big, Offset:32/big>>),
    send_piece(FileRanges, State).

send_piece(FileRanges, #state{sock = Sock,
			      info_hash = InfoHash}) ->
    lists:foreach(
      fun({Path, Offset, Length}) ->
	      backend:fold_file(Path, Offset, Length,
				fun(Data, _) ->
					gen_tcp:send(Sock, Data),
					torrentdb:inc_uploaded(InfoHash, size(Data))
				end, nil)
      end, FileRanges).
