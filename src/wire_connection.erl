-module(wire_connection).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {sock, buffer = <<>>,
		mode = handshake, info_hash,
		choked = true, interested = false}).

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
start_link(Sock) ->
    io:format("wire_connection:start_link(~p)~n", [Sock]),
    {ok, Pid} = gen_server:start_link(?MODULE, [Sock], []),
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
init([Sock]) ->
    link(Sock),
    {ok, #state{sock = Sock}}.

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
    io:format("wire_connection:handle_info({tcp, ~p, ~p})~n", [Sock, Data]),
    State2 = State1#state{buffer = list_to_binary([Buffer, Data])},
    State3 = process_input(State2),

    case {State3#state.mode,
	  State3#state.choked} of
	{run, true} ->
	    send_message(Sock, <<?UNCHOKE>>),
	    {noreply, State3#state{choked = false}};
	_ ->
	    {noreply, State3}
    end;
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
terminate(_Reason, _State) ->
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
%%% Internal functions
%%%===================================================================

%% Waiting for handshake

process_input(#state{mode = handshake,
		     buffer = <<19,
				"BitTorrent protocol",
				Buffer/binary>>,
		     sock = Sock
		    } = State) ->
    gen_tcp:send(Sock, <<19, "BitTorrent protocol">>),
    process_input(State#state{mode = extensions,
			      buffer = Buffer});

process_input(#state{mode = handshake,
		     buffer = Buffer} = State)
  when size(Buffer) < 20 ->
    State;

%% Waiting for extensions

process_input(#state{mode = extensions,
		     buffer = Buffer,
		     sock = Sock} = State)
  when size(Buffer) >= 8 ->
    {_, Rest} = split_binary(Buffer, 8),
    gen_tcp:send(Sock, <<0, 0, 0, 0, 0, 0, 0, 0>>),
    process_input(State#state{mode = info_hash,
			      buffer = Rest});

process_input(#state{mode = extensions} = State) ->
    State;

%% Waiting for info_hash

process_input(#state{mode = info_hash,
		     buffer = Buffer,
		     sock = Sock} = State)
  when size(Buffer) >= 20 ->
    {InfoHash, Rest} = split_binary(Buffer, 20),
    ok = torrentdb:register_peer(InfoHash),
    gen_tcp:send(Sock, InfoHash),
    process_input(State#state{mode = peer_id,
			      buffer = Rest,
			      info_hash = InfoHash});

process_input(#state{mode = info_hash} = State) ->
    State;

%% Waiting for peer_id

process_input(#state{mode = peer_id,
		     buffer = Buffer,
		     sock = Sock} = State)
  when size(Buffer) >= 20 ->
    {_PeerId, Rest} = split_binary(Buffer, 20),
    {ok, MyPeerId} = torrentdb:peer_id(),
    gen_tcp:send(Sock, MyPeerId),
    send_bitfield(State),
    process_input(State#state{mode = run,
			      buffer = Rest});

process_input(#state{mode = peer_id} = State) ->
    State;

%% Waiting for any message

process_input(#state{mode = run,
		     buffer = <<Len:32/big, Buffer/binary>>
		    } = State1)
  when size(Buffer) >= Len ->
    %% Read enough
    {Message, Rest} = split_binary(Buffer, Len),
    State2 = State1#state{buffer = Rest},
    io:format("process_message(~p)~n", [Message]),
    State3 = process_message(Message, State2),
    process_input(State3);

process_input(#state{mode = run} = State) ->
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
		#state{sock = Sock,
		       info_hash = InfoHash} = State) ->
    FileRanges = piecesdb:map_files(InfoHash, Piece, Offset, Length),
    MessageLength = 1 + 4 + 4 + Length,
    gen_tcp:send(Sock, <<MessageLength:32/big, ?PIECE,
			 Piece:32/big, Offset:32/big>>),
    send_piece(Sock, FileRanges),
    State;

process_message(Msg, State) ->
    State.

send_message(Sock, Msg) ->
    Len = size(Msg),
    ok = gen_tcp:send(Sock,
		      <<Len:32/big,
			Msg/binary>>).

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

send_piece(Sock, FileRanges) ->
    lists:foreach(
      fun({Path, Offset, Length}) ->
	      io:format("open(~p)~n", [Path]),
	      {ok, F} = file:open(Path, [read, binary]),
	      io:format("position(~p, ~p)~n", [F, Offset]),
	      {ok, Offset} = file:position(F, Offset),
	      send_piece1(Sock, F, Length),
	      file:close(F)
      end, FileRanges).

-define(CHUNK_SIZE, 8192).
send_piece1(_, _, 0) ->
    ok;
send_piece1(Sock, F, Length) ->
    Length1 = if Length >= ?CHUNK_SIZE -> ?CHUNK_SIZE;
		 true -> Length
	      end,
    io:format("read(~p, ~p)~n", [F, Length1]),
    {ok, Data} = file:read(F, Length1),
    io:format("read ~B bytes~n", [size(Data)]),
    gen_tcp:send(Sock, Data),
    send_piece1(Sock, F, Length - Length1).
