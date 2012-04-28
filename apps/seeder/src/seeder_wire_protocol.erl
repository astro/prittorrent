-module(seeder_wire_protocol).
-behaviour(cowboy_protocol).

%% API.
-export([start_link/4]).


-record(state, {socket,
		data_length,
		piece_length,
		has,
		storage
	       }).
%% For code reload:
-export([loop/1]).

%% For testing:
-export([make_bitfield/2, make_empty_bitfield/2]).


-define(PACKET_OPTS, [{packet, 4},
		      {packet_size, 8192}]).

-define(CHOKE, 0).
-define(UNCHOKE, 1).
-define(INTERESTED, 2).
-define(NOT_INTERESTED, 3).
-define(HAVE, 4).
-define(BITFIELD, 5).
-define(REQUEST, 6).
-define(PIECE, 7).
-define(CANCEL, 8).

%%
%% API
%%

-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(_ListenerPid, Socket, _Transport, Opts) ->
    Pid = spawn_link(
	   fun() ->
		   case (catch init(Socket, Opts)) of
		       {'EXIT', Reason} ->
			   io:format("Wire: ~p exit: ~p~n", [Socket, Reason]);
		       _ ->
			   ok
		   end
		   %% TODO: cleanup stats
	   end),
    {ok, Pid}.

			    


%%
%% FSM
%%

init(Socket, _Opts) ->
    {ok, Peername} = inet:peername(Socket),
    io:format("Peer ~p just connected~n", [Peername]),

    %% Handshake
    {ok, <<19, "BitTorrent protocol">>} = gen_tcp:recv(Socket, 20),
    ok = gen_tcp:send(Socket, <<19, "BitTorrent protocol">>),
    
    %% Extensions
    {ok, _Extensions} = gen_tcp:recv(Socket, 8),
    ok = gen_tcp:send(Socket, <<0, 0, 0, 0, 0, 0, 0, 0>>),
    
    %% Info Hash
    {ok, InfoHash} = gen_tcp:recv(Socket, 20),
    {ok, _FileName, Length, PieceLength, Storage} =
	lookup_torrent(InfoHash),
    ok = gen_tcp:send(Socket, InfoHash),

    %% Peer ID
    {ok, PeerID} = gen_tcp:recv(Socket, 20),
    io:format("Peer ~p~n connected from ~p~n", [PeerID, Peername]),
    ok = gen_tcp:send(Socket, peer_id:generate()),

    %% All following messages will be length-prefixed.
    %% Also, we don't download at all:
    inet:setopts(Socket, ?PACKET_OPTS),

    %% Initial Bitfield
    send_bitfield(Socket, Length, PieceLength),
    Has = make_empty_bitfield(Length, PieceLength),

    ?MODULE:loop(#state{socket = Socket,
			data_length = Length,
			piece_length = PieceLength,
			has = Has,
			storage = Storage}).

lookup_torrent(InfoHash) ->
    case model_torrents:get_torrent(InfoHash) of
	{ok, Name, TorrentFile} ->
	    Torrent = benc:parse(TorrentFile),

	    Info = proplists:get_value(<<"info">>, Torrent),
	    Length =
		proplists:get_value(<<"length">>, Info),
	    PieceLength =
		proplists:get_value(<<"piece length">>, Info),

	    [URL | _] = proplists:get_value(<<"url-list">>, Torrent),
	    Storage = storage:make([URL]),
	    if
		is_binary(Name),
		is_integer(Length),
		is_integer(PieceLength),
		is_tuple(Storage) ->
		    {ok, Name, Length, PieceLength, Storage};
		true ->
		    {error, invalid_torrent}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

send_message(Sock, Msg) ->
    ok = gen_tcp:send(Sock, Msg).


send_bitfield(Sock, Length, PieceLength) ->
    Map = make_bitfield(Length, PieceLength),
    send_message(Sock, <<?BITFIELD, Map/binary>>).

make_bitfield(Length, PieceLength) ->
    list_to_binary(
      lists:map(
    	fun(I) ->
    		if
    		    %% Last piece?
    		    %% Keep spare bits empty
    		    I >= Length - PieceLength * 8 ->
    			lists:foldl(
    			  fun(I1, R) when I1 >= Length ->
    				  R;
    			     (_, R) ->
    				  16#80 bor (R bsr 1)
    			  end, 0, lists:seq(I, I + 8 * PieceLength, PieceLength));
    		    true ->
    			255
    		end
    	end, lists:seq(0, Length - 1, PieceLength * 8))).

make_empty_bitfield(Length, PieceLength) ->
    << <<0>>
       || _ <- lists:seq(0, Length - 1, PieceLength * 8) >>.

loop(#state{socket = Socket} = State1) ->
    ok = inet:setopts(Socket, [{active, once}]),

    receive
	{tcp, Socket, Data} ->
	    case handle_message(Data, State1) of
		{ok, State2} ->
		    ?MODULE:loop(State2);
		{close, _State2} ->
		    close
	    end;
	{tcp_closed, Socket} ->
	    tcp_closed
    end.


handle_message(<<>>, State) ->
    %% Keep-alive
    {ok, State};

handle_message(<<?INTERESTED>>,
	       #state{socket = Socket} = State) ->
    send_message(Socket, <<?UNCHOKE>>),
    {ok, State};

handle_message(<<?HAVE, Piece:32>>, #state{has = Has} = State1) ->
    %% update piece map
    I = trunc(Piece / 8),
    <<Has1:I/binary, Pieces:8, Has2/binary>> = Has,
    State2 =
	State1#state{has = <<Has1/binary,
			     (Pieces bor (16#80 bsr (Piece - I * 8))):8,
			     Has2/binary>>},
    io:format("Have ~B: ~p~n", [Piece, State2#state.has]),

    %% disconnect new seeders:
    check_bitfield(State2);

handle_message(<<?BITFIELD, Bits/binary>>, State) ->
    if
	size(Bits) == size(State#state.has) ->
	    check_bitfield(State#state{has = Bits});
	true ->
	    exit(bitfield_size_mismatch)
    end;

handle_message(<<?REQUEST, Piece:32, Offset:32, Length:32/big>>,
	       #state{socket = Socket,
		      piece_length = PieceLength,
		      storage = Storage} = State) ->
    %%io:format("Request ~p+~p-~p~n", [Piece, Offset, Length]),

    PieceHeader = <<?PIECE, Piece:32/big, Offset:32/big>>,
    %% We switch to manual packetization for streaming
    inet:setopts(Socket, [{packet, raw}]),
    ok =
	gen_tcp:send(Socket,
		     <<(Length + size(PieceHeader)):32/big,
		       PieceHeader/binary>>),
    Transmitted =
	storage:fold(
	  Storage,
	  Piece * PieceLength + Offset, Length,
	  fun(Transmitted, Data) ->
		  ok = gen_tcp:send(Socket, Data),
		  Transmitted + size(Data)
	  end, 0),
    %%io:format("Piece ~p+~p-~p~n", [Piece, Offset, Transmitted]),
    if
	Transmitted < Length ->
	    io:format("Short read from HTTP: ~p~n", [Storage]),
	    %% Make them fail hash check. Perhaps they will retry
	    %% later when the file is up on HTTP completely.
	    ok =
		gen_tcp:send(<< <<0>>
				|| _ <- lists:seq(1, Length - Transmitted)
			     >>);
	Transmitted > Length ->
	    %% Yikes
	    io:format("Excess data from HTTP: ~p~n", [Storage]);
	true ->
	    alright
    end,

    %% Continue receiving & sending in len-prefixed packets
    inet:setopts(Socket, ?PACKET_OPTS),
    {ok, State};

handle_message(Data, State) ->
    io:format("Unhandled wire message: ~p~n", [Data]),
    {ok, State}.

check_bitfield(#state{has = Has,
		      data_length = Length,
		      piece_length = PieceLength} = State) ->
    IsSeeder = lists:foldl(
		 fun(_, false) ->
			 false;
		    (I, true) ->
			 J = 8 * size(Has) - I - 1,
			 case Has of
			     <<_:I, Bit:1, _:J>> ->
				 Bit == 1;
			     _ ->
				 %% Non-fatal:
				 io:format("Cannot match bitfield: ~p~n", [Has])
			 end
		 end, true, lists:seq(0, trunc((Length - 1) / PieceLength))),
    case IsSeeder of
	true ->
	    {close, State};
	false ->
	    {ok, State}
    end.
