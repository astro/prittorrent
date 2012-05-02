-module(seeder_wire_protocol).
-behaviour(cowboy_protocol).

%% API.
-export([start_link/4]).


-record(state, {socket,
		data_length,
		piece_length,
		has,
		request_queue = queue:new(),
		timer_armed = false,
		storage
	       }).
-record(request, {offset,
		  length,
		  cb}).

%% For code reload:
-export([loop/1]).

%% Wait for subsequent chunks of a piece to be requested so they can
%% be merged into bigger HTTP requests.
-define(PIECE_DELAY, 50).

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
		       R ->
			   io:format("Wire terminated with ~p~n", [R]),
			   ok
		   end,

		   catch gen_tcp:close(Socket)

		   %% TODO: cleanup stats
	   end),
    {ok, Pid}.

			    


%%
%% Initialization
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

%%
%% Main loop
%%

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
	    tcp_closed;
	request_pieces ->
	    State2 = State1#state{timer_armed = false},
	    {ok, State3} = request_pieces(State2),
	    State4 = may_arm_timer(State3),
	    ?MODULE:loop(State4)
    end.

may_arm_timer(#state{timer_armed = false,
		     request_queue = RequestQueue} = State) ->
    case queue:is_empty(RequestQueue) of
	false ->
	    timer:send_after(?PIECE_DELAY, request_pieces),
	    State#state{timer_armed = true};
	true ->
	    State
    end;
may_arm_timer(State) ->
    State.


%%
%% Incoming message
%%

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
	       #state{piece_length = PieceLength,
		      request_queue = RequestQueue1,
		      socket = Socket} = State) ->
    %% Add
    RequestQueue2 =
	queue:in(
	  #request{offset = Piece * PieceLength + Offset,
		   length = Length,
		   cb = fun(Data) ->
				send_piece(Piece, Offset,
					   Socket, Data)
			end},
	  RequestQueue1),

    {ok, may_arm_timer(State#state{request_queue = RequestQueue2})};

handle_message(<<?CANCEL, Piece:32, Offset:32, Length:32/big>>,
	       #state{piece_length = PieceLength,
		      request_queue = RequestQueue1} = State) ->
    Offset1 = Piece * PieceLength + Offset,
    %% Remove from queue (though we could be requesting them already)
    RequestQueue2 =
	queue:filter(fun(#request{offset = Offset2,
				  length = Length1})
			   when Offset1 == Offset2, Length == Length1 ->
			     false;
			(_) ->
			     true
		     end, RequestQueue1),
    {ok, State#state{request_queue = RequestQueue2}};

handle_message(Data, State) ->
    io:format("Unhandled wire message: ~p~n", [Data]),
    {ok, State}.

check_bitfield(#state{has = Has,
		      data_length = Length,
		      piece_length = PieceLength} = State) ->
    %% TODO: optimize
    Pieces = trunc((Length - 1) / PieceLength) + 1,
    case is_bitfield_seeder(Has, Pieces) of
	true ->
	    %% Nothing left to seed
	    {close, State};
	false ->
	    {ok, State}
    end.

is_bitfield_seeder(Bitfield, Pieces) ->
    is_bitfield_seeder(Bitfield, 0, Pieces).

is_bitfield_seeder(_, Pieces, Pieces) ->
    %% All pieces present
    true;
is_bitfield_seeder(Bitfield, Piece, Pieces) ->
    Remain = 8 * size(Bitfield) - Piece - 1,
    <<_:Piece, Bit:1, _:Remain>> = Bitfield,
    case Bit of
	1 ->
	    %% Peer has piece, look further
	    is_bitfield_seeder(Bitfield, Piece + 1, Pieces);
	0 ->
	    %% Peer doesn't have piece, terminate
	    false
    end.

%%
%% Data Transfer
%%


request_pieces(#state{request_queue = RequestQueue1,
		      storage = Storage} = State) ->
    case queue:out(RequestQueue1) of
	{{value, #request{offset = Offset} = Request}, RequestQueue2} ->
	    {SubsequentRequests, RequestQueue3} =
		collect_contiguous_requests(Request#request.offset + Request#request.length,
					    RequestQueue2),
	    Requests = [Request | SubsequentRequests],
	    %% TODO: could be returned by collect_contiguous_requests/2 for
	    %% performance reasons
	    Length = lists:foldl(fun(#request{length = Length1}, Length) ->
					 Length + Length1
				 end, 0, Requests),
	    io:format("Processing ~B requests with ~B bytes~n", [length(Requests), Length]),

	    %% Transfer request by request
	    {RemainRequests, _} =
		storage:fold(Storage,
			     Offset, Length,
			     fun serve_requests/2, {Requests, <<>>}),

	    %% Retry later
	    RequestQueue4 =
		lists:foldl(fun(RemainRequest, RequestQueue) ->
				    queue:in(RemainRequest, RequestQueue)
			    end, RequestQueue3, RemainRequests),

	    {ok, may_arm_timer(State#state{request_queue = RequestQueue4})};
	{empty, RequestQueue1} ->
	    {ok, State}
    end.

collect_contiguous_requests(Offset, RequestQueue1) ->
    case queue:peek(RequestQueue1) of
	{value, #request{offset = Offset1,
			 length = Length1}}
	  when Offset == Offset1 ->
	    {{value, Request}, RequestQueue2} =
		queue:out(RequestQueue1),
	    {Requests, RequestQueue3} =
		collect_contiguous_requests(Offset + Length1, RequestQueue2),
	    {[Request | Requests], RequestQueue3};
	_ ->
	    {[], RequestQueue1}
    end.

%% Used with storage:fold/5
serve_requests({[], Data1}, Data2) ->
    {[], <<Data1/binary, Data2/binary>>};
serve_requests({[#request{length = Length} | Requests], Data1}, Data)
  when Length =< 0 ->
    serve_requests({Requests, Data1}, Data);
serve_requests({[#request{length = ReqLength,
			  cb = ReqCb} = Req | Requests], Data1}, Data) ->
    Data2 = <<Data1/binary, Data/binary>>,
    if
	ReqLength =< size(Data2) ->
	    %% Next request satisfied
	    {Data3, Data4} = split_binary(Data2, ReqLength),
	    ReqCb(Data3),
	    serve_requests({Requests, <<>>}, Data4);
	true ->
	    %% Not enough data
	    {[Req | Requests], Data2}
    end.


send_piece(Piece, Offset, Socket, Data) ->
    %% We switch to manual packetization for streaming
    inet:setopts(Socket, [{packet, raw},
			  {active, false}]),

    %% Length prefixed header
    PieceHeader = <<?PIECE, Piece:32/big, Offset:32/big>>,
    case gen_tcp:send(Socket,
		      <<(size(PieceHeader) + size(Data)):32/big,
			PieceHeader/binary, Data/binary>>) of
	ok -> ok;
	{error, closed} -> throw(tcp_closed)
    end,

    %% Continue receiving & sending in len-prefixed packets
    inet:setopts(Socket, ?PACKET_OPTS).
