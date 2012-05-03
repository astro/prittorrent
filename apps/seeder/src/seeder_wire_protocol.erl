-module(seeder_wire_protocol).

-behaviour(cowboy_protocol).
-behaviour(gen_server).

%% API
-export([start_link/4]).

-define(HANDSHAKE_TIMEOUT, 10000). 
%% Wait for subsequent chunks of a piece to be requested so they can
%% be merged into bigger HTTP requests.
-define(PIECE_DELAY, 50).

%% For testing:
-export([make_bitfield/2, make_empty_bitfield/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {socket,
		info_hash,
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

%%%===================================================================
%%% API
%%%===================================================================
%% Don't block when invoking start_link/4, it waits for peer handshake
%% to finish.
-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(_ListenerPid, Socket, _Transport, Opts) ->
    gen_server:start_link(?MODULE, [Socket, Opts],
			  [{timeout, ?HANDSHAKE_TIMEOUT}]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Socket, _Opts]) ->
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
    inet:setopts(Socket, [{active, once} | ?PACKET_OPTS]),

    %% Initial Bitfield
    send_bitfield(Socket, Length, PieceLength),
    Has = make_empty_bitfield(Length, PieceLength),

    {ok, #state{socket = Socket,
		info_hash = InfoHash,
		data_length = Length,
		piece_length = PieceLength,
		has = Has,
		storage = Storage}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, 
	    #state{socket = Socket} = State1) ->
    case handle_message(Data, State1) of
	{ok, State2} ->
	    inet:setopts(Socket, [{active, once}]),
	    {noreply, State2};
	{close, State2} ->
	    {stop, normal, State2}
    end;

handle_info({tcp_closed, Socket},
	    #state{socket = Socket} = State) ->
    {stop, normal, State};

handle_info(request_pieces,
	    #state{} = State1) ->
    State2 = State1#state{timer_armed = false},
    case (catch request_pieces(State2)) of
	{ok, State3} ->
	    State4 = may_arm_timer(State3),
	    {noreply, State4};
	tcp_closed ->
	    {stop, normal, State2};
	{'EXIT', Reason} ->
	    io:format("Wire cannot request_pieces: ~p~n", [Reason]),
	    {stop, Reason, State2}
    end;

handle_info(_Info, State) ->
    io:format("Unhandled wire info in ~p: ~p~n", [self(), _Info]),
    {noreply, State}.


terminate(_Reason, #state{socket = Socket}) ->
    catch gen_tcp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
		      storage = Storage,
		      info_hash = InfoHash} = State) ->
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
	    RemainRequests =
		case (catch storage:fold(Storage,
					 Offset, Length,
					 fun collect_data/2, {Requests, <<>>})) of
		    {'EXIT', Reason} ->
			io:format("storage request failed: ~p~n", [Reason]),
			%% Throwing an error means nothing has been processed
			Requests;
		    tcp_closed ->
			%% Re-throw to terminate gen_server. The HTTP
			%% client hopefully doesn't receive too much
			%% excess data...
			throw(tcp_closed);
		    {RemainRequests1, _} ->
			RemainRequests1
		end,

	    %% Calculate stats
	    NotTransferred =
		lists:foldl(fun(#request{length = Length1}, NotTransferred) ->
				    NotTransferred + Length1
			    end, 0, RemainRequests),
	    model_stats:add_counter(up_seeder, InfoHash, Length - NotTransferred),
	    model_stats:add_counter(storage_fail, InfoHash, NotTransferred),

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
collect_data({[], Data}, <<>>) ->
    {[], Data};

collect_data({Requests, Data}, <<>>) ->
    %% No input left, look for next hunk to serve
    serve_requests(Requests, Data);
     
collect_data({Requests, <<>>}, Data) ->
    %% Fast path
    collect_data({Requests, Data}, <<>>);

collect_data({Requests, Data1}, Data2) ->
    %% Merge input
    collect_data({Requests, <<Data1/binary, Data2/binary>>}, <<>>).

serve_requests([], Data) ->
    {[], Data};

serve_requests([#request{length = ReqLength,
			 cb = ReqCb} | Requests2] = Requests1, Data1) ->
    if
	size(Data1) >= ReqLength ->
	    %% Next request satisfied
	    {Piece, Data2} = split_binary(Data1, ReqLength),
	    ReqCb(Piece),
	    serve_requests(Requests2, Data2);
	true ->
	    %% Not enough data
	    {Requests1, Data1}
    end.


send_piece(Piece, Offset, Socket, Data) ->
    %% We switch to manual packetization for streaming
    inet:setopts(Socket, [{active, false},
			  {packet, raw}]),

    %% Length prefixed header
    PieceHeader = <<?PIECE, Piece:32/big, Offset:32/big>>,
    case gen_tcp:send(Socket,
		      <<(size(PieceHeader) + size(Data)):32/big,
			PieceHeader/binary, Data/binary>>) of
	ok -> ok;
	{error, closed} -> throw(tcp_closed)
    end,

    %% Continue receiving & sending in len-prefixed packets
    inet:setopts(Socket, [{active, once} | ?PACKET_OPTS]).
