-module(piecesdb).

-export([init/0, add_t/4, rm_t/1, get_dir_t/1, set_dir_t/2, piece_count/1, map_files/4]).

-record(pieces, {info_hash,
		 dir,
		 piece_length,
		 files}).

init() ->
    mnesia:create_table(pieces, [{attributes, record_info(fields, pieces)}]).

add_t(InfoHash, Dir, PieceLength, Files) ->
    mnesia:write(#pieces{info_hash = InfoHash,
			 dir = Dir,
			 piece_length = PieceLength,
			 files = Files}).

rm_t(InfoHash) ->
    mnesia:delete(pieces, InfoHash).

get_dir_t(InfoHash) ->
    case mnesia:read(pieces, InfoHash) of
	[#pieces{dir = Dir}] ->
	    {ok, Dir};
	_ ->
	    not_found
    end.

set_dir_t(InfoHash, Dir) ->
    lists:foreach(fun(Pieces) ->
			  mnesia:write(Pieces#pieces{dir = Dir})
		  end, mnesia:read(pieces, InfoHash)).


piece_count(InfoHash) ->
    {atomic, {PieceLength, Files}} =
	mnesia:transaction(
	  fun() ->
		  [#pieces{piece_length = PieceLength,
			   files = Files}] =
		      mnesia:read(pieces, InfoHash),
		  {PieceLength, Files}
	  end),
    TotalSize =
	lists:foldl(fun({_, Size}, TotalSize) ->
			       TotalSize + Size
		    end, 0, Files),
    case TotalSize rem PieceLength of
	0 -> TotalSize div PieceLength;
	_ -> (TotalSize div PieceLength) + 1
    end.

map_files(InfoHash, Piece, PieceOffset, Length) ->
    {atomic, {PieceLength, Files, Dir}} =
	mnesia:transaction(
	  fun() ->
		  [#pieces{piece_length = PieceLength,
			   files = Files,
			   dir = Dir}] =
		      mnesia:read(pieces, InfoHash),
		  {PieceLength, Files, Dir}
	  end),
    Offset = (Piece * PieceLength) + PieceOffset,
    [{binary_to_list(
	list_to_binary([Dir, $/, Path1])),
      Offset1, Length1}
     || {Path1, Offset1, Length1} <- map_files1(Offset, Length, Files)].

%% Done:
map_files1(_, 0, _) ->
    [];
%% Skip:
map_files1(Offset, Length, [{_, Size} | Files])
  when Offset >= Size ->
    map_files1(Offset - Size, Length, Files);
%% Take:
map_files1(Offset, Length, [{Path, Size} | Files]) ->
    Length1 = if
		  Offset + Length > Size -> Size - Offset;
		  Offset + Length =< Size -> Length
	      end,
    [{Path, Offset, Length1}
     | map_files1(0, Length - Length1,
		  Files)].
