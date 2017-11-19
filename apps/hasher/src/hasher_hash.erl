-module(hasher_hash).

-export([update_torrent/2, make_torrent/1]).

-define(DEFAULT_PIECE_LENGTH, trunc(math:pow(2, 20))).

update_torrent(URL, OldTorrent) ->
    OldInfo = benc:parse(OldTorrent),
    {value, {<<"url-list">>, OldURLList}} = lists:keysearch(<<"url-list">>, 1, OldInfo),
    case lists:member(URL, OldURLList) of
	true ->
	    io:format("update_torrent ~s didn't add a new URL~n", [URL]),
	    OldTorrent;
	false ->
	    io:format("update_torrent: new url ~s~n", [URL]),
	    NewInfo = lists:keystore(<<"url-list">>, 1, OldInfo, {<<"url-list">>, [URL | OldURLList]}),
	    benc:to_binary(NewInfo)
    end.

make_torrent(URL) ->
    {ok, Size, Pieces, ETag, LastModified} = hash_torrent(URL),
    io:format("hash_torrent ~p - Size: ~p, Pieces: ~p~n", [URL, Size, length(Pieces)]),
    Name = extract_name_from_url(URL),
    InfoValue =
	[{<<"name">>, Name},
	 {<<"piece length">>, ?DEFAULT_PIECE_LENGTH},
	 {<<"pieces">>, list_to_binary(Pieces)},
	 {<<"length">>, Size}
	],
    AnnounceURL =
	case application:get_env(hasher, announce_url) of
	    {ok, AnnounceURL1} when is_list(AnnounceURL1) ->
		list_to_binary(AnnounceURL1);
	    {ok, AnnounceURL1} when is_binary(AnnounceURL1) ->
		AnnounceURL1
	end,
    Torrent =
	[{<<"announce">>, AnnounceURL},
	 {<<"url-list">>, [URL]},
	 {<<"info">>, InfoValue}
	],
    InfoHash = benc:hash(InfoValue),
    {ok, InfoHash, Name, Size, benc:to_binary(Torrent), ETag, LastModified}.

extract_name_from_url(URL) ->
    Parts = split_path(URL, []),
    lists:foldl(fun(Part, R) ->
			if
			    is_binary(Part),
			    size(Part) > 0 ->
				Part;
			    true ->
				R
			end
		end, undefined, Parts).

%% split_path has been bluntly copied from cowboy_router. The function
%% was previously exported. :-( Its license applies.
%%
%% Following RFC2396, this function may return path segments containing any
%% character, including <em>/</em> if, and only if, a <em>/</em> was escaped
%% and part of a path segment.
-spec split_path(binary()) -> [binary()] | badrequest.
split_path(<< $/, Path/bits >>) ->
	split_path(Path, []);
split_path(_) ->
	badrequest.

split_path(Path, Acc) ->
	try
		case binary:match(Path, <<"/">>) of
			nomatch when Path =:= <<>> ->
				lists:reverse([cow_qs:urldecode(S) || S <- Acc]);
			nomatch ->
				lists:reverse([cow_qs:urldecode(S) || S <- [Path|Acc]]);
			{Pos, _} ->
				<< Segment:Pos/binary, _:8, Rest/bits >> = Path,
				split_path(Rest, [Segment|Acc])
		end
	catch
		error:badarg ->
			badrequest
	end.


-spec hash_torrent(binary())
      -> {ok, integer(), binary(), binary(), binary()}.
hash_torrent(URL) ->
    case storage:resource_info(URL) of
	{ok, _, Size, ETag, LastModified}
	  when is_integer(Size) ->
	    Storage = {storage, [{URL, Size}]},
	    Pieces =
		map_pieces(
		  Size, fun(Offset, Length) ->
				io:format("Hash ~s: ~B%~n", [URL, trunc(100 * (Offset + Length) / Size)]),
				hash_piece(Storage, Offset, Length)
			end),
	    {ok, Size, Pieces, ETag, LastModified};

	{ok, _, _, _, _} ->
	    exit(no_content_length);

	{error, E} ->
	    {error, E}
    end.

map_pieces(TotalLength, F) ->
    map_pieces(TotalLength, 0, F).

map_pieces(0, _, _) ->
    [];
map_pieces(Remain, N, F) ->
    Offset = N * ?DEFAULT_PIECE_LENGTH,
    Length = min(?DEFAULT_PIECE_LENGTH, Remain),
    [F(Offset, Length) | map_pieces(Remain - Length, N + 1, F)].

hash_piece(Storage, Offset, Length) ->
    {Sha, ActualLength} =
	storage:fold(Storage, Offset, Length,
		     fun({Sha, ActualLength}, Data) ->
			     {crypto:hash_update(Sha, Data),
			      ActualLength + size(Data)}
		     end, {crypto:hash_init(sha), 0}),
    Digest = crypto:hash_final(Sha),
    if
	ActualLength == Length ->
	    Digest;
	true ->
	    exit({expected_but_received, Length, ActualLength})
    end.
