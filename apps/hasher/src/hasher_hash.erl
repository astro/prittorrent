-module(hasher_hash).

-export([update_torrent/2, make_torrent/1]).

-define(DEFAULT_PIECE_LENGTH, trunc(math:pow(2, 20))).

update_torrent(URL, OldTorrent) ->
    OldInfo = benc:parse(OldTorrent),
    OldURLList = lists:keysearch(<<"url-list">>, 1, OldInfo),
    case lists:member(URL, OldURLList) of
	true ->
	    io:format("update_torrent ~s didn't add a new URL~n", [URL]),
	    OldTorrent;
	false ->
	    io:format("update_torrent: new url ~s~n", [URL]),
	    NewInfo = lists:keystore(<<"url-list">>, 1, OldInfo, [URL | OldURLList]),
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
    {Parts, _, _} =
	cowboy_dispatcher:split_path(URL, fun cowboy_http:urldecode/1),
    lists:foldl(fun(Part, R) ->
			if
			    is_binary(Part),
			    size(Part) > 0 ->
				Part;
			    true ->
				R
			end
		end, undefined, Parts).


-spec(hash_torrent/1 :: (binary()) -> {ok, integer(), binary(), binary(), binary()}).

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
    Digest = crypto:crypto_final(Sha),
    if
	ActualLength == Length ->
	    Digest;
	true ->
	    exit({expected_but_received, Length, ActualLength})
    end.
