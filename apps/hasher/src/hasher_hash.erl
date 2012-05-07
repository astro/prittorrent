-module(hasher_hash).

-export([make_torrent/1]).

-define(DEFAULT_PIECE_LENGTH, trunc(math:pow(2, 20))).


make_torrent(URL) when is_binary(URL) ->
    make_torrent([URL]);

make_torrent(URLs) ->
    {ok, Size, Pieces} = hash_torrent(URLs),
    io:format("hash_torrent ~p - Size: ~p, Pieces: ~p~n", [URLs, Size, length(Pieces)]),
    Name = extract_name_from_urls(URLs),
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
	 {<<"url-list">>, URLs},
	 {<<"info">>, InfoValue}
	],
    InfoHash = benc:hash(InfoValue),
    {ok, InfoHash, Name, Size, benc:to_binary(Torrent)}.

extract_name_from_urls(URLs) when is_list(URLs) ->
    Name = lists:foldl(
	     fun(URL, undefined) ->
		     case extract_name_from_url(URL) of
			 Name
			   when is_binary(Name),
				size(Name) > 0 ->
			     Name;
			 _ ->
			     undefined
		     end;
		(_, R) ->
		     R
	     end, undefined, URLs),
    case Name of
	undefined ->
	    exit(no_suitable_name_found);
	_ ->
	    Name
    end.

extract_name_from_url(URL) ->
    {Parts, _, _} =
	cowboy_dispatcher:split_path(URL, fun cowboy_http:urldecode/1),
    io:format("Parts: ~p~n", [Parts]),
    lists:foldl(fun(Part, R) ->
			if
			    is_binary(Part),
			    size(Part) > 0 ->
				Part;
			    true ->
				R
			end
		end, undefined, Parts).


-spec(hash_torrent/1 :: ([binary()]) -> [binary()]).

hash_torrent(URL) when is_binary(URL) ->
    hash_torrent([URL]);

hash_torrent(URLs) ->
    Storage = storage:make(URLs),
    Size = storage:size(Storage),
    Pieces =
	map_pieces(
	  Size, fun(Offset, Length) ->
			io:format("Hash ~s: ~B%~n", [hd(URLs), trunc(100 * (Offset + Length) / Size)]),
			hash_piece(Storage, Offset, Length)
		end),
    {ok, Size, Pieces}.

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
			     {crypto:sha_update(Data, Sha),
			      ActualLength + size(Data)}
		     end, {crypto:sha_init(), 0}),
    Digest = crypto:sha_final(Sha),
    if
	ActualLength == Length ->
	    Digest;
	true ->
	    exit({expected_but_received, Length, ActualLength})
    end.
