-module(backend).

-export([behaviour_info/1, read_file/1, fold_file/5]).

behaviour_info(callbacks) ->
    [{fold_file, 5}];
behaviour_info(_) ->
    undefined.

read_file(Path) ->
    list_to_binary(
      lists:reverse(
	fold_file(Path, 0, nil,
		  fun(Chunk, R) ->
			  [Chunk | R]
		  end, []))).

fold_file(Path, Offset, Length, F, AccIn) ->
    BE = backend(Path),
    BE:fold_file(Path, Offset, Length, F, AccIn).

backend([$h, $t, $t, $p, $:, $/, $/ | _]) ->
    backend_http;
backend(_) ->
    backend_file.
