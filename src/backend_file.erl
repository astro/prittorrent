-module(backend_file).

-export([fold_file/5]).

-behaviour(backend).

fold_file(Path, Offset, Length, Fold, AccIn) ->
    {ok, File} = file:open(Path, [read, binary]),
    {ok, Offset} = file:position(File, Offset),
    AccOut = fold_file1(File, Length, Fold, AccIn),
    file:close(File),
    AccOut.

-define(CHUNK_SIZE, 8192).

fold_file1(_, 0, _, AccOut) ->
    AccOut;
fold_file1(File, Length, Fold, AccIn) ->
    Length1 = if not is_number(Length) -> ?CHUNK_SIZE;
		 Length >= ?CHUNK_SIZE -> ?CHUNK_SIZE;
		 true -> Length
	      end,
    case file:read(File, Length1) of
	eof when not is_number(Length) ->
	    AccIn;
	{ok, Data} ->
	    AccOut = Fold(Data, AccIn),
	    Length2 = if is_number(Length) -> Length - Length1;
			 true -> Length
		      end,
	    fold_file1(File, Length2, Fold, AccOut);
	eof ->
	    exit(truncated_seed_file)
    end.


