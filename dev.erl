-module(dev).

-export([init/0, start_deps/0]).

init() ->
    R1 =
	add_app_paths("deps") ++
	add_app_paths("apps"),
    R2 =
	start_deps(),
    R1 ++ R2.

add_app_paths(BaseDir) ->
    {ok, Filenames} = file:list_dir(BaseDir),
    lists:map(fun("." ++ _) ->
		      ignore;
		 (Filename) ->
		      code:add_patha(BaseDir ++ "/" ++ Filename ++ "/ebin")
	      end, Filenames).

start_deps() ->
    R1 =
	lists:map(
	  fun application:start/1,
	  [crypto, public_key, ssl, inets]),

    {ok, Filenames} = file:list_dir("deps"),
    R2 =
	lists:map(
	  fun(Filename) ->
		  application:start(list_to_atom(Filename))
	  end, Filenames),
    R1 ++ R2.
