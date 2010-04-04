-module(seedlist).

-export([start_link/1, loop/2, load_seedlist/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kernel/include/file.hrl").


start_link(Filename) ->
    Pid = spawn_link(
	    fun() ->
		    loop(Filename, nil)
	    end),
    {ok, Pid}.

-define(RELOAD_INTERVAL, 10).
loop(Filename, ModTime1) ->
    ModTime2 = get_mtime(Filename),
    if
	ModTime2 =/= ModTime1 ->
	    io:format("~s has been modified~n", [Filename]),
	    case (catch load_seedlist(Filename)) of
		{'EXIT', Reason} ->
		    io:format("Cannot load seedlist ~s: ~p~n",
			      [Filename, Reason]);
		SeedList ->
		    torrentdb:apply_seedlist(SeedList)
	    end;
	true ->
	    not_modified
    end,

    receive
    after ?RELOAD_INTERVAL * 1000 ->
	    ?MODULE:loop(Filename, ModTime2)
    end.

load_seedlist(Filename) ->
    {SeedsEl, _} = xmerl_scan:file(Filename),
    #xmlElement{content = SeedsChildren} = SeedsEl,
    lists:filter(
      fun({_, _}) -> true;
	 (_) -> false
      end,
      lists:map(fun(#xmlElement{name = seed} = SeedEl) ->
			try {get_el_text(torrent, SeedEl),
			     get_el_text(dir, SeedEl)}
			catch _ ->
				nil
			end;
		   (_) ->
			nil
		end, SeedsChildren)).

get_el_text(Name, #xmlElement{content = Children}) ->
    get_el_text1(Name, Children).
get_el_text1(Name, []) ->
    exit({not_found, Name});
get_el_text1(Name, [#xmlElement{name = Name,
				content = Children} | _]) ->
    lists:append(
      lists:map(
	fun(#xmlText{value = Text}) ->
		Text;
	   (_) ->
		[]
	end, Children));
get_el_text1(Name, [_ | Els]) ->
    get_el_text1(Name, Els).


get_mtime(Filename) ->
    {ok, #file_info{mtime = ModTime}} =
	file:read_file_info(Filename),
    ModTime.
