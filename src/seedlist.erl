-module(seedlist).

-export([start_link/0, loop/2, load_seedlist/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kernel/include/file.hrl").


start_link() ->
    {ok, Filename} = application:get_env(servtorrent, seedlist),
    Pid = spawn_link(
	    fun() ->
		    loop(Filename, [])
	    end),
    {ok, Pid}.

-define(RELOAD_INTERVAL, 10).
loop(Filename, ModTimes) ->
    Modified = case ModTimes of
		   [] -> true;
		   _ ->
		       lists:foldl(
			 fun({Path, Time}, Modified) ->
				 CurrentTime = get_mtime(Path),
				 if
				     CurrentTime > Time ->
					 logger:log(control, info,
						    "~s has been modified, mtime=~p", [Path, CurrentTime]),
					 true;
				     true ->
					 Modified
				 end
			 end, false, ModTimes)
	       end,
    if
	Modified ->
	    case (catch load_seedlist(Filename)) of
		{'EXIT', Reason} ->
		    logger:log(control, fatal,
			       "Cannot load seedlist ~s: ~p~n",
			       [Filename, Reason]);
		{ok, SeedList, NewModTimes} ->
		    logger:log(control, info,
			       "Loaded ~B seeds by ~s",
			       [length(SeedList), Filename]),
		    torrentdb:apply_seedlist(SeedList),
		    ?MODULE:loop(Filename, NewModTimes)
	    end;
	true ->
	    receive
	    after ?RELOAD_INTERVAL * 1000 ->
		    ?MODULE:loop(Filename, ModTimes)
	    end
    end.


load_seedlist(Filename) ->
    ModTime = {Filename, get_mtime(Filename)},
    BasePath = filename:dirname(Filename),
    File = backend:read_file(Filename),
    {SeedsEl, _} = xmerl_scan:string(binary_to_list(File)),
    #xmlElement{content = SeedsChildren} = SeedsEl,
    lists:foldl(
      fun(#xmlElement{name = seed} = SeedEl,
	  {ok, SeedList, ModTimes} = R) ->
	      try
		  TorrentFile = filename:join([BasePath, get_el_text(torrent, SeedEl)]),
		  DataDir = filename:join([BasePath, get_el_text(data, SeedEl)]),
		  {ok, [{TorrentFile, DataDir} | SeedList], ModTimes}
	      catch _ ->
		      R
	      end;
	 (#xmlElement{name = include} = IncludeEl,
	  {ok, SeedList, ModTimes}) ->
	      Filename1 = get_el_text(IncludeEl),
	      %% Recurse:
	      {ok, SeedList1, ModTimes1} =
		  load_seedlist(
		    filename:join([BasePath, Filename1])),
	      {ok, SeedList1 ++ SeedList, ModTimes1 ++ ModTimes};
	 (_, Result) ->
	      Result
      end, {ok, [], [ModTime]}, SeedsChildren).


get_el_text(#xmlElement{content = Children}) ->
    lists:append(
      lists:map(
	fun(#xmlText{value = Text}) ->
		Text;
	   (_) ->
		[]
	end, Children)).

get_el_text(Name, #xmlElement{content = Children}) ->
    get_el_text1(Name, Children).
get_el_text1(Name, []) ->
    exit({not_found, Name});
get_el_text1(Name, [#xmlElement{name = Name} = El | _]) ->
    get_el_text(El);
get_el_text1(Name, [_ | Els]) ->
    get_el_text1(Name, Els).


get_mtime(Filename) ->
    {ok, #file_info{mtime = ModTime}} =
	file:read_file_info(Filename),
    ModTime.
