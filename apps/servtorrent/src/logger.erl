-module(logger).

-behaviour(gen_server).

%% API
-export([start_link/0, log/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {opts, funs}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

log(Target, Level, Fmt, Param) ->
    gen_server:cast(?SERVER, {log, Target, Level, now(), Fmt, Param}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, Opts} = application:get_env(servtorrent, logging),
    Destinations = uniq_list(
		     lists:append(
		       [[Dest
			 || {_, Dest} <- Filters]
			|| {_, Filters} <- Opts])),
    Funs = [{Dest, make_filter(Dest)}
	    || Dest <- Destinations],
    {ok, #state{opts = Opts,
		funs = Funs}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({log, Target, Level, Time, Fmt, Param},
	    #state{opts = Opts,
		   funs = Funs} = State) ->
    case lists:keysearch(Target, 1, Opts) of
	{value, {_, Filters}} ->
	    lists:foreach(
	      fun({FLevel, FDest}) ->
		      case level_le(FLevel, Level) of
			  true ->
			      {value, {_, FFun}} =
				  lists:keysearch(FDest, 1, Funs),
			      %%io:format("~p(~p, ~p, ~p, ~p)~n", [FFun, Level, Time, Fmt, Param]),
			      FFun(Level, Time, Fmt, Param);
			  false ->
			      ignore
		      end
	      end, Filters);
	false ->
	    ignore
    end,
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

make_filter(sasl) ->
    fun(Level, _Time, Fmt, Param) ->
	    M = case Level of
		    fatal -> error_msg;
		    error -> error_msg;
		    warn -> warning_msg;
		    info -> info_msg;
		    debug -> info_msg
		end,
	    error_logger:M(Fmt, Param)
    end;
make_filter(stdout) ->
    fun(_Level, Time, Fmt, Param) ->
	    io:format("~s " ++ Fmt ++ "~n", [format_time(Time) | Param])
    end;
make_filter(Filename) when is_list(Filename) ->
    {ok, F} = file:open(Filename, [write, append]),
    fun(_Level, Time, Fmt, Param) ->
	    io:format(F, "~s " ++ Fmt ++ "~n", [format_time(Time) | Param])
    end.

level_le(_, debug) -> true;
level_le(debug, _) -> false;
level_le(_, info) -> true;
level_le(info, _) -> false;
level_le(_, warn) -> true;
level_le(warn, _) -> false;
level_le(_, error) -> true;
level_le(error, _) -> false;
level_le(_, fatal) -> true.

format_time({_, _, US} = Time) ->
    {{Y, Mo, D}, {H, M, S}} = calendar:now_to_local_time(Time),
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~4..0B",
		  [Y, Mo, D, H, M, S, US div 1000]).

uniq_list([]) ->
    [];
uniq_list([E | L]) ->
    case lists:member(E, L) of
	true ->
	    uniq_list(L);
	false ->
	    [E | uniq_list(L)]
    end.
