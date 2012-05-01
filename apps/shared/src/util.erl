-module(util).

-export([get_now/0, get_now_us/0, measure/2,
	 pmap/2]).

get_now() ->
    {MS, S, SS} = erlang:now(),
    MS * 1000000 + S + SS / 1000000.
    

get_now_us() ->
    {MS, S, SS} = erlang:now(),
    (MS * 1000000 + S) * 1000000 + SS.


measure(Label, F)
  when is_list(Label) ->
    T1 = get_now_us(),
    R = F(),
    T2 = get_now_us(),
    io:format("[~.1fms] ~s~n", [(T2 - T1) / 1000, Label]),
    R;
measure(Label, F) ->
    measure(io_lib:format("~p", [Label]), F).

pmap(F, L) ->
    I = self(),
    Pids =
	[spawn(fun() ->
		       I ! {ok, self(), F(E)}
	       end) || E <- L],
    [receive
	 {ok, Pid, E2} ->
	     E2
     end || Pid <- Pids].
