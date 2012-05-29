-module(ui_graphs).

-export([render_swarm/3, render_traffic/3, render_downloads/3]).

-record(graph, {start, stop, interval, plots}).
-record(plot, {x1, y1, x2, y2, lines}).
-record(line, {type, data, color, title}).

-define(NS_SVG, "http://www.w3.org/2000/svg").

%%
%% API
%%

render_swarm(InfoHash, Start1, Stop1) ->
    Start2 = calendar:datetime_to_gregorian_seconds(Start1),
    Stop2 = calendar:datetime_to_gregorian_seconds(Stop1),
    Interval = choose_interval(Start2, Stop2),
    Seeders = normalize_gauge(
		model_graphs:get_gauge(seeders, InfoHash, Start1, Stop1, Interval),
		Start2, Interval),
io:format("Seeders: ~p~n",[Seeders]),
    Leechers = normalize_gauge(
		 model_graphs:get_gauge(leechers, InfoHash, Start1, Stop1, Interval),
		 Start2, Interval),
io:format("Leechers: ~p~n",[Leechers]),
io:format("Start: ~p\tStop: ~p\tI: ~p~n", [Start2, Stop2, Interval]),
    render_graph(#graph{start = Start2,
			stop = Stop2,
			interval = Interval,
			plots =
			    [#plot{x1 = 20,
				   y1 = 199,
				   x2 = 399,
				   y2 = 1,
				   lines =
				       [#line{data = Leechers,
					      color = <<"#2f2fff">>,
					      type = smooth,
					      title = <<"Leechers">>},
					#line{data = Seeders,
					      color = <<"#2fff2f">>,
					      type = smooth,
					      title = <<"Seeders">>}
				       ]}
			    ]}).

render_traffic(InfoHash, Start, Stop) ->
    %% TODO: fill_data_gaps()
    InfoHash, Start, Stop, ok.

render_downloads(InfoHash, Start, Stop) ->
    %% TODO: fill_data_gaps()
    InfoHash, Start, Stop, ok.


choose_interval(Start, Stop) ->
    Minutes = 60,
    Hours = 60 * Minutes,
    Days = 24 * Hours,
    Years = 365 * Days,
    Diff = Stop - Start,
    io:format("Diff: ~p~n", [Diff]),
    if
	Diff =< 2 * Hours ->
	    10 * Minutes;
	Diff =< 1 * Days ->
	    30 * Minutes;
	Diff =< 3 * Days ->
	    1 * Hours;
	Diff =< 31 * Days ->
	    1 * Days;
	Diff < 360 * Days ->
	    7 * Days;
	Diff =< 3 * Years ->
	    30 * Days;
	true ->
	    365 * Days
    end.

normalize_gauge(Data1, Start, Interval) ->
    Data2 = convert_data_time(Data1),
    Data3 = case Data2 of
		[{Time1, Value1} | _]
		  when Time1 > Start,
		       Value1 > 0 ->
		    [{Time1 - Interval, 0} | Data2];
		_ ->
		    Data2
	    end,
    fixup_gauge_data(Data3, Interval).

convert_data_time(Data) ->
    [{calendar:datetime_to_gregorian_seconds({Date, {H, M, trunc(S)}}),
      Value}
     || {{Date, {H, M, S}}, Value} <- Data].

fixup_gauge_data([], _) ->
    [];
fixup_gauge_data([{Time, Value}], _) ->
    [{Time, Value}];
%% Because gauges store only changes
fixup_gauge_data([{Time1, Value1} | [{Time2, Value2} | _] = Data], Interval)
  when Value1 =/= Value2,
       Time1 < Time2 - Interval ->
    [{Time1, Value1},
     {Time2 - Interval, Value1} |
     fixup_gauge_data(Data, Interval)];
fixup_gauge_data([{Time1, Value1} | Data], Interval) ->
    [{Time1, Value1} | fixup_gauge_data(Data, Interval)].



fill_data_gaps([], _) ->
    [];
fill_data_gaps([{Start, _} | _] = Data, Interval) ->
    fill_data_gaps(Data, Start, Interval).
fill_data_gaps([], _, _) ->
    [];
fill_data_gaps([{Time, Value} | _] = Data, Start, Interval)
  when Start < Time ->
    [{Start, 0}, fill_data_gaps(Data, Start + Interval, Data)];
fill_data_gaps([{Time, Value} | Data], Start, Interval) ->
    [{Time, Value} |
     fill_data_gaps(Data, Start + Interval, Interval)].

render_graph(#graph{start = Start,
		    stop = Stop,
		    interval = Interval,
		    plots = Plots} = Graph) ->
    html:to_iolist(
      {svg, [{xmlns, ?NS_SVG},
	     {version, "1.1"},
	     {viewBox, "0 0 400 200"},
	     {width, "400px"},
	     {height, "200px"},
	     {preserveAspectRatio, "xMidYMid"}],
       [{g, [{transform, "translate(0.5, 0.5)"}],
	 lists:map(fun render_plot/1, Plots)
	}
       ]}).

render_plot(#plot{x1 = X1, y1 = Y1,
		  x2 = X2, y2 = Y2,
		  lines = Lines}) ->
    {Start, Stop, Max} =
	lists:foldl(
	  fun(#line{data = Data}, SSM) ->
		  lists:foldl(
		    fun({Time, Value}, {Start1, Stop1, Max1}) ->
			    Start2 =
				if
				    is_atom(Start1);
				    Time < Start1 ->
					Time;
				    true ->
					Start1
				end,
			    Stop2 =
				if
				    is_atom(Stop1);
				    Time > Stop1 ->
					Time;
				    true ->
					Stop1
				end,
			    Max2 =
				if
				    is_atom(Max1);
				    Value > Max1 ->
					Value;
				    true ->
					Max1
				end,
			    {Start2, Stop2, Max2}
		    end, SSM, Data)
	  end, {undefined, undefined, undefined}, Lines),
    Top = get_top(Max),
    io:format("Max: ~p\tTop: ~p~n", [Max, Top]),
    MapX = fun(Time) ->
		   (X2 - X1) * (Time - Start) / (Stop - Start) + X1
	   end,
    MapY = fun(Value) ->
		   (Y2 - Y1) * Value / Top + Y1
	   end,
    [{g, [],
      [
       %% X/Y axis
       {path, [{d, io_lib:format("M ~p ~p L ~p ~p L ~p ~p",
				 [X1, Y2, X1, Y1, X2, Y1])},
	       {fill, "none"},
	       {stroke, "black"},
	       {'stroke-width', "0.5px"}
	      ], []},
       %% Top label
       {text, [{x, io_lib:format("~p", [X1 - 2])},
	       {y, io_lib:format("~p", [Y2 + 3])},
	       {'text-anchor', "end"},
	       {'font-size', "10px"},
	       {fill, "black"}],
	integer_to_list(Top)},
       {path, [{d, io_lib:format("M ~p ~p L ~p ~p",
				 [X1 - 1, Y2, X1 + 1, Y2])},
	       {stroke, "black"},
	       {'stroke-width', "1px"}
	      ], []} |
       if
	   Top / 2 == trunc(Top / 2) ->
	       HalfTop = trunc(Top / 2),
	       HalfTopY = MapY(HalfTop),
	       [{text, [{x, io_lib:format("~p", [X1 - 2])},
			{y, io_lib:format("~p", [HalfTopY + 3])},
			{'text-anchor', "end"},
			{'font-size', "10px"},
			{fill, "black"}],
		 integer_to_list(HalfTop)},
		{path, [{d, io_lib:format("M ~p ~p L ~p ~p",
					  [X1 - 1, HalfTopY, X1 + 1, HalfTopY])},
			{stroke, "black"},
			{'stroke-width', "1px"}
		       ], []}];
	   true ->
	       []
       end
      ]} |
     [render_line(Line, MapX, MapY)
      || Line <- Lines]
    ].

%% Automatic axis scaling
get_top(Max) ->
    M = trunc(math:pow(10, trunc(math:log10(Max)))),
    lists:foldl(fun(Top, _) when Top >= Max ->
			Top;
		   (_, Top) ->
			Top
		end, Max,
		lists:seq(10 * M, 1 * M, -M)).

render_line(#line{data = Data,
		  type = Type,
		  color = Color}, MapX, MapY) ->
    Points = [{MapX(Time), MapY(Value)}
	      || {Time, Value} <- Data],
    case Type of
	smooth ->
	    case Data of
		[{Time, Value} | Data1] ->
		    {path, [{d, iolist_to_binary(smooth_path(Points))},
			    {fill, "none"},
			    {stroke, Color},
			    {'stroke-width', "2px"}], []};
		_ ->
		    []
	    end
    end.

smooth_path(Points) ->
    smooth_path(Points, 0).

smooth_path([], _) ->
    "";
smooth_path([{PX, PY} | Points], N) ->
    [io_lib:format("~s ~.5f ~.5f",
		   [if
			N == 0 ->
			    "M";
			true ->
			    ""
		    end, PX, PY]),
     case Points of
	 [{NX, NY} | _] ->
	     DX = NX - PX,
	     [io_lib:format(" C ~.5f ~.5f ~.5f ~.5f",
			    [PX + DX * 0.4, PY,
			     NX - DX * 0.4, NY]),
	      smooth_path(Points, N + 1)];
	 [] ->
	     ""
     end].
