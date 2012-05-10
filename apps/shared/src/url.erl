-module(url).

-export([join/2]).

join(Base, Target)
  when is_binary(Base) ->
    join(binary_to_list(Base),
	 Target
	);

join(Base, Target)
  when is_binary(Target) ->
    list_to_binary(
      join(Base,
	   binary_to_list(Target)
	  ));
    
join(Base, Target) ->
    ColonIndex =
	length(lists:takewhile(fun($:) -> false;
				  (_) -> true
			       end, Target)),
    if
	ColonIndex < 16 andalso
	ColonIndex < length(Target) ->
	    %% Target is full link w/ "proto:"
	    Target;
	true ->
	    %% Target is relative
	    {Host, Port, Path, Ssl} =
		lhttpc_lib:parse_url(Base),
	    NewPath = filename:join(Path, Target),
	    io_lib:format("~s://~s:~B~s",
			  [if
			       Ssl ->
				   "https";
			       true ->
				   "http"
			   end,
			   Host,
			   Port,
			   NewPath
			  ])
    end.
