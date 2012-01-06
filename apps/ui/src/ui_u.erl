-module(ui_u).

%% API
-export([run/1]).

%% Internal
-export([handle_user/1]).

-record(session, {ws, user}).


run(WS) ->
    wait_for_authentication(WS).

wait_for_authentication(WS) ->
    receive
	{browser, Data} ->
	    {ok, {obj, Obj}, _} = rfc4627:decode(Data),
	    User = proplists:get_value("user", Obj),
	    Password = proplists:get_value("password", Obj),
	    ok = model_users:authenticate(User, Password),
	    handle_user(#session{user = User, ws = WS});
	closed ->
	    io:format("WS closed~n")
    after 10000 ->
	    timeout
    end.


handle_user(Session) ->
    receive
	{browser, Data} ->
	    {ok, Value, _} = rfc4627:decode(Data),
	    io:format("From WS: ~p~n",[Value]),
	    Session2 = handle_data(Value, Session),
	    ?MODULE:handle_user(Session2);
	closed ->
	    io:format("WS closed~n")
    end.    

handle_data(Value, Session) ->
    Session.
