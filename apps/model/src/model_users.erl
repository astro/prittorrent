-module(model_users).

-export([register/3, authenticate/2, activate/1]).


-define(POOL, pool_users).
-define(Q(Stmt, Params), model_sup:equery(?POOL, Stmt, Params)).

register(Name, Email, Password) ->
    ?Q("INSERT INTO users (\"name\", \"email\", \"password\") VALUES ($1, $2, $3)",
       [Name, Email, Password]).

authenticate(Name, Password) ->
    case ?Q("SELECT \"name\" FROM users WHERE \"name\"=$1 AND \"password\"=$2 AND \"activated\"",
	    [Name, Password]) of
	{ok, _, [_ | _]} ->
	    ok;
	_ ->
	    denied
    end.

activate(Name) ->
    ?Q("UPDATE users SET \"activated\"=TRUE WHERE \"name\"=$1",
       [Name]).
