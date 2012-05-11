-module(model_token).

-export([generate/1,
	 validate/1
	]).

-define(POOL, pool_users).
-define(Q(Stmt, Params), model_sup:equery(?POOL, Stmt, Params)).
-define(T(Fun), model_sup:transaction(?POOL, Fun)).


generate(UserName) ->
    case model_users:get_salted(UserName) of
	{ok, _Salted, Salt} ->
	    Token = generate_token(),
	    {ok, 1} =
		?Q("INSERT INTO login_tokens (\"user\", \"token\", \"created\") VALUES ($1, $2, NOW())",
		   [UserName, Token]),
	    {ok, Salt, Token};
	{error, not_found} ->
	    {error, not_found}
    end.

generate_token() ->
    util:seed_random(),

    << <<(random:uniform(256) - 1):8>>
       || _ <- lists:seq(1, 16) >>.

validate(Token) ->
    case ?Q("DELETE FROM login_tokens WHERE \"token\"=$1 RETURNING \"user\"", 
	    [Token]) of
	{ok, 1, _, [{UserName}]} ->
	    case model_users:get_salted(UserName) of
		{ok, Salted, Salt} ->
		    {ok, UserName, Salted, Salt};
		{error, not_found} ->
		    {error, not_found}
	    end;
	_ ->
	    {error, invalid_token}
    end.
