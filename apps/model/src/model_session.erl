-module(model_session).

-export([generate/1,
	 validate/1
	]).

-define(POOL, pool_users).
-define(Q(Stmt, Params), model_sup:equery(?POOL, Stmt, Params)).
-define(T(Fun), model_sup:transaction(?POOL, Fun)).


generate(UserName) ->
    Sid = generate_sid(),
    {ok, 1} =
	?Q("INSERT INTO user_sessions (\"user\", \"sid\", \"updated\") VALUES ($1, $2, NOW())",
	   [UserName, Sid]),
    {ok, Sid}.

generate_sid() ->
    util:seed_random(),

    << <<(random:uniform(256) - 1):8>>
       || _ <- lists:seq(1, 16) >>.

validate(Sid) ->
    case ?Q("UPDATE login_tokens SET \"updated\"=NOW() WHERE \"sid\"=$1 RETURNING \"user\"", 
	    [Sid]) of
	{ok, 1, _, [{UserName}]} ->
	    {ok, UserName};
	_ ->
	    {error, invalid_session}
    end.
