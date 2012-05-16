-module(model_token).

-export([generate/2,
	 validate/2,
	 peek/2
	]).

-define(POOL, pool_users).
-define(Q(Stmt, Params), model_sup:equery(?POOL, Stmt, Params)).
-define(T(Fun), model_sup:transaction(?POOL, Fun)).


generate(Kind, UserName) when is_atom(Kind) ->
    generate(atom_to_list(Kind), UserName);
generate(Kind, UserName) when is_list(Kind) ->
    generate(list_to_binary(Kind), UserName);
generate(Kind, UserName) ->
    Token = generate_token(),
    {ok, 1} =
	?Q("INSERT INTO user_tokens (\"kind\", \"user\", \"token\", \"created\") VALUES ($1, $2, $3, NOW())",
	   [Kind, UserName, Token]),
    {ok, Token}.

generate_token() ->
    util:seed_random(),

    << <<(random:uniform(256) - 1):8>>
       || _ <- lists:seq(1, 16) >>.

validate(Kind, Token) when is_atom(Kind) ->
    validate(atom_to_list(Kind), Token);
validate(Kind, Token) when is_list(Kind) ->
    validate(list_to_binary(Kind), Token);
validate(Kind, Token) ->
    case ?Q("DELETE FROM user_tokens WHERE \"kind\"=$1 AND \"token\"=$2 RETURNING \"user\"", 
	    [Kind, Token]) of
	{ok, 1, _, [{UserName}]} ->
	    {ok, UserName};
	_ ->
	    {error, invalid_token}
    end.

%% validate w/o removing
peek(Kind, Token) when is_atom(Kind) ->
    peek(atom_to_list(Kind), Token);
peek(Kind, Token) when is_list(Kind) ->
    peek(list_to_binary(Kind), Token);
peek(Kind, Token) ->
    case ?Q("SELECT \"user\" FROM user_tokens WHERE \"kind\"=$1 AND \"token\"=$2",
	    [Kind, Token]) of
	{ok, _, [{UserName}]} ->
	    {ok, UserName};
	_ ->
	    {error, invalid_token}
    end.

