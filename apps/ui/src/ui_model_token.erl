-module(ui_model_token).

-export([init/0,
	 generate/1,
	 validate/1
	]).

-record(ui_token, {token, username, time}).

%% TODO: with app start
init() ->
    mnesia:create_table(ui_token,
			[{attributes, record_info(fields, ui_token)}
			]).

generate(UserName) ->
    case model_users:get_salted(UserName) of
	{ok, _Salted, Salt} ->
	    Token = generate_token(),
	    {atomic, ok} =
		mnesia:transaction(
		  fun() ->
			  mnesia:write(#ui_token{username = UserName,
						 token = Token,
						 time = util:get_now()
						})
		  end),
	    {ok, Salt, Token};
	{error, not_found} ->
	    {error, not_found}
    end.

generate_token() ->
    << <<(random:uniform(8)):8>>
       || _ <- lists:seq(1, 16) >>.

validate(Token) ->
    {atomic, Tokens} =
	mnesia:transaction(
	  fun() ->
		  %% Get
		  Tokens = mnesia:read(ui_token, Token),
		  %% Delete
		  lists:foreach(fun mnesia:delete_object/1, Tokens),
		  %% Return
		  Tokens
	  end),

    case Tokens of
	[] ->
	    {error, invalid_token};
	[#ui_token{username = UserName}] ->
	    case model_users:get_salted(UserName) of
		{ok, Salted, Salt} ->
		    {ok, UserName, Salted, Salt};
		{error, not_found} ->
		    {error, not_found}
	    end
    end.
