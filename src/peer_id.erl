-module(peer_id).

-export([generate/0]).

generate() ->
    <<"-<30000-",
      << <<0>> || _ <- lists:seq(1, 12) >>/binary
      >>.
