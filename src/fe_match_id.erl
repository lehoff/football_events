-module(fe_match_id).

-export([init/0]).
-export([new/0]).

-define(MATCH_ID, match_id).

init() ->
  gproc:add_shared_local_counter(?MATCH_ID, 0).

new() ->
  gproc:update_shared_counter({c, l, ?MATCH_ID}, 1).
