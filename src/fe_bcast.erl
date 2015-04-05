-module(fe_bcast).

-export([status/2]).
-export([subscribe_tick/0,
         tick/0]).

status(MatchId, Status) ->
  MatchInfo = {match_info, MatchId},
  gproc:send({p, l, MatchInfo}, {MatchInfo, Status}).

subscribe_tick() ->
  gproc:reg({p, l, fe_ticks}).

tick() ->
  gproc:send({p, l, fe_ticks}, tick).
