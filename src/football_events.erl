-module(football_events).


-export([new_match/0,
         matches/0]).

-export([subscribe/1,
         unsubscribe/1]).

%% -export([start_ticks/0,
%%          tick_interval/0,
%%          set_tick_interval/1]).



%% @doc Generates a new match with two random teams, to start in 10..60 minutes.
new_match() ->
  MatchId = fe_match_id:new(),
  Fixtures = fe_teams:all_fixtures(),
  Index = random:uniform(length(Fixtures)) - 1,
  {Home, Away} = lists:nth(Index, Fixtures),
  HomeStrength = fe_teams:strength(Home),
  AwayStrength = fe_teams:strength(Away),
  StartsIn = random:uniform(51) + 9,
  fe_match_sup:start_match(MatchId, 
                           {Home, HomeStrength},
                           {Away, AwayStrength},
                           StartsIn).
  
matches() ->
  Key = {fe_match, '$1'},
  GProcKey = {n, l, Key},
  MatchHead = {GProcKey, '_', '_'},
  Guard = [],
  Result = ['$1'],
  gproc:select([{MatchHead, Guard, Result}]).

subscribe(MatchId) ->
  gproc:reg({p, l, {match_info, MatchId}}).

unsubscribe(MatchId) ->
  gproc:unreg({p, l, {match_info, MatchId}}).

