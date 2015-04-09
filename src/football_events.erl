-module(football_events).


-export([new_match/0,
         matches/0,
         match_info/1]).

-export([subscribe/1,
         unsubscribe/1]).

-export([start_ticks/0,
         stop_ticks/0,
         tick_interval/0,
         set_tick_interval/1]).

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
                           StartsIn),
  MatchId.
  
%% @doc returns a list of match ids
matches() ->
  Key = {fe_match, '$1'},
  GProcKey = {n, l, Key},
  MatchHead = {GProcKey, '_', '_'},
  Guard = [],
  Result = ['$1'],
  gproc:select([{MatchHead, Guard, Result}]).

%% @doc returns a pair consisting of the pair of teams involed and the current
%% status of the match.
match_info(MatchId) ->
  {fe_match:teams(MatchId), fe_match:status(MatchId)}.

%% @doc subscribes the calling process to match_info events from the MatchId match.
%% The info has the shape {match_info, match_id()}, status()}. 
subscribe(MatchId) ->
  gproc:reg({p, l, {match_info, MatchId}}).

unsubscribe(MatchId) ->
  gproc:unreg({p, l, {match_info, MatchId}}).


%% @doc start the ticks that drives forward the matches.
start_ticks() ->
  fe_ticks:run().

stop_ticks() ->
  fe_ticks:stop().

%% @doc get the time in milli seconds between the ticks.
tick_interval() ->
  fe_ticks:interval().

%% @doc set the tick interval to T milli seconds.
set_tick_interval(T) ->
  fe_ticks:set_interval(T).
