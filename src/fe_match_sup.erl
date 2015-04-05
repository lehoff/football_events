-module(fe_match_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([start_match/4]).

-export([init/1]).



start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_match(MatchId, Home, Away, StartsIn) ->
  supervisor:start_child(?MODULE, [MatchId, Home, Away, StartsIn]).


init([]) ->
  MatchSpec = {fe_match, {fe_match, start_link, []},
               transient, brutal_kill, worker, [fe_match]},
  Procs = [MatchSpec],
  {ok, {{simple_one_for_one, 1, 5}, Procs}}.
