-module(fe_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  MatchSup = {fe_match_sup, {fe_match_sup, start_link, []},
              transient, infinity, supervisor, [fe_match_sup]},
  Ticks = {fe_ticks, {fe_ticks, start_link, []},
           permanent, brutal_kill, worker, [fe_ticks]},
  Procs = [MatchSup, Ticks],
  {ok, {{one_for_one, 1, 5}, Procs}}.
