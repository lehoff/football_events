-module(fe_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  seed(),
  fe_match_id:init(),
  fe_sup:start_link().

stop(_State) ->
  ok.

seed() ->
  {A, B, C} = now(),
  random:need(A, B, C).
