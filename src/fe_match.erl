-module(fe_match).

-behaviour(gen_server).

-export([start_link/4]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-record(state,
        { match_id,
          match :: fe_mmatch:match() }).


start_link(MatchId, 
           {_HomeName,{_HomeDef,_HomeAtt}}=Home, 
           {_AwayName, {_AwayDef,_AwayAtt}}=Away, 
           StartsIn) ->
  init([MatchId, Home, Away, StartsIn]).


init([MatchId, Home, Away, StartsIn]) ->
  Match = fe_mmatch:new(Home, Away, StartsIn),
  {ok, #state{match_id = MatchId, match=Match}}.

handle_call(_, _From, State) ->
  {stop, not_implemented, going_down, State}.

handle_cast(_, State) ->
  {stop, not_implemented, State}.

handle_info(_, State) ->
  {stop, not_implemented, State}.



terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

