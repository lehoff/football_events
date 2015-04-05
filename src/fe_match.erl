-module(fe_match).

-behaviour(gen_fsm).

-export([start_link/4]).

-export([tick/1,
         status/1,
         teams/1]).

-export([broadcast_status/2]).

%% states
-export([starts_in/2, starts_in/3,
         first_half/2, first_half/3,
         halftime/2, halftime/3,
         second_half/2, second_half/3,
         finished/2, finished/3]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

%% testing and debugging
-export([test/1]).

-type status() :: {'starts_in', fe_mmatch:minute()} |
                  {'halftime', fe_mmatch:minute(), fe_mmatch:score()} |
                  fe_mmach:status().

-type match_id() :: non_neg_integer().

-export_type([status/0, match_id/0]).

-record(state,
        { match_id :: match_id(),
          home_name :: string(),
          away_name :: string(),
          match :: fe_mmatch:match() }).


start_link(MatchId, 
           {_HomeName,{_HomeDef,_HomeAtt}}=Home, 
           {_AwayName, {_AwayDef,_AwayAtt}}=Away, 
           StartsIn) ->
  gen_fsm:start_link({via, gproc, process_name(MatchId)},
                     ?MODULE, [MatchId, Home, Away, StartsIn], []).


process_name(MatchId) -> {n, l, {fe_match, MatchId}}.
  

tick(MatchId) ->
  gen_fsm:send_event({via, gproc, process_name(MatchId)}, tick).

-spec status(match_id()) -> status().
status(MatchId) ->
  gen_fsm:sync_send_all_state_event({via, gproc, process_name(MatchId)}, status).

-spec teams(match_id()) -> {string(), string()}.
teams(MatchId) ->
  gen_fsm:sync_send_all_state_event({via, gproc, process_name(MatchId)}, teams).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([MatchId, {HomeName, {AlphaH, BetaH}}, {AwayName, {AlphaA, BetaA}}, StartsIn]) ->
  Lambda = AlphaH * BetaA/90,
  Mu     = AlphaA * BetaH/90,
  Match = fe_mmatch:new(Lambda, Mu),
  fe_bcast:subscribe_tick(),
  broadcast_status(MatchId, {starts_in, StartsIn}),
  {ok, starts_in,
   {StartsIn, #state{match_id = MatchId, 
                     home_name = HomeName,
                     away_name = AwayName,
                     match=Match}}}.

starts_in(tick, {0, #state{}=S}) ->
  Status = fe_mmatch:status(S#state.match),
  broadcast_status(S#state.match_id, Status),
  {next_state, first_half, S};
starts_in(tick, {N,  #state{}=S}) ->
  broadcast_status(S#state.match_id, {starts_in, N - 1}),
  {next_state, starts_in, {N-1, S}}.

first_half(tick, #state{match=M}=S) ->
  NewM = fe_mmatch:tick(M),
  NewS = S#state{match=NewM},
  case fe_mmatch:status(NewM) of
    {halftime, Score} ->
      Status = {halftime, 15, Score},
      broadcast_status(S#state.match_id, Status),
      {next_state, halftime, {15, NewS}};
    Status ->
      broadcast_status(S#state.match_id, Status),
      {next_state, first_half, NewS}
  end.

halftime(tick, {0, #state{match=M}=S}) ->
  NewM = fe_mmatch:tick(M),
  NewS = S#state{match=NewM},
  broadcast_status(S#state.match_id, fe_mmatch:status(NewM)),
  {next_state, second_half, NewS};
halftime(tick, {N, #state{match=M}=S}) ->
  Status = {halftime, N-1, fe_mmatch:status(M)},
  broadcast_status(S#state.match_id, Status),
  {next_state, halftime, {N-1, S}}.

second_half(tick, #state{match=M}=S) ->
  NewM = fe_mmatch:tick(M),
  NewS = S#state{match=NewM},
  case fe_mmatch:status(NewM) of
    {finished, Score} ->
      Status = {halftime, 15, Score},
      broadcast_status(S#state.match_id, Status),
      {next_state, finished, NewS};
    Status ->
      broadcast_status(S#state.match_id, Status),
      {next_state, second_half, NewS}
  end.

finished(tick, S) ->
  {next_state, finished, S}.

starts_in(_Event, _From, {_N, S}) ->
  {stop, not_implemented, going_down, S}.

first_half(_Event, _From, S) ->
    {stop, not_implemented, going_down, S}.

halftime(_Event, _From, {_N,S}) ->
  {stop, not_implemented, going_down, S}.

second_half(_Event, _From, S) ->
  {stop, not_implemented, going_down, S}.

finished(_Event, _From, S) ->
  {stop, not_implemented, going_down, S}.


handle_sync_event(status, _From, starts_in, {N, S}) ->
  Reply = {starts_in, N},
  {reply, Reply, starts_in, {N,S}};
handle_sync_event(status, _From, halftime, {N, S}) ->
  {halftime, Score} = fe_mmatch:status(S#state.match),
  Reply = {halftime, N, Score},
  {reply, Reply, halftime, {N,S}};
handle_sync_event(status, _From, StateName, S) ->
  Reply = fe_mmatch:status(S#state.match),
  {reply, Reply, StateName, S};

handle_sync_event(teams, _From, StateName, {N, S}) ->
  Reply = {S#state.home_name, S#state.away_name},
  {reply, Reply, StateName, {N, S}};
handle_sync_event(teams, _From, StateName, S) ->
  Reply = {S#state.home_name, S#state.away_name},
  {reply, Reply, StateName, S}.

handle_event(_Event, _StateName, State) ->
  {stop, not_implemented, State}.


handle_info(tick, StateName, {N,S}) ->
  fe_match:tick(S#state.match_id),
  {next_state, StateName, {N,S}};
handle_info(tick, StateName, S) ->
  fe_match:tick(S#state.match_id),
  {next_state, StateName, S}.




terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper functions

broadcast_status(MatchId, Status) ->
  fe_bcast:status(MatchId, Status).

test(1) ->
  start_link(1, 
             {"ManU", {2.4,0.3}},
             {"ManC", {2.7,0.45}},
             10);
test({new_id, N}) ->
  start_link(N, 
             {"ManU", {2.4,0.3}},
             {"ManC", {2.7,0.45}},
             10).

