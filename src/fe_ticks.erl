-module(fe_ticks).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([run/0,
         stop/0,
         interval/0,
         set_interval/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, 
        { tref,
          interval }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

run() ->
  gen_server:cast(?SERVER, run).

stop() ->
  gen_server:cast(?SERVER, stop).

set_interval(T) ->
  gen_server:cast(?SERVER, {set_interval, T}).

interval() ->
  gen_server:call(?SERVER, interval).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  TRef = new_interval_timer(1000),
  {ok, #state{ tref = TRef, interval=1000}}.



handle_call(interval, _From, #state{interval=T}=S) ->
  Reply = T,
  {reply, Reply, S}.

handle_cast(run, #state{ tref=undefined }=S) ->
  TRef = new_interval_timer(S#state.interval),
  {noreply, S#state{tref=TRef}};
handle_cast(run, #state{ tref=TRef }=S) ->
  cancel_timer(TRef),
  NewTRef = new_interval_timer(S#state.interval),
  {noreply, S#state{tref=NewTRef}};
handle_cast(stop, #state{tref=undefined}=S) ->
  {noreply, S};
handle_cast(stop, #state{tref=TRef}=S) ->
  cancel_timer(TRef),
  {noreply, S#state{tref=undefined}};
handle_cast({set_interval,T}, #state{tref=undefined}=S) ->
  TRef = new_interval_timer(T),
  {noreply, S#state{ tref=TRef, interval=T}};
handle_cast({set_interval,T}, #state{tref=TRef}=S) ->
  cancel_timer(TRef),
  NewTRef = new_interval_timer(T),
  {noreply, S#state{ tref=NewTRef, interval=T}}.



handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
new_interval_timer(T) ->
  {ok, TRef} = timer:apply_interval(T, fe_bcast, tick, []),
  TRef.

cancel_timer(TRef) -> 
  timer:cancel(TRef).
