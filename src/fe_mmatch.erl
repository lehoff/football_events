-module(fe_mmatch).

-export([new/2,
         tick/1,
         ticks/2,
         status/1]).

-export([test/1]).

-type minute() :: non_neg_integer().
-type score() :: {non_neg_integer(), non_neg_integer()}.

-type status() :: {'first_half', minute(), score()} |
                  {'halftime', score()} |
                  {'second_half', minute(), score()} |
                  {'finished', score()}.


-record(match,
        {lh :: float(),
         la :: float(),
         stoppage1 :: 0..3,
         stoppage2 :: 0..6,
         status :: status()}).


-type match() :: #match{}.

-export_type([match/0]).

-spec new(float(), float()) -> match().
new(LambdaHome, LambdaAway) ->
  #match{ lh = LambdaHome * 1.37,
          la = LambdaAway, 
          stoppage1 = stoppage(1),
          stoppage2 = stoppage(2),
          status = {first_half, 0, {0,0}}}.

-spec status(match()) -> status().
status(#match{status=Status}) ->
  Status.

tick(#match{status={finished, _}}=M) ->
  M;
tick(#match{status={halftime, Score}}=M) ->
  M#match{status = {second_half, 45, Score}};
tick(#match{status={first_half, T, Score},
            stoppage1 = S1}=M) when T>=45+S1 ->
  M#match{status={halftime, Score}};
tick(#match{status={second_half, T, Score},
            stoppage2 = S2}=M) when T>=90+S2 ->
  M#match{ status = {finished, Score} };
tick(#match{lh=Lh, la= La,
            status={Half, T, {Gh, Ga}}}=M) ->
  M#match{ status={Half, T+1, {Gh+goal(Lh), Ga+goal(La)}} }.


ticks(M, 0) ->
  M;
ticks(M, N) ->
  ticks(tick(M), N-1).

goal(L) ->
  case (rand() < L) of
    true-> 1;
    false -> 0
  end.

rand() ->
  <<N:32>> = crypto:rand_bytes(4),
  N / 4294967296.

stoppage(1) ->
  crypto:rand_uniform(0,3);
stoppage(2) ->
  crypto:rand_uniform(1,3) + crypto:rand_uniform(0,4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test(1) ->
  new(2.4 * 0.4/90, 2.7*0.34/90).
