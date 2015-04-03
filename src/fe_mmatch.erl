-module(fe_mmatch).

-compile(export_all).

-export([new/2,
         tick/1,
         ticks/2,
         status/1]).

-export([test/1,
         stats/1]).

-type minute() :: non_neg_integer().
-type score() :: {non_neg_integer(), non_neg_integer()}.

-type status() :: {'first_half', minute(), score()} |
                  {'halftime', score()} |
                  {'second_half', minute(), score()} |
                  {'finished', score()}.


-record(match,
        {lambda    :: float(),
         mu        :: float(),
         stoppage1 :: 0..3,
         stoppage2 :: 0..6,
         status    :: status()}).


-type match() :: #match{}.

-export_type([status/0, match/0, minute/0, score/0]).

-spec new(float(), float()) -> match().
new(Lambda, Mu) ->
  #match{lambda = Lambda * 1.37,
         mu = Mu,
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
tick(#match{lambda=Lambda, mu=Mu,
            status={Half, T, {Gh, Ga}=Score}}=M) ->
  M#match{ status={Half, T+1, {Gh+goal(lambda(Lambda, Score, T)), Ga+goal(mu(Mu, Score, T))}} }.


ticks(M, 0) ->
  M;
ticks(M, N) ->
  ticks(tick(M), N-1).

goal(L) ->
  case (rand() < L) of
    true-> 1;
    false -> 0
  end.

lambda(Lambda, Score, T) ->
  Lambda * lambda_xy(Score) + xi_1(T).

mu(Mu, Score, T) ->
  Mu * mu_xy(Score) + xi_2(T).

lambda_xy({N,N}) -> 1.00;
lambda_xy({1,0}) -> 0.86;
lambda_xy({0,1}) -> 1.10;
lambda_xy({Gh, Ga}) when Gh > Ga, Gh > 1 -> 1.01;
lambda_xy(_) -> 1.13.

mu_xy({N,N}) -> 1.00;
mu_xy({1,0}) -> 1.33;
mu_xy({0,1}) -> 1.07;
mu_xy({Gh, Ga}) when Gh > Ga, Gh > 1 -> 1.53;
mu_xy(_) -> 1.16.
         
xi_1(T) -> 0.67/90 * T/90.

xi_2(T) -> 0.47/90 * T/90.     
  
  
  


rand() ->
  <<N:32>> = crypto:rand_bytes(4),
  N / 4294967296.

stoppage(1) ->
  crypto:rand_uniform(0,3);
stoppage(2) ->
  crypto:rand_uniform(1,3) + crypto:rand_uniform(0,4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test(1) ->
  new(2.27*0.28/90, 2.55*0.36/90);
test(2) ->
  M = test(1),
  [ sim_match(M) || _ <- lists:seq(1,10000) ];
test(3) ->
  {2.27*0.28/90, 2.55*0.36/90};
test(4) ->
  Alpha =1.877,
  Beta  =0.442, 
  Mu = Alpha * Beta,
  Lambda = Mu,
  new(Lambda, Mu);  
test(N) ->
  [ sim_match(random_match()) || _ <- lists:seq(1,N) ]. 
  

rates(Lambda, Mu, Score, T) ->
  {90*lambda(Lambda, Score, T)*1.37, 90*mu(Mu, Score, T)}.

sim_match(#match{status={finished, Score}}) ->
  Score; 
sim_match(M) ->
  sim_match(tick(M)).

stats(Rs) ->
  AllScores = lists:usort(Rs),
  [ {S, count(S, Rs)} || S <- AllScores ].

grouped_stats(Stats) ->
  TwoNil = [ St || {{X,0},_}=St <- Stats,
                   X>1],
  TwoOne = [ St || {{X,1},_}=St <- Stats,
                       X>1],
  NilTwo = [ St || {{0,Y},_}=St <- Stats,
                   Y>1],
  OneTwo = [ St || {{1,Y},_}=St <- Stats,
                   Y>1],
  Rest   = [ St || {{X,Y},_}=St <- Stats,
                   X>1, Y>1 ],
  [ lists:keyfind({0,0}, 1, Stats),
    lists:keyfind({1,0}, 1, Stats),
    lists:keyfind({0,1}, 1, Stats),
    lists:keyfind({1,1}, 1, Stats),
    sum_grouped(TwoNil, {x,0}),
    sum_grouped(TwoOne, {x,1}),
    sum_grouped(NilTwo, {0,y}),
    sum_grouped(OneTwo, {1,y}),
    sum_grouped(Rest, {x,y})].

sum_grouped(Stats, Tag) ->
  Sum = lists:sum( [ C || {_,C} <- Stats ]),
  {Tag, Sum}.
                   
random_match() ->
  new(random_rate(), random_rate()).

random_rate() ->
  r_alpha() * r_beta() / 90.

r_alpha() ->
  1.516 + random:uniform() * 1.1.

r_beta() ->
  0.26 + random:uniform() * 0.25.
  

count(_, []) ->
  0;
count(X, [X|Xs]) ->
  1 + count(X, Xs);
count(X, [_|Xs]) ->
  count(X, Xs).
