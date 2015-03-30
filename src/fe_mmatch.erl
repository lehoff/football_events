-module(fe_mmatch).

-export([new/4,
         tick/1,
         ticks/2]).

-type minute() :: non_neg_integer().
-type score() :: {non_neg_integer(), non_neg_integer()}.

-type status() :: {'starts_in', minute()} |
                  {'first_half', minute(), score()} |
                  {'halftime', minute(), score()} |
                  {'second_half', minute(), score()} |
                  {'finished', score()}.

-type team_info() :: {string(), {float(), float()}}.

-type match_data() :: 'undefined' |
                      #{lh => float(),
                        la => float(),
                        stoppage1 => 0..3,
                        stoppage2 => 0..6}.

-record(match,
        {match_id :: pos_integer(),
         home     :: team_info(),
         away     :: team_info(),
         status   :: status(),
         match_data :: match_data()}).

%-type match() :: #match{}.

new(MatchId, {_HomeName,{_HomeDef,_HomeAtt}}=Home, {_AwayName, {_AwayDef,_AwayAtt}}=Away, StartsIn) ->
  #match{ match_id =MatchId, 
          home = Home,
          away = Away, 
          status = {starts_in, StartsIn},
          match_data = undefined}.

tick(#match{status={finished, _}}=M) ->
  M;
tick(#match{status={starts_in, 0}}=M) ->
  {_, {HomeDef, HomeAtt}} = M#match.home,
  {_, {AwayDef, AwayAtt}} = M#match.away,
  MatchData = #{ lh => (HomeAtt * AwayDef * 1.37) /90,
                 la => (AwayAtt * HomeDef) /90,
                 stoppage1 => stoppage(1),
                 stoppage2 => stoppage(2) }, 
  M#match{ status = {first_half, 0, {0,0}},
           match_data = MatchData};
tick(#match{status={starts_in, N}}=M) ->
  M#match{status={starts_in, N-1}};
tick(#match{status={halftime, 0, Score}}=M) ->
  M#match{status = {second_half, 0, Score}};
tick(#match{status={first_half, T, Score},
            match_data=#{stoppage1 := S1}}=M) when T>45+S1 ->
  M#match{status={halftime, 15, Score}};
tick(#match{status={first_half, T, {Gh, Ga}},
            match_data=MD}=M) ->
  #{ lh := Lh, la := La} = MD,
  M#match{ status={first_half, T+1, {Gh+goal(Lh), Ga+goal(La)}} };
tick(#match{status={second_half, T, Score},
            match_data=#{stoppage2 := S2}}=M) when T>45+S2 ->
  M#match{ status = {finished, Score} };
tick(#match{status={second_half, T, {Gh, Ga}}}=M) ->
  #{ lh := Lh, la := La} = M#match.match_data,
  M#match{ status={second_half, T+1, {Gh+goal(Lh), Ga+goal(La)}} }.


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

