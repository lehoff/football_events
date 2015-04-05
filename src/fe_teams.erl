-module(fe_teams).

-compile(export_all).

-export([all/0,
         strength/1,
         all_strengths/0]).

-export([all_fixtures/0]).

-export([expected/2]).

all() ->
  maps:keys(team_performance()).

%% We fix the average beta to 0.45 and calculate everything from that.

%% equations:
%% AvgGF =  (alpha * avg(beta) + (xi_1+xi_2)/4) * (1+rho)/2 
%% beta = AvgGA / avg_goals_per_team * avg(beta)
strength(Team) ->
  {G, _WDL, {GF, GA}, _P} = maps:get(Team, team_performance()),
  AvgGF = GF/G,
  AvgGA = GA/G,
  Alpha = ( (AvgGF / ((1+1.37)/2)) - (0.67+0.47)/4 )  / 0.45,
  Beta = AvgGA / avg_goals_per_team() * 0.45,
  {Alpha, Beta}.

all_strengths() ->
  Teams = all(),
  [ {Team, strength(Team)} || Team <- Teams ].

expected(Home, Away) ->
  {Lambda, Mu} = rates(Home, Away),
  ExpH = (Lambda + 0.67/2)* 1.37,
  ExpA = (Mu + 0.47/2),
  {ExpH, ExpA}. 

rates(Home, Away) ->
  {Ah,Bh} = strength(Home),
  {Aa,Ba} = strength(Away),
  { Ah*Ba, Aa*Bh}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% helper functions


avg_goals_per_team() ->
  0.5 * goals_scored() / games_played().

goals_scored() ->
  maps:fold(fun (_K,{_,_,{GF,_},_}, Acc) -> Acc+GF end,
            0,
            team_performance()).

games_played() ->
  maps:fold(fun (_K,{G,_,{_,_},_}, Acc) -> Acc+G end,
            0,
            team_performance()) / 2.

team_performance() ->
 #{ 
    "Chelsea" => {29,{20,7,2},{61,25},67},
    "Manchester City" => {30,{18,7,5},{62,28},61},
    "Arsenal" => {30,{18,6,6},{58,31},60},
    "Manchester United" => {30,{17,8,5},{52,27},59},
    "Liverpool" => {30,{16,6,8},{44,32},54},
    "Southampton" => {30,{16,5,9},{42,21},53},
    "Tottenham Hotspur" => {30,{16,5,9},{50,45},53},
    "Swansea City" => {30,{12,7,11},{34,38},43},
    "West Ham United" => {30,{11,9,10},{40,37},42},
    "Stoke City" => {30,{12,6,12},{34,37},42},
    "Crystal Palace" => {30,{9,9,12},{36,41},36},
    "Newcastle United" => {30,{9,8,13},{33,48},35},
    "Everton" => {30,{8,10,12},{38,42},34},
    "West Bromwich Albion" => {30,{8,9,13},{27,39},33},
    "Hull City" => {30,{6,10,14},{28,40},28},
    "Aston Villa" => {30,{7,7,16},{19,39},28},
    "Sunderland" => {30,{4,14,12},{23,44},26},
    "Burnley" => {30,{5,10,15},{26,49},25},
    "Queens Park Rangers" => {30,{6,4,20},{31,54},22},
    "Leicester City" => {29,{4,7,18},{27,48},19}
  }.


play_match(Home, Away) ->
  {Lambda, Mu } = rates(Home,Away),
  M = fe_mmatch:new(Lambda/90, Mu/90),
  Score = fe_mmatch:sim_match(M),
  {{Home, Away}, Score}.

all_fixtures() ->
  Teams = all(),
  [ {Home, Away} || Home <- Teams,
                    Away <- Teams,
                    Home /= Away ].

play_season() ->
  Fixtures = all_fixtures(),
  [ play_match(Home, Away) || {Home,Away} <- Fixtures ].
  
results_to_points(Results) ->
  lists:flatten( [ result_to_points(Res) || Res <- Results ] ).

results_to_goals(Results) ->
  lists:flatten( [ result_to_goals(Res) || Res <- Results ] ).

table(Results) ->
  Points = results_to_points(Results),
  Goals  = results_to_goals(Results),
  Summed = [ {Team, sum_goals(Team, Goals), sum_points(Team, Points)} || Team <- all() ],
  lists:sort( fun team_rank/2, Summed).
                   
team_rank({_,_,P1}, {_,_,P2}) when P1 > P2 -> true;
team_rank({_,_,P1}, {_,_,P2}) when P1 < P2 -> false;
team_rank({_,{GF1,GA1},P}, {_,{GF2,GA2},P}) when (GF1-GA1) == (GF2-GA2) -> GF1>GF2; 
team_rank({_,{GF1,GA1},P}, {_,{GF2,GA2},P}) -> (GF1-GA1) > (GF2-GA2).
  

sum_goals(Team, Goals) ->  
  TeamGoals = [ Score || {T, Score} <- Goals,
                         T == Team ],
  lists:foldl( fun({X,Y}, {Xt, Yt}) ->
                   {Xt+X, Yt+Y}
               end,
               {0,0},
               TeamGoals ).
  
  
sum_points(Team, Points) ->
  lists:sum([P || {T,P} <- Points,
                  T == Team]).

result_to_goals({{Home, Away}, {X, Y}}) ->
  [{Home, {X,Y}}, {Away,{Y,X}}].

result_to_points({{Home, Away}, {N, N}}) ->
  [{Home, 1}, {Away, 1}];
result_to_points({{Home, _Away}, {X, Y}}) when X>Y ->
  [{Home, 3}];
result_to_points({{_Home, Away}, _}) ->
  [{Away, 3}].
                 
