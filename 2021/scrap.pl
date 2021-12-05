state(S), [S] --> [S].
state(S0, S), [S] --> [S0].

sum([]) --> [].
sum([X|Xs]) -->
  state(S0, S),
  {S #= S0 + X},
  sum(Xs).
