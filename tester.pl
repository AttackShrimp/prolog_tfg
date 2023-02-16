process_calls(Clauses, Clauses_perc) :- 
  phrase(count_duplicates(Clauses), [[]], [Clause_set]),
  to_key_value(Clause_set, Clause_keys),
  keysort(Clause_keys, Clauses_sorted),
  percentage(Clauses_sorted, Clauses_perc).


count_duplicates([]) --> [].

count_duplicates([E | T]) --> 
  state(Original, Modified),
  {
    append(Start, [(E, Num) | End], Original),
    M is Num + 1,
    append(Start, [(E, M) | End], Modified)
  },
  !,
  count_duplicates(T).

count_duplicates([E | T]) --> state(S, [(E, 1) | S]), count_duplicates(T).


to_key_value([],[]).
to_key_value([(F/A - Order, Amount) | Ta], [Order - (F/A - Amount) | Tb]) :- to_key_value(Ta, Tb).


percentage(In, Out) :- percentage(In, Out, 0, _Total).
percentage([], [], Acc, Acc).
percentage([Line - (Functor / Arity - Occ) | Ta], [Line - (Functor / Arity - Occ, G_percentage) | Tb], Acc, Res) :-
  New_acc is Acc + Occ,
  percentage(Ta, Tb, New_acc, Res),
  G_percentage is Occ / Res.

pretty([]).
pretty([Line - (Functor / Arity - Occ, G_percentage) | T]) :- 
  format('~|~t~d~3+  ~w/~w~25+# ~w~10+G ~2f~n',[Line, Functor, Arity, Occ, G_percentage]),
  pretty(T).

state(S0),     [S0] --> [S0].
state(S0, S1), [S1] --> [S0].
