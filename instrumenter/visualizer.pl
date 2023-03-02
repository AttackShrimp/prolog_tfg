:- module(visualizer, [get_percentages/3]).

get_percentages(Signatures, Global_percentages, Local_percentages) :-
    phrase(percentages(Signatures, Global_percentages, signature_to_global), [[]], [_Global_occ]),
    phrase(percentages(Signatures, Local_percentages, signature_to_local), [[]], [_Local_occ]).

percentages([], [], _Signature_to_id) --> [].
percentages(
        [Occurances - Signature | Tail_terms], 
        [Percent | Tail_perc], 
        Signature_to_id) --> 
    {call(Signature_to_id, Signature, Id)},
    find_and_add(Id, Occurances),
    percentages(Tail_terms, Tail_perc, Signature_to_id),
    get_percentage(Id, Occurances, Percent).

find_and_add(Element, Amount) -->
    state(Initial, Final),
    {
        append(Start, [(Element, Total_occurances) | End], Initial),
        !,
        New_occurances is Total_occurances + Amount,
        append(Start, [(Element, New_occurances) | End], Final)
    }.
find_and_add(Element, Amount) -->
    state(Initial, Final),
    {Final = [(Element, Amount)| Initial]}.

get_percentage(ID, Occurances, Percent) -->
    state(Final),
    {
      member((ID, Total_occ), Final),
      (Total_occ =:= 0 ->
          Percent = 0
          ; 
          Percent is Occurances / Total_occ)
    }.

signature_to_global(_Signature, all).
signature_to_local(Signature, Local) :-
    Signature = Local / _.

state(S0),     [S0] --> [S0].
state(S0, S1), [S1] --> [S0].

% ___________________________________________________


header :-
  format('~n~|~95t~20+ ~w ~|~95t~20+~n~n', ['CODE COVERAGE']).

pretty([]).
pretty([Line - (Functor / Arity - Occ, G_percentage, L_percentage) | T]) :- 
  format('~|~t~d~3+  ~w/~w~25+# ~w~10+G ~2f~10+L ~2f~n',[Line, Functor, Arity, Occ, G_percentage, L_percentage]),
  pretty(T).