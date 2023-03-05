:- module(visualizer, [visualize/1]).
/** <module> logger

This module is in charge of visualizing the function calls recorded by
the logger.

@author Daniel Santamarina

*/
:- use_module(utils, [rearrange/4]).

%% visualize(+Signatures : list).
%
% Succeeds after calculating percentages, creating a table, ordering and 
% printing <Signatures>.
%
% @param Term_signatures   The list of signature occurances given by the logger
%                          in the form <Occurances>-<Functor>/<Arity>/<Number>.
visualize(Signatures) :-
    percentages(Signatures, G_percentages, signature_to_global),
    percentages(Signatures, L_percentages, signature_to_local),
    utils:rearrange(Signatures, 
        Occurances - Functor / Arity / Number, 
        Number - Functor / Arity / Occurances,
        Signature_table),
    maplist(add_column, Signature_table, G_percentages, G_percentage_table),
    maplist(add_column, G_percentage_table, L_percentages, Percentage_table),
    keysort(Percentage_table, Print_table),
    header,
    pretty(Print_table).

%% percentages(+Signatures : list, -Percentages : list, :Signature_to_id : term).
%
% Succeeds after calculating percentages for each signature as occurrances of
% the term created by applying <Signature_to_id> to the signature. 
%
% @param Signatures         The list of signature occurances given by the logger 
%                           in the form <Occurances>-<Signature>.
% @param Percentages        The list of % occurances for each signature after
%                           applying <Signature_to_id>.
% @param Signature_to_id    A predicate in the form <Signature_to_id>(In, Out)
%                           converting a signature in the form <Functor>/
%                           <Arity>/<Number> to an ID.
percentages(Signatures, Percentages, Signature_to_id) :-
    phrase(
        percentages(Signatures, Percentages, Signature_to_id), 
        [[]], 
        [_Per_occ]).

percentages([], [], _Signature_to_id) --> [].
percentages(
        [Occurances - Signature | Tail_terms], 
        [Percent | Tail_perc], 
        Signature_to_id) --> 
    {call(Signature_to_id, Signature, Id)},
    find_and_add(Id, Occurances),
    percentages(Tail_terms, Tail_perc, Signature_to_id),
    calculate_percent(Id, Occurances, Percent).

%% find_and_add(+Element : term, +Amount : integer, _DCG_A : list, _DCG_B : list).
%
% Succeeds after finding <Element> in the DCG, and updating it's count
% by <Amount>.
% If <Element> is not present, it add's a new (<Element>, <Count>) to the DCG,
% with <Count> = <Amount>.
%
% @param Element    The ID of an element to find in the DCG.
% @param Amount     The number by which to increment the current count for
%                   <Element>.
find_and_add(Element, Amount) -->
    state(Initial, Final),
    {
        append(Start, [(Element, Count) | End], Initial),
        !,
        New_count is Count + Amount,
        append(Start, [(Element, New_count) | End], Final)
    }.
find_and_add(Element, Amount) -->
    state(Initial, Final),
    {Final = [(Element, Amount)| Initial]}.

%% calculate_percent(+Element : term, +Amount : integer, -Percent : float, _DCG_A : list, _DCG_B : list).
%
% Succeeds after calculating the proportion of <Amount> over the total
% given in the DCG for <Element>.
% If the total in the DCG is 0, <Percent> = 0 (0% of 0 is 0).
%
% @param Element    The ID of an element to find in the DCG.
% @param Amount     The numerator of the proportion.
% @param Percent    The result of <Amount> over the given total in the DCG.
calculate_percent(Element, Amount, Percent) -->
    state(Final),
    {
      member((Element, Total), Final),
      (Total =:= 0 ->
          Percent = 0
          ; 
          Percent is Amount / Total)
    }.

signature_to_global(_Signature, all).
signature_to_local(Signature, Local) :-
    Signature = Local / _.

state(S0),     [S0] --> [S0].
state(S0, S1), [S1] --> [S0].

add_column(Key - Columns, Column_to_add, Key - Columns/Column_to_add).

header :-
  format('~n~|~95t~20+ ~w ~|~95t~20+~n~n', ['CODE COVERAGE']).

pretty([]).
pretty([Number - Functor / Arity / Occurances / Global_p / Local_p | Tail]) :- 
  format('~|~t~d~3+  ~w/~w~25+# ~w~10+G ~2f~10+L ~2f~n',
    [Number, Functor, Arity, Occurances, Global_p, Local_p]),
  pretty(Tail).