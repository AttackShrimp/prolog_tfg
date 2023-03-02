:- module(logger, [pred_start/2, clean/0]).
/** <module> logger

This module is in charge of asserting and managing the calls to pred_start

@author Daniel Santamarina

*/
:- dynamic covered/2.

%% pred_start(+Number : number, +Name : '/'(string, int)).
%
% Succeeds after asserting a 'covered' term to the knowledge base, acting
% as a counter for pred_start called with the same parameters.
% If this specific parameter combination has already been asserted, adds
% 1 to the counter.
%
% @param Number      ID of the predicate.
% @param Name        Functor/Arity of the predicate head.
pred_start(Number, Functor / Arity) :-
    covered(Number - Functor / Arity, Count),
    !,
    Next is Count + 1,
    retract(covered(Number - Functor / Arity, Count)),
    assertz(covered(Number - Functor / Arity, Next)).
pred_start(Number, Functor / Arity) :-
    assertz(covered(Number - Functor / Arity, 1)).

%% clean.
%
% Succeeds after retracting all asserted covered/2 terms.
clean :-
    retractall(covered(_,_)).