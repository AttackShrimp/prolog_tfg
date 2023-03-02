:- module(logger, [init/1, pred_start/2, clean/0]).
/** <module> logger

This module is in charge of asserting and managing the calls to pred_start

@author Daniel Santamarina

*/
:- dynamic coverage/2.

%% init(+Term_signatures : list)
%
% Succeeds after asserting a 'coverage' term to the knowledge base, acting
% as a counter for pred_start called with the same parameters.
%
% @param Term_signatures   The list of signatures for all terms to handle
%                          terms in the form <ID>-<Functor>/<Arity>.
init([]).
init([Number - Functor / Arity | Tail]) :-
    assertz(coverage(Number - Functor / Arity, 0)),
    init(Tail).

%% pred_start(+Number : number, +Name : '/'(string, int)).
%
% Succeeds after asserting a 'coverage' term to the knowledge base, acting
% as a counter for pred_start called with the same parameters.
% If this specific parameter combination has already been asserted, adds
% 1 to the counter.
%
% @param Number      ID of the predicate.
% @param Name        Functor/Arity of the predicate head.
pred_start(Number, Functor / Arity) :-
    coverage(Number - Functor / Arity, Count),
    !,
    Next is Count + 1,
    retract(coverage(Number - Functor / Arity, Count)),
    assertz(coverage(Number - Functor / Arity, Next)).
pred_start(Number, Functor / Arity) :-
    assertz(coverage(Number - Functor / Arity, 1)).

%% clean.
%
% Succeeds after retracting all asserted coverage/2 terms.
clean :-
    retractall(coverage(_,_)).