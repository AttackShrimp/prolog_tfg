:- module(logger, [init/1, pred_start/2, coverage/1, clean/0, stop/0]).
/** <module> logger

This module is in charge of asserting and managing the calls to pred_start

*/
:- dynamic coverage/2.
:- dynamic grounds/2.
:- dynamic branches/2.

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
% @param Number      The ID of the predicate.
% @param Name        The Functor/Arity of the predicate head.
pred_start(Number, Functor / Arity) :-
    coverage(Number - Functor / Arity, Count),
    !,
    Next is Count + 1,
    retract(coverage(Number - Functor / Arity, Count)),
    assertz(coverage(Number - Functor / Arity, Next)).
pred_start(Number, Functor / Arity) :-
    assertz(coverage(Number - Functor / Arity, 1)).

pred_ground(Predicate) :-
    get_ground_indicators(Predicate, Ground_indicators), 
    grounds(Ground_indicators, Count),
    !,
    Next is Count + 1,
    retract(grounds(Ground_indicators, Count)),
    assertz(grounds(Ground_indicators, Next)).
pred_ground(Predicate) :-
    get_ground_indicators(Predicate, Ground_indicators),
    assertz(grounds(Ground_indicators, 1)).

get_ground_indicators(Predicate, Pred_with_indicators) :-
    functor(Predicate, _, Arity), 
    Predicate =.. [Head | Args], 
    length(Indicators, Arity), 
    maplist(detect_ground, Args, Indicators), 
    Pred_with_indicators=..[Head | Indicators].

detect_ground(P, g) :- ground(P), !.
detect_ground(_, ng).

pred_branch(Branch_list, Predicate) :-
    maplist(unifies(Predicate), Branch_list, Branches),
    functor(Predicate, Functor, Arity), 
    branches(Functor / Arity - Branches, Count),
    !,
    Next is Count + 1,
    retract(branches(Functor / Arity - Branches, Count)),
    assertz(branches(Functor / Arity - Branches, Next)).
pred_branch(Branch_list, Predicate) :-
    maplist(unifies(Predicate), Branch_list, Branches),
    functor(Predicate, Functor, Arity),
    assertz(branches(Functor / Arity - Branches, 1)).

unifies(Predicate, Branch, 1) :- unifiable(Predicate, Branch, _), !.
unifies(_, _, 0).

%% coverage(-Covered_terms: list).
%
% Succeeds after setting Covered_terms to the list of terms with 
% number of calls to pred_start.
%
% @param Covered_terms  The list of terms with number of calls in the format
% <CallNumber> - <Functor> / <Arity> / <Number>
coverage(Covered_terms) :-
    findall(Count - Functor / Arity / Number, 
        coverage(Number - Functor / Arity, Count),
        Covered_terms).

%% clean.
%
% Succeeds after setting all coverage/2 terms back to 0, restarting the logger
% counter.
%
% All existing asserted coverage/2 terms are retracted and asserted with the 
% counter set to 0.
clean :-
    findall(coverage(A,0), coverage(A,_), Logs),
    retractall(coverage(_,_)),
    maplist(assertz, Logs).

%% stop.
%
% Succeeds after retracting all asserted coverage/2 terms.
stop :-
    retractall(coverage(_,_)).