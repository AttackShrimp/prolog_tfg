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
% Succeeds after asserting a 'coverage' term to the count in the knowledge base.
%
% @param Number      The ID of the predicate.
% @param Name        The Functor/Arity of the predicate head.
pred_start(Number, Functor / Arity) :-
    add_to_asserted(coverage, Number - Functor / Arity).

%% pred_ground(+Predicate : Term).
%
% Succeeds after asserting a 'grounds' term to the count in the knowledge base.
%
% @param Predicate   The head of clause that has matched with a query.
pred_ground(Predicate) :-
    get_ground_indicators(Predicate, Ground_indicators), 
    add_to_asserted(grounds, Ground_indicators).

%% get_ground_indicators(+Predicate : Term, -Predicate_with_indicators : Term).
%
% Succeeds after substituting arguments in a term in the form <Name>(arg1,...) 
% with an indicator of whether they are grounded.
%
% The output will show g if the argument is grounded and ng if it is not.
%
% @param Predicate   The head of clause that has matched with a query.
% @param Predicate   The head of clause with 'g' and 'ng' as arguments.
get_ground_indicators(Predicate, Pred_with_indicators) :-
    functor(Predicate, _, Arity), 
    Predicate =.. [Head | Args], 
    length(Indicators, Arity), 
    maplist(detect_ground, Args, Indicators), 
    Pred_with_indicators =.. [Head | Indicators].

detect_ground(P, g) :- ground(P), !.
detect_ground(_, ng).

%% pred_branch(+Branch_list : list, +Predicate : Term).
%
% Succeeds after asserting a 'branches' term to the count in the knowledge base.
% The first argument of this term is a list indicating if the clause number 
% corresponding to the list index matches with the <Predicate> term with a 1 if 
% there is a match and a 0 if not.
%
% @param Branch_list    A list of clauses for a given predicate
% @param Predicate      The query term to test for unification.
pred_branch(Branch_list, Predicate) :-
    maplist(unifies(Predicate), Branch_list, Branches),
    functor(Predicate, Functor, Arity), 
    add_to_asserted(branches, Functor / Arity - Branches).

unifies(Predicate, Branch, 1) :- unifiable(Predicate, Branch, _), !.
unifies(_, _, 0).

%% add_to_asserted(+Name : atom, +Content : term).
%
% Succeeds after asserting a term to the knowledge base, with a counter 
% storing the times the predicate has been called with the same parameters.
%
% If this specific parameter combination has already been asserted, adds
% 1 to the counter storing it.
%
% @param Name        The Functor of a predicate.
% @param Content     The first argument of the predicate.
add_to_asserted(Name, Content) :-
    Predicate =.. [Name, Content, Count],
    call(Predicate),
    !,
    Next is Count + 1,
    Replacement =.. [Name, Content, Next],
    retract(Predicate),
    assertz(Replacement).
add_to_asserted(Name, Content) :-
    Predicate =.. [Name, Content, 1],
    assertz(Predicate).
    

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