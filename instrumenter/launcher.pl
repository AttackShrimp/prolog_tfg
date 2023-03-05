:- module(launcher, [configure_launcher/4]).
/** <module> launcher

This module adds launcher code to an terms that have been processed by the 
instrumenter, adding function calls that allow processing of more fine-grained
details by the coverage tools.

@author Daniel Santamarina

*/
:- use_module(utils, [rearrange/4, univ_to/2]).

%% configure_launcher(+Terms : list, +Term_signatures: list, +Options: list, -Launcher_terms: list).
%
% Succeeds after adding Launcher configuration predicates to a list of 
% prolog terms.
%
% When given the option [cmd], the launcher will be active. Otherwise, the
% terms will remain the same and no launcher instrumentation will be added.
%
% @param Terms             The list of terms of terms to process.
% @param Term_signatures   The list of signatures for all terms in the 
%                          form <ID>-<Functor>/<Arity>.
% @param Options           The list of options for the launcher, either [cmd]
%                          or [].
% @param Launcher_terms    The original Terms with the launcher instrumentation
%                          added at the start of the list.
configure_launcher(Terms, Term_signatures, Options, Launcher_terms) :-
    member(cmd, Options),
    !,
    create_launcher_predicates(Term_signatures, Launcher_predicates),
    mark_terms(Terms, Terms_renamed),
    append(Launcher_predicates, Terms_renamed, Launcher_terms).
 configure_launcher(Logged_terms,_,_, Logged_terms).
 
%% create_launcher_predicates(+Signatures : list, -Launchers: list).
%
% Succeeds after creating launcher predicates for every distinct signature
% in <Signatures>.
% 
% Signatures are understood to contain duplicates, and are therefore sorted to
% remove any.
%
% @param Signatures   The list of predicate signatures in the form 
%                     <Counter> - <Functor> / <Arity>.
% @param Launchers    The generated list of launcher predicates.
 create_launcher_predicates(Signatures, Launchers) :-
    utils:rearrange(Signatures, _Counter - Functor / Arity, 
       Functor / Arity, Names),
    sort(Names, Names_unique),
    names_to_launchers(Names_unique, Launchers).
 
%% names_to_launchers(+Names : list, -Launchers: list).
%
% Succeeds after creating launcher predicates for every name in <Names>.
% 
% Three launcher predicates are created for every name, each represent a
% different behaviour:
%   - First:  When the predicate is not the initial query and simply redirects
%             to the original.
%   - Second: When the predicate is the initial query and succeeds when 
%             redirecting.
%   - Third:  When the predicate is the initial query and fails when 
%             redirecting.
%
% @param Signatures   The list of predicate names in the form 
%                     <Functor> / <Arity>.
% @param Launchers    The list of launchers, 3x the original in size.
names_to_launchers([],[]).
names_to_launchers([Functor/Arity | NTail], [First, Second, Third | LTail]) :-
    functor(Launcher, Functor, Arity),
    add_instrumenter_mark(Launcher, Predicate_call),
    First = (
        Launcher :- 
            loader:not_first, 
            !, 
            Predicate_call),
    Second = (
        Launcher :- 
            assertz(loader:not_first), 
            Predicate_call, 
            !, 
            loader:get_coverage_and_clear),
    Third = (
        Launcher :- 
            loader:get_coverage_and_clear,
            fail),
    names_to_launchers(NTail, LTail).

%% mark_terms(+Terms : list, -Marked_terms: list).
%
% Succeeds after marking the head of each term in the list to 
% indicate they belong to the instrumenter.
%
% @param Terms          The list of terms to be modified.
% @param Marked_terms   The resulting list after modifications.
mark_terms(Terms, Marked_terms) :-
    utils:univ_to(Terms, Univ_terms),
    rename_heads(Univ_terms, Univ_terms_marked),
    utils:univ_to(Marked_terms, Univ_terms_marked).

%% rename_heads(+Input : list, -Output: list).
%
% Succeeds after renaming the head of every term in the list.
% 
% Terms that are complex but not predicates are recognized and renamed in the
% same manner.
%
% @param Input        The list of terms to be modified.
% @param Output       The resulting list after modifications.
rename_heads([],[]).
rename_heads([[Operand, Head, Body] | ITail], [[Operand, Head_renamed, Body]| OTail]) :-
    member(Operand, [:-, -->]),
    add_instrumenter_mark(Head, Head_renamed),
    rename_heads(ITail, OTail).
rename_heads([[Functor | Args] | ITail], [[Functor_renamed | Args] | OTail]) :-
    findall(Op, current_op(_,_,Op), Operands),
    not(member(Functor, Operands)),
    !,
    add_instrumenter_mark(Functor, Functor_renamed),
    rename_heads(ITail, OTail).
rename_heads([Unrecognized | ITail], [Unrecognized | OTail]) :-
    rename_heads(ITail, OTail).

%% add_instrumenter_mark(+Term : term, -Term_renamed: term).
%
% Succeeds after renaming the functor of a term, adding '_i' to indicate it
% belongs to the instrumenter.
% 
% Terms that are atoms are similarly renamed.
%
% @param Term           The term to be modified.
% @param Term_renamed   The <Term> with a '_i' added.
add_instrumenter_mark(Term, Term_renamed) :-
    Term =.. [Functor | Args],
    atom_concat(Functor, "_i", Functor_renamed),
    Term_renamed =.. [Functor_renamed | Args].