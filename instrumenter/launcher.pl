:- module(launcher, [configure_launcher/4]).
/** <module> launcher

This module adds launcher code to an terms that have been processed by the 
instrumenter, adding function calls that allow processing of more fine-grained
details by the coverage tools.

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
configure_launcher(Terms, Term_signatures, Options, Terms_configured) :-
    get_predicate_names(Term_signatures, Names),
    phrase(configure_launcher_options(Terms-Term_signatures, Names, Options, Max_indents), [[]], [Launcher_terms]),
    mark_terms(Terms, Indented_terms, Max_indents),
    append(Indented_terms, Launcher_terms, Terms_configured).

configure_launcher_options(T-TS, Names, Options, Indent) -->
    {absorb_element(branch, Options, Other_options)},
    configure_launcher_options(T-TS, Names, Other_options, Next_indent),
    state(Initial, Final),
    {
        Indent is Next_indent + 1,
        names_to_branches(T-TS, Names, Branch_predicates, Indent),
        append(Initial, Branch_predicates, Final)
    }.
configure_launcher_options(T-TS, Names, Options, Indent) -->
    {absorb_element(ground, Options, Other_options)},
    configure_launcher_options(T-TS, Names, Other_options, Next_indent),
    state(Initial, Final),
    {
        Indent is Next_indent + 1,
        names_to_grounds(Names, Ground_predicates, Indent),
        append(Initial, Ground_predicates, Final)
    }.
configure_launcher_options(T-TS, Names, Options, Indent) -->
    {absorb_element(cmd, Options, Other_options)},
    configure_launcher_options(T-TS, Names, Other_options, Next_indent),
    state(Initial, Final),
    {
        Indent is Next_indent + 1,
        names_to_launchers(Names, Launcher_predicates, Indent),
        append(Initial, Launcher_predicates, Final)
    }.
configure_launcher_options(_, _, [], 0) --> state(Logged_terms, Logged_terms).
 
absorb_element(Elem, List, New_list) :-
    append(L, [Elem | R], List),
    !,
    append(L, R, New_list).

state(S0),     [S0] --> [S0].
state(S0, S1), [S1] --> [S0].

get_predicate_names(Signatures, Names_unique) :-
    utils:rearrange(Signatures, _Counter - Functor / Arity, 
       Functor / Arity, Names),
       sort(Names, Names_unique).

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
 create_launcher_predicates(Signatures, Launchers, Indent) :-
    utils:rearrange(Signatures, _Counter - Functor / Arity, 
       Functor / Arity, Names),
    sort(Names, Names_unique),
    names_to_launchers(Names_unique, Launchers, Indent).
 
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
names_to_launchers([],[],_).
names_to_launchers([Functor/Arity | NTail], [First, Second, Third | LTail], Indent) :-
    functor(Predicate, Functor, Arity),
    add_instrumenter_mark(Predicate, Predicate_call, Indent),
    Past_indent is Indent - 1,
    add_instrumenter_mark(Predicate, Launcher, Past_indent),
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
    names_to_launchers(NTail, LTail, Indent).

names_to_grounds([],[],_).
names_to_grounds([Functor/Arity | NTail], [First | LTail], Indent) :-
    functor(Predicate, Functor, Arity),
    add_instrumenter_mark(Predicate, Predicate_call, Indent),
    Past_indent is Indent - 1,
    add_instrumenter_mark(Predicate, Launcher, Past_indent),
    First = (
        Launcher :- 
            logger:pred_ground(Predicate),
            Predicate_call),
    names_to_grounds(NTail, LTail, Indent).

names_to_branches(_, [],[],_).
names_to_branches(T-TS, [Functor/Arity | NTail], [First | LTail], Indent) :-
    functor(Predicate, Functor, Arity),
    add_instrumenter_mark(Predicate, Predicate_call, Indent),
    Past_indent is Indent - 1,
    add_instrumenter_mark(Predicate, Launcher, Past_indent),
    maplist(extract_heads(Functor/Arity), TS, T, Filtered_heads),
    exclude(=(none), Filtered_heads, Heads),
    First = (
        Launcher :- 
            logger:pred_branch(Heads, Predicate),
            Predicate_call),
    names_to_branches(T-TS, NTail, LTail, Indent).

extract_heads(Functor / Arity, _Counter - Functor / Arity, T, Head) :-
    T =.. Univ_term,
    Univ_term = [Operand, Head, _Body],
    member(Operand, [:-, -->]).
extract_heads(Functor / Arity, _Counter - Functor / Arity, T, T) :-
    T =.. Univ_term,
    Univ_term = [Functor, _Arguments],
    findall(Op, current_op(_,_,Op), Operands),
    not(member(Functor, Operands)),
    !.
extract_heads(Functor / Arity, _Counter - Functor / Arity, T, T).
extract_heads(_, _, _, none).


%% mark_terms(+Terms : list, -Marked_terms: list).
%
% Succeeds after marking the head of each term in the list to 
% indicate they belong to the instrumenter.
%
% @param Terms          The list of terms to be modified.
% @param Marked_terms   The resulting list after modifications.
mark_terms(Terms, Marked_terms, Indent) :-
    utils:univ_to(Terms, Univ_terms),
    rename_heads(Univ_terms, Univ_terms_marked, Indent),
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
rename_heads([],[], _).
rename_heads([[Operand, Head, Body] | ITail], [[Operand, Head_renamed, Body]| OTail], Indent) :-
    member(Operand, [:-, -->]),
    add_instrumenter_mark(Head, Head_renamed, Indent),
    rename_heads(ITail, OTail, Indent).
rename_heads([[Functor | Args] | ITail], [[Functor_renamed | Args] | OTail], Indent) :-
    findall(Op, current_op(_,_,Op), Operands),
    not(member(Functor, Operands)),
    !,
    add_instrumenter_mark(Functor, Functor_renamed, Indent),
    rename_heads(ITail, OTail, Indent).
rename_heads([Unrecognized | ITail], [Unrecognized | OTail], Indent) :-
    rename_heads(ITail, OTail, Indent).

%% add_instrumenter_mark(+Term : term, -Term_renamed: term).
%
% Succeeds after renaming the functor of a term, adding '_i' to indicate it
% belongs to the instrumenter.
% 
% Terms that are atoms are similarly renamed.
%
% @param Term           The term to be modified.
% @param Term_renamed   The <Term> with a '_i' added.
add_instrumenter_mark(Term, Term_renamed, Indent) :-
    Term =.. [Functor | Args],
    length(Mark_list, Indent), 
    maplist(=('_i'),Mark_list), 
    Concat_list = [Functor | Mark_list], 
    atomic_list_concat(Concat_list, Functor_renamed),
    Term_renamed =.. [Functor_renamed | Args].