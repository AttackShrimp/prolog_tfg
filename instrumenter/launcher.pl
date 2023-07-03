:- module(launcher, [configure_launcher/4]).
/** <module> launcher

This module adds launcher code to an terms that have been processed by the 
instrumenter, adding function calls that allow processing of more fine-grained
details by the coverage tools.

*/
:- use_module(utils, [rearrange/4, univ_to/2]).

%% configure_launcher(+Terms : list, +Term_signatures: list, +Options: list, -Launcher_terms: list).
%
% Succeeds after adding additional configuration predicates to a list of 
% prolog terms. These predicates can have multiple layers to measure different
% things and eventually finish by calling the original terms.
%
% There are three possible configuration layers to add, each is selected using a
% keyword option:
%  - [cmd]:     The laucher option adds an initial layer allowing the coverage  
%               to be measured directly from the prolog terminal, by simply  
%               typing a term in the same way as if there was no code coverage 
%               running.
%  - [ground]:  The ground layer measures the ground terms in each predicate 
%               before the execution is complete. This gives an indication of 
%               what predicate parameters are in/out.
%  - [branch]:  The branch layer measures the possible coverage for each clause
%               branch in a predicate call, independently of whether or not the 
%               clause ends up being chosen for execution.
%
% Due to the structure of the added layers, the [cmd] layer must always be the 
% first layer in the execution if present, and therefore must be the last to be 
% added.
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
    cmd_last(Options, Options_reordered),
    phrase(instrument_with_option(Terms-Term_signatures, Names, Options_reordered, Max_indents), [[]], [Launcher_terms]),
    mark_terms(Terms, Indented_terms, Max_indents),
    append(Indented_terms, Launcher_terms, Terms_configured).

cmd_last(Options, Options_reordered) :-
    append(A, [cmd | B], Options),
    !,
    append([A, B, [cmd]], Options_reordered).
cmd_last(Options, Options).

%% instrument_with_option(+Terms_and_signatures : term, +Names : list, +Options: list, +Depth: int).
%
% Succeeds after instrumenting every predicate following the options in 
% <Options>. For each option a new set of predicates linked to the next depth.
% The original predicates are not considered, only adding predicates to measure 
% additional parameters.
%
% @param Terms_and_signatures   A <list>-<list> representing all predicate 
%                               clauses in the program and their signatures.
% @param Signatures             The list of predicate names in the form 
%                               <Functor> / <Arity>.
% @param Options                List of user-defined options describing what 
%                               terms to add.
% @param Depth                  The current instrumenter depth.
instrument_with_option(T-TS, Names, [Option | Options], Depth) -->
    instrument_with_option(T-TS, Names, Options, Next_depth),
    state(Initial, Final),
    {
        Depth is Next_depth + 1,
        names_to(T-TS, Names, Option, Depth, Instrumented_terms),
        append(Initial, Instrumented_terms, Final)
    }.
instrument_with_option(_, _, [], 0) --> state(Logged_terms, Logged_terms).

state(S0),     [S0] --> [S0].
state(S0, S1), [S1] --> [S0].

%% create_launcher_predicates(+Signatures : list, -Names_unique: list).
%
% Succeeds after converting <Signatures> of all clauses into a list of predicate
% names.
% 
% Due to multiple clauses per predicate, clause names are sorted to remove 
% duplicates.
%
% @param Signatures   The list of predicate clause signatures in the form 
%                     <Counter> - <Functor> / <Arity>.
% @param Names_unique The list of predicate names in the form 
%                     <Functor> / <Arity>.
get_predicate_names(Signatures, Names_unique) :-
    utils:rearrange(Signatures, _Counter - Functor / Arity, 
       Functor / Arity, Names),
       sort(Names, Names_unique).
 
%% names_to(+Terms_and_signatures : term, +Names : list, +Option: term, +Depth: int, -Instrumented_terms: list).
%
% Succeeds after instrumenting every name in <Names> according to a given 
% option.
%
% @param Terms_and_signatures   A <list>-<list> representing all predicate 
%                               clauses in the program and their signatures.
% @param Signatures             The list of predicate names in the form 
%                               <Functor> / <Arity>.
% @param Option                 User-defined option describing what to instrument
% @param Depth                  The current instrumenter depth.
% @param Instrumented_terms     The list of instrumented terms.
names_to(_-_, Names, cmd, Depth, Instrumented_terms) :-
    names_to_launchers(Names, Instrumented_terms, Depth).
names_to(_-_, Names, ground, Depth, Instrumented_terms) :-
    names_to_grounds(Names, Instrumented_terms, Depth).
names_to(Terms-Term_sigatures, Names, branch, Depth, Instrumented_terms) :-
    names_to_branches(Terms-Term_sigatures, Names, Instrumented_terms, Depth).
%% names_to_launchers(+Names : list, -Launchers: list, +Depth: int).
%
% Succeeds after creating launcher predicates for every name in <Names>.
% 
% Three launcher predicates are created for every name, each represent a
% different behaviour:
%   - First:  When the predicate is not the initial query and simply redirects
%             to the next instrumenter depth.
%   - Second: When the predicate is the initial query and succeeds when 
%             redirecting.
%   - Third:  When the predicate is the initial query and fails when 
%             redirecting.
%
% @param Signatures   The list of predicate names in the form 
%                     <Functor> / <Arity>.
% @param Launchers    The list of launchers, 3x the original in size.
% @param Depth        The current instrumenter depth.
names_to_launchers([],[],_).
names_to_launchers([Functor/Arity | NTail], [First, Second, Third | LTail], Depth) :-
    functor(Predicate, Functor, Arity),
    add_instrumenter_marks(Predicate, Predicate_call, Depth),
    Past_depth is Depth - 1,
    add_instrumenter_marks(Predicate, Launcher, Past_depth),
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
    names_to_launchers(NTail, LTail, Depth).

%% names_to_grounds(+Names : list, -Grounds: list, +Depth: int).
%
% Succeeds after creating a Ground predicate for every name in <Names>. Each
% Ground predicate will log the ground of the predicate and redirect to the 
% next instrumenter depth.
%
% @param Signatures   The list of predicate names in the form 
%                     <Functor> / <Arity>.
% @param Grounds      The list of Ground predicates.
% @param Depth        The current instrumenter depth.
names_to_grounds([],[],_).
names_to_grounds([Functor/Arity | NTail], [First | LTail], Depth) :-
    functor(Predicate, Functor, Arity),
    add_instrumenter_marks(Predicate, Predicate_call, Depth),
    Past_depth is Depth - 1,
    add_instrumenter_marks(Predicate, Launcher, Past_depth),
    First = (
        Launcher :- 
            logger:pred_ground(Predicate),
            Predicate_call),
    names_to_grounds(NTail, LTail, Depth).

%% names_to_branches(+Terms_and_signatures: term, +Names : list, -Branches: list, +Depth: int).
%
% Succeeds after creating a Branch predicate for every name in <Names>. Each
% Branch predicate will contain all possible clauses to unify for the predicate
% to test it's branching capacity.
%
% Similarly to other instrumenter functions, each Branch predicate redirects to
% the next instrumenter depth.
% 
% @param Terms_and_signatures   A <list>-<list> representing all predicate 
%                               clauses in the program and their signatures.
% @param Signatures             The list of predicate names in the form 
%                               <Functor> / <Arity>.
% @param Branches               The list of Branch predicates.
% @param Depth                  The current instrumenter depth.
names_to_branches(_, [],[],_).
names_to_branches(T-TS, [Functor/Arity | NTail], [First | LTail], Depth) :-
    functor(Predicate, Functor, Arity),
    add_instrumenter_marks(Predicate, Predicate_call, Depth),
    Past_depth is Depth - 1,
    add_instrumenter_marks(Predicate, Launcher, Past_depth),

    get_clause_heads(Functor / Arity, T-TS, Clause_heads),
    First = (
        Launcher :- 
            logger:pred_branch(Clause_heads, Predicate),
            Predicate_call),
    names_to_branches(T-TS, NTail, LTail, Depth).

%% get_clause_heads(+Signature : term, +Terms_and_signatures : term, -Clause_heads: list).
%
% Succeeds after finding all clause heads in the term list of 
% <Terms_and_signatures> that match in name with <Signature>. In other words,
% finds every clause in the program belonging to a predicate with name 
% <Signature>. 
% 
% @param Signature              The predicate name in the form <Functor> / 
%                               <Arity>.
% @param Terms_and_signatures   A <list>-<list> representing all predicate 
%                               clauses in the program and their signatures.
% @param Clause_heads           The list of clauses belonging to <Signature>.
get_clause_heads(
        FA, 
        [Term | Terms] - [_ - FA | Term_signatures], 
        [Head | Clause_heads]) :-
    Term =.. [_Operand, Head, _Body],
    !,
    get_clause_heads(FA, Terms - Term_signatures, Clause_heads).
get_clause_heads(_, []-[], []) :- !.
get_clause_heads(FA, [_ | T] - [_ | TS], Clause_heads) :-
    get_clause_heads(FA, T-TS, Clause_heads).

%% mark_terms(+Terms : list, -Marked_terms: list, +Depth: int).
%
% Succeeds after marking the head of each term in the list to 
% indicate they belong to the instrumenter.
%
% @param Terms          The list of terms to be modified.
% @param Marked_terms   The resulting list after modifications.
% @param Depth          The depth of the marks to add.
mark_terms(Terms, Marked_terms, Depth) :-
    utils:univ_to(Terms, Univ_terms),
    rename_heads(Univ_terms, Univ_terms_marked, Depth),
    utils:univ_to(Marked_terms, Univ_terms_marked).

%% rename_heads(+Input : list, -Output: list, +Depth: int).
%
% Succeeds after renaming the head of every term in the list.
% 
% Terms that are complex but not predicates are recognized and renamed in the
% same manner.
%
% @param Input        The list of terms to be modified.
% @param Output       The resulting list after modifications.
% @param Depth        The depth of the renaming.    
rename_heads([],[], _).
rename_heads([[Operand, Head, Body] | ITail], [[Operand, Head_renamed, Body]| OTail], Depth) :-
    member(Operand, [:-, -->]),
    add_instrumenter_marks(Head, Head_renamed, Depth),
    rename_heads(ITail, OTail, Depth).
rename_heads([[Functor | Args] | ITail], [[Functor_renamed | Args] | OTail], Depth) :-
    findall(Op, current_op(_,_,Op), Operands),
    not(member(Functor, Operands)),
    !,
    add_instrumenter_marks(Functor, Functor_renamed, Depth),
    rename_heads(ITail, OTail, Depth).
rename_heads([Unrecognized | ITail], [Unrecognized | OTail], Depth) :-
    rename_heads(ITail, OTail, Depth).

%% add_instrumenter_marks(+Term : term, -Term_renamed: term, +Amount: int).
%
% Succeeds after renaming the functor of <Term>, adding <Amount> amount of 
% '_i' instrumenter indication marks after the functor name.
% 
% Terms that are atoms are similarly renamed.
%
% @param Term           The term to be modified.
% @param Term_renamed   The <Term> with <Amount> '_i's added.
% @param Amount         The amount of '_i's to add.
add_instrumenter_marks(Term, Term_renamed, Amount) :-
    Term =.. [Functor | Args],
    length(Mark_list, Amount), 
    maplist(=('_i'),Mark_list), 
    Concat_list = [Functor | Mark_list], 
    atomic_list_concat(Concat_list, Functor_renamed),
    Term_renamed =.. [Functor_renamed | Args].