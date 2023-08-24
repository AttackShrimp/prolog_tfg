:- module(instrumenter, [instrument/3, process_term/4, log_term/7]).
/** <module> instrumenter

This module instruments a .pl file, adding function calls to build a basis for
code coverage tools to use.

When additional options are given, the <launcher> module is called to append
additional instrumentation.

*/
:- use_module(utils, [univ_to/2, call_over_file/4, rearrange/4, maplist_nth/2, zip/6]).
:- use_module(launcher, [insert_launcher_calls/5]).

%% instrument(+File : string, -Term_signatures: list, +Options: list).
%
% Succeeds after instrumenting a prolog code file.
%
% This predicated makes a call to the launcher module, sending the specified
% options. This module will append it's own coverage predicates the the existing
% instrumenter ones.
%
% @param File              The name of the file to consult.
% @param Term_signatures   The list of signatures for all instrumented
%                          terms in the form <ID>-<Functor>/<Arity>.
% @param Options           The option list.
instrument(File, Predicates, Options) :-
   utils:call_over_file(File, read_terms, read, Terms),

   maplist(process_term, Terms, Types, IDs, Structures),

   reduce_count(IDs, Predicates),

   unfold(Predicates, Terms_clauses),

   insert_logger_calls(Types, Terms_clauses, Structures, Logged_Types, Logged_structures),

   utils:zip(IDs, Logged_Types, ID_1, Type_1, ID_1-Type_1, ID_and_Type),

   list_to_set(ID_and_Type, ID_and_Type_Predicates),

   utils:zip(Logged_Pred_IDs, Logged_Pred_Types, ID_2, Type_2, ID_2-Type_2, ID_and_Type_Predicates),
   
   launcher:insert_launcher_calls(Options, Logged_Pred_Types, Logged_Pred_IDs, Logged_Types-IDs-Logged_structures, Launcher_terms),

   utils:call_over_file('.temp.pl', write_terms, write, Launcher_terms).


reduce_count(L1, L2) :- reduce_count(L1, L2, 1).
reduce_count([H, H | T], Red, Count) :-
    !,
    Next is Count + 1,
    reduce_count([H | T], Red, Next).
reduce_count([H | T], [H - Count | Red], Count) :-
    reduce_count(T, Red, 1).
reduce_count([], [], _).

unfold(Input, Output) :-
   maplist(unfold_term, Input, Output_2D),
   flatten(Output_2D, Output).

unfold_term(ID-Count, Output) :-
   numlist(1, Count, Increments),
   utils:rearrange(Increments,
      Number,
      ID - Number,
      Output).

process_term(Predicate, fact, Name / Arity, Predicate - true) :-
   functor(Predicate, Name, Arity),
   not(current_op(_,_,Name)),
   !. % Y
process_term(Predicate, rule, Name / Arity, Head - Body) :-
   functor(Predicate, :-, 2), 
   !, % Y
   arg(1, Predicate, Head),
   arg(2, Predicate, Body),
   functor(Head, Name, Arity).
process_term(Predicate, dcg, Name / Arity, Head - Body) :-
   functor(Predicate, -->, 2),
   !, % Y
   arg(1, Predicate, Head),
   arg(2, Predicate, Body),
   functor(Head, Name, Arity).
process_term(Predicate, ignore, _, Predicate - fail).

%% insert_logger_calls(+Terms : list, -Logged_terms: list, -Term_names: list).
%
% Succeeds after adding a call to the logger to every term in a list.
% Every modified term is saved as a signature in the <Term_names> list.
%
% @param Terms          The list of terms to be modified.
% @param Logged_terms   The resulting list after modifications.
% @param Term_names  The list of term signatures in form <ID>-<Functor>/<Arity>.
insert_logger_calls(Types, Terms_clauses, Structures, Log_types, Log_structures) :-
   maplist(log_predicate, Terms_clauses, Coverage_predicates),
   utils:maplist_nth(instrumenter:log_term(front), [Types, Coverage_predicates, Structures, Log_types, _, Log_structures]).

%% log_terms(+Input : list, -Output: list, +Counter: integer, -Term_names: list).
%
% Succeeds after adding a pred_start/2 call to every term in a list.
% 
% 3 different types of terms are recognized:
%     - Predicates - <head> :- <body>  becomes <head> :- pred_start, <body>
%     - DCG's      - <head> --> <body> becomes <head> --> {pred_start}, <body>
%     - terms      - <term>            becomes <term> :- pred_start
% Terms who's functor is an operator (excluding :- and -->) are not modified.
% pred_start/2 is given 2 arguments, a counter representing the term number and
% a term Functor/Arity as a signature.
% Every modified term is saved as a signature in the <Term_names> list.
%
% @param Input       The list of terms to be modified.
% @param Output      The resulting list after modifications.
% @param Counter     The counter representing the term number.
% @param Term_names  The list of term signatures in form <ID>-<Functor>/<Arity>.
log_predicate(Functor / Arity - Clause, logger:coverage(clause, Functor / Arity, Clause)).

log_term(_, fact, Cov_pred, Head - _, rule, 
   (Head :- Cov_pred), Head - Cov_pred).

log_term(front, rule, Cov_pred, Head - Body, rule,
   (Head :- Cov_pred, Body), Head - (Cov_pred, Body)).

log_term(back, rule, Cov_pred, Head - Body, rule, 
   (Head :- Body, Cov_pred), Head - (Cov_pred, Body)).

log_term(both, rule, Cov_pred_front / Cov_pred_back, Head - Body, rule, 
   (Head :- Cov_pred_front, Body, Cov_pred_back), 
   Head - (Cov_pred_front, Body, Cov_pred_back)).

log_term(front, dcg, Cov_pred, Head - Body, dcg,
   (Head --> {Cov_pred}, Body), Head - ({Cov_pred}, Body)).

log_term(back, dcg, Cov_pred, Head - Body, dcg,
   (Head --> Body, {Cov_pred}), Head - ({Cov_pred}, Body)).

log_term(both, dcg, Cov_pred_front / Cov_pred_back, Head - Body, dcg,
   (Head --> {Cov_pred_front}, Body, {Cov_pred_back}), 
   Head - ({Cov_pred_front}, Body, {Cov_pred_back})).

log_term(_, ignore, _, Predicate - _, _, Predicate, Predicate - _).