:- module(instrumenter, [instrument/3]).
/** <module> instrumenter

This module instruments a .pl file, adding function calls to build a basis for
code coverage tools to use.

When additional options are given, the <launcher> module is called to append
additional instrumentation.

*/
:- use_module(utils, [univ_to/2, call_over_file/4]).
:- use_module(launcher, [configure_launcher/4]).

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
instrument(File, Term_signatures, Options) :-
   utils:call_over_file(File, read_terms, read, Terms),
   insert_logger_calls(Terms, Logged_terms, Term_signatures),
   configure_launcher(Logged_terms, Term_signatures, Options, Logged_launcher_terms),
   utils:call_over_file('.temp.pl', write_terms, write, Logged_launcher_terms).

%% insert_logger_calls(+Terms : list, -Logged_terms: list, -Term_names: list).
%
% Succeeds after adding a call to the logger to every term in a list.
% Every modified term is saved as a signature in the <Term_names> list.
%
% @param Terms          The list of terms to be modified.
% @param Logged_terms   The resulting list after modifications.
% @param Term_names  The list of term signatures in form <ID>-<Functor>/<Arity>.
insert_logger_calls(Terms, Logged_terms, Term_names) :-
   utils:univ_to(Terms, Univ_terms),
   log_terms(Univ_terms, Logged_univ_terms, 0, Term_names),
   utils:univ_to(Logged_terms, Logged_univ_terms).

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
log_terms([],[], _Counter, []).
log_terms(
      [[:-, Head, Body]                                               | ITail], 
      [[:-, Head, (logger:pred_start(Counter, Functor/Arity), Body)]  | OTail],
      Counter,
      [Counter - Functor / Arity                                      | TTail]) :-
   !,
   functor(Head, Functor, Arity),
   Next is Counter + 1,
   log_terms(ITail, OTail, Next, TTail).
log_terms(
      [[-->, Head, Body]                                               | ITail], 
      [[-->, Head, ({logger:pred_start(Counter, Functor/Arity)},Body)] | OTail],
      Counter,
      [Counter - Functor / Arity                                       | TTail]) :-
   !,
   functor(Head, Functor, Arity_DCG),
   Arity is Arity_DCG + 2,
   Next is Counter + 1,
   log_terms(ITail, OTail, Next, TTail).
log_terms(
      [[Functor | Args]                                        | ITail], 
      [[:-, Head, logger:pred_start(Counter, Functor / Arity)] | OTail],
      Counter,
      [Counter - Functor / Arity                               | TTail]) :-
   findall(Op, current_op(_,_,Op), Operands),
   not(member(Functor, Operands)),
   !,
   Head =.. [Functor | Args],
   functor(Head, Functor, Arity),
   Next is Counter + 1,
   log_terms(ITail, OTail, Next, TTail).
log_terms([Unrecognized | ITail], [Unrecognized | OTail], Counter, Term_names) :-
   log_terms(ITail, OTail, Counter, Term_names).