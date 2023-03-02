:- module(instrumenter, [instrument/1]).
/** <module> instrumenter

This module instruments a .pl file, adding function calls to build a basis for
code coverage tools to use.

@author Daniel Santamarina

*/

%% instrument(+File : string).
%
% Succeeds after instrumenting and running consult over a prolog code file.
%
% @param File The name of the file to consult.
instrument(File) :-
   call_over_file(File, read_terms, read, Terms),
   insert_logger_calls(Terms, Logged_terms),
   call_over_file('temp.pl', write_terms, write, Logged_terms),
   !,
   [temp]. % We could close the file here, it is transparent to the user

%% call_over_file(+File : string, +Functor: atom,  +Mode: atom, -Result: term).
%
% Succeeds after calling a compound term with the stream of a file, obtaining
% a result
%
% @param File     The name of the file to generate the stream.
% @param Functor  The functor of the compound term to call as
%                 <Functor>(<File_Stream>, <Result>).
% @param Mode     One of read, write, append or update.
% @param Result   Result of the compound term call.
call_over_file(File, Functor, Mode, Result) :-
   open(File, Mode, FStream),
   Predicate =.. [Functor, FStream, Result],
   call(Predicate),
   close(FStream). 

%% read_terms(+Stream : stream, -Terms: list).
%
% Succeeds after reading all lines of a file into a list
%
% @param Stream   The stream to read from
% @param Terms    A list of terms resulting from calling read_term/3 over
%                 every line
read_terms(Stream, Terms) :-
   read_term(Stream, Term, []),
   process_term(Term, Stream, Terms).

process_term(end_of_file, _Stream, []).
process_term(Term, Stream, [Term | Tail]) :-
   read_terms(Stream, Tail).

%% insert_logger_calls(+Terms : list, -Logged_terms: list).
%
% Succeeds after adding a call to the logger to every term in a list.
%
% @param Terms          The list of terms to be modified.
% @param Logged_terms   The resulting list after modifications.
insert_logger_calls(Terms, Logged_terms) :-
   univ_to(Terms, Univ_terms),
   log_terms(Univ_terms, Logged_univ_terms, 0),
   univ_to(Logged_terms, Logged_univ_terms).

%% univ_to(+Input : list, -Output: list).
%
% Succeeds after calling univ/1 over every term in a list.
%
% @param Input    The list of terms.
% @param Output   A list of lists resulting from calling univ over every element
univ_to([],[]).
univ_to([Term | ITail], [Univ_term | OTail]) :-
   Term =.. Univ_term,
   univ_to(ITail, OTail).

%% log_terms(+Input : list, -Output: list, +Counter: integer).
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
%
% @param Input       The list of terms to be modified.
% @param Output      The resulting list after modifications.
% @param Counter     The counter representing the term number
log_terms([],[], _Counter).
log_terms(
      [[:-, Head, Body]                                        | ITail], 
      [[:-, Head, (pred_start(Counter, Functor/Arity), Body)]  | OTail],
      Counter) :-
   !,
   functor(Head, Functor, Arity),
   Next is Counter + 1,
   log_terms(ITail, OTail, Next).
log_terms(
      [[-->, Head, Body]                                        | ITail], 
      [[-->, Head, ({pred_start(Counter, Functor/Arity)},Body)] | OTail],
      Counter) :-
   !,
   functor(Head, Functor, Arity_DCG),
   Arity is Arity_DCG + 2,
   Next is Counter + 1,
   log_terms(ITail, OTail, Next).
log_terms(
      [[Functor | Args]                                 | ITail], 
      [[:-, Head, pred_start(Counter, Functor / Arity)] | OTail],
      Counter) :-
   findall(Op, current_op(_,_,Op), Operands),
   not(member(Functor, Operands)),
   !,
   Head =.. [Functor | Args],
   functor(Head, Functor, Arity),
   Next is Counter + 1,
   log_terms(ITail, OTail, Next).
log_terms([Unrecognized | ITail], [Unrecognized | OTail], Counter) :-
   log_terms(ITail, OTail, Counter).

%% write_terms(+Stream : stream, +Terms: list).
%
% Succeeds after writing all terms in <Terms> into the stream.
% 
% After write_terms, the file is prolog-valid and can be loaded with consult.
%
% @param Stream   The stream to write to.
% @param Terms    A list of terms resulting to write to the file.
write_terms(_Stream, []).
write_terms(Stream, [Term | Tail]) :-
   writeq(Stream, Term),
   write(Stream, '.'),
   nl(Stream),
   write_terms(Stream, Tail).
