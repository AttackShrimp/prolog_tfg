:- module(utils, [rearrange/4, univ_to/2, call_over_file/4]).
/** <module> utils

This module contains general-purpose utility functions used by multiple
modules.

*/

%% rearrange(+Input : list, +Original : term, +Rearranged : term, -Output : list).
%
% Succeeds after matching <Input> element-wise with <Original>, giving the 
% bounded <Rearranged> values for the corresponding element in Output.
%
% For every element, <Original> and <Rearranged> are copied as fresh variables
% retaining the bingings between each other. This way each element in <Input> is
% matched with a fresh <Original> term to generate a <Rearranged> term.
%
% @param Input          The list of elements matching with Original
% @param Original       The term matching with each element in Input and bounded 
%                       to <Rearranged>.
% @param Rearranged     The term bounded to Original.
% @param Output         The list of each bounded <Rearranged>
rearrange([], _Original, _Rearranged, []).
rearrange([IElement | ITail], Original, Rearranged, [OElement | OTail]) :-
    copy_term((Original, Rearranged), (Fresh_original, Fresh_rearranged)),
    rearrange(ITail, Fresh_original, Fresh_rearranged, OTail),
    IElement = Original,
    OElement = Rearranged.

%% univ_to(+Input : list, -Output: list).
%
% Succeeds after calling univ/1 over every term in a list.
%
% @param Input    The list of terms.
% @param Output   A list of lists resulting from calling univ over every element.
univ_to([],[]).
univ_to([Term | ITail], [Univ_term | OTail]) :-
   Term =.. Univ_term,
   univ_to(ITail, OTail).

%% call_over_file(+File : string, +Functor: atom,  +Mode: atom, -Result: term).
%
% Succeeds after calling a compound term with the stream of a file, obtaining
% a result.
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
 % Succeeds after reading all lines of a file into a list of terms.
 %
 % @param Stream   The stream to read from.
 % @param Terms    A list of terms resulting from calling read_term/3 over
 %                 every line.
 read_terms(Stream, Terms) :-
    read_term(Stream, Term, []),
    process_term(Term, Stream, Terms).
 
 process_term(end_of_file, _Stream, []).
 process_term(Term, Stream, [Term | Tail]) :-
    read_terms(Stream, Tail).

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
