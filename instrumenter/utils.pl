:- module(utils, [rearrange/4, univ_to/2]).
/** <module> utils

This module contains general-purpose utility functions used by multiple
modules.

@author Daniel Santamarina

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