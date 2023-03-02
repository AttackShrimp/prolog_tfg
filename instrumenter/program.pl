:- use_module(instrumenter, [instrument/2]).
:- use_module(logger, [init/1, pred_start/2, coverage/1, clean/0]).
:- use_module(visualizer, [get_percentages/3]).

start :-
    instrumenter : instrument('test.pl', Term_sigs),
    logger : init(Term_sigs).

measure(G_p, L_p) :-
    coverage(Terms),
    get_percentages(Terms, G_p, L_p).

stop :-
    logger : clean.