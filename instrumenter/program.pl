:- use_module(instrumenter, [instrument/2]).
:- use_module(logger, [init/1, pred_start/2, coverage/1, clean/0]).
:- use_module(visualizer, [visualize/1]).

start :-
    instrumenter : instrument('test.pl', Term_sigs),
    logger : init(Term_sigs).

measure :-
    logger : coverage(Terms),
    visualizer : visualize(Terms).

stop :-
    logger : clean.