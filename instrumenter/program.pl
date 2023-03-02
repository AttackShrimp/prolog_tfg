:- use_module(instrumenter, [instrument/2]).
:- use_module(logger, [init/1, pred_start/2, clean/0]).

start :-
    instrumenter : instrument('test.pl', Term_sigs),
    logger : init(Term_sigs).

stop :-
    logger : clean.