:- use_module(instrumenter, [instrument/3]).
:- use_module(logger, [init/1, pred_start/2, coverage/1, clean/0, stop/0]).
:- use_module(visualizer, [visualize/1]).

%% TODO:
% rename this to something on the lines of "executor" and refactor it into dealing with execution aspects, 
% move program to a new file that imports only the executor
% refactor launcher:configure_launcher
% check instrumenter documentation
% general variable naming in launcher
% instrumenter:log_terms and launcher:rename_heads are similar in structure

:- dynamic not_first/0.

load(File) :-
    instrumenter : instrument(File, Term_sigs, [cmd]),
    logger : init(Term_sigs),
    !,
    [temp].

load(File, Commands) :-
    instrumenter : instrument(File, Term_sigs, Commands),
    logger : init(Term_sigs),
    !,
    [temp].

get_coverage_and_clear :-
    get_coverage,
    clear_logs.

get_coverage :-
    logger : coverage(Terms),
    visualizer : visualize(Terms).

clear_logs :- 
    retract(not_first),
    logger : clean.

unload :-
    (not_first -> retract(not_first);true),
    logger : stop.