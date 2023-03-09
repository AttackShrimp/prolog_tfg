:- use_module(loader, [load_kb/2, get_coverage_and_clear/0, clear_dynamic/0]).

:- use_module(utils, [call_over_file/4]).

load(File) :-
    loader: load_kb(File, [cmd]),
    ['.temp.pl'].

load(File, Commands) :-
    loader: load_kb(File, Commands),
    ['.temp.pl'].

query(File) :-
    utils:call_over_file(File, read_terms, read, Terms),
    maplist(call, Terms),
    !,
    coverage.

coverage :-
    loader: get_coverage_and_clear.

unload :-
    % currently, if coverage is run after unload it fails silently, in order to
    % solve this, the loaded predicates should also be unloaded.
    loader: clear_dynamic.