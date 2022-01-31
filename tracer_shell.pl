#!/usr/bin/env swipl  
:- initialization main,halt.

:- use_module(utils).
:- use_module(parser).
:- use_module(tokenize).

:- dynamic fresh_vars/1.
:- dynamic verbose/0.

main :- 
    current_prolog_flag(argv,ArgV),
    get_options(ArgV,Options,RemArgV), !,
    (RemArgV = [FileName] 
        -> assert(cli_initial_file(FileName))
        ;  assert(cli_initial_file('examples/ancestor.pl'))),
    %(member(verbose,Options) -> (assert_verbose, print(Options),nl)
    %  ; (member(very_verbose,Options) -> (assert_verbose,assert_very_verbose, print(Options),nl) ; true)),
    (member(goal(Goal),Options) -> assert(cli_goal(Goal))
      ; assert(cli_goal('[ancestor(A,fanny)]'))),
    %% ((member(goal(F),Options), assert(cli_goal(F)),fail)
    %%  ; true),
    (member(output(Format),Options) -> assert(cli_output(Format)) ; assert(cli_output(raw))),  %%alternative: text
    %% ((member(file(File),Options), assert(cli_initial_file(File)),fail)
    %%  ; true),
    main_cli.

:- dynamic cli_initial_file/1.
:- dynamic cli_output/1.
:- dynamic cli_goal/1.

main_cli :-
  cli_initial_file(File),
  %cli_output(Format),
  cli_goal(QS),
  term_string(Q,QS),
  !,
  load(File),
  run(Q).

get_options([],Rec,Rem) :- !,Rec=[],Rem=[].
get_options(Inputs,RecognisedOptions,RemOptions) :-
   (recognise_option(Inputs,Flag,RemInputs)
     -> (RecognisedOptions = [Flag|RecO2], 
         assert(cli_option(Flag)), %%print(Flag),nl,
         RemO2 = RemOptions)
     ;  (Inputs = [H|RemInputs], RemOptions = [H|RemO2], RecO2 = RecognisedOptions)
   ),
   get_options(RemInputs,RecO2,RemO2).

recognise_option(Inputs,Flag,RemInputs) :-
   recognised_option(Heads,Flag),
   append(Heads,RemInputs,Inputs).
   
recognised_option(['-f',NT],file(NT)).
recognised_option(['-o',F],output(F)).
recognised_option(['-g',F],goal(F)).

:- dynamic cl/3.
:- dynamic comment/1.
:- dynamic visible/1.
:- dynamic unsafe/1.
:- dynamic explanations/1.
:- dynamic filename/1.
:- dynamic query/1.
:- dynamic counter/1.

counter(0).

% empty set of preds: no renaming is required
pred_renaming([],Prob,Atom,[(Prob,Pred,N)],Atom) :- !,functor(Atom,Pred,N).
% a derived atom is renamed when it already belongs to Preds (duplication is not a problem)
pred_renaming(Preds,1,Atom,Preds,RAtom) :- 
  functor(Atom,Pred,N),
  member((1,Pred,N),Preds),!,
  counter(I),retractall(counter(I)),J is I+1,assertz(counter(J)),
  Atom =.. [_|Args],
  atom_concat(Pred,I,PredN),
  RAtom =.. [PredN|Args].
% a probabilistic atom is renamed when the predicate belongs to Preds with a DIFFERENT probability
% (this is needed to avoid confusing the probabilities...)
pred_renaming(Preds,Prob,Atom,Preds,RAtom) :-  %% no need to add the new one to Preds...
  Prob < 1,
  functor(Atom,Pred,N),
  member((Prob2,Pred,N),Preds),
  Prob \== Prob2,!,
  counter(I),retractall(counter(I)),J is I+1,assertz(counter(J)),
  Atom =.. [_|Args],
  atom_concat(Pred,I,PredN),
  RAtom =.. [PredN|Args].
% in the remaining cases, renaming is not needed...
% (note that we don't want to duplicate probabilistic predicates)
pred_renaming(Preds,Prob,Atom,[(Prob,Pred,N)|Preds],Atom) :- 
  functor(Atom,Pred,N).

load(File) :-
  retractall(cl(_,_,_)),
  retractall(visible(_)),
  retractall(unsafe(_)),
  retractall(comment(_)),
  retractall(query(_)),
  retractall(filename(_)),
  retractall(explanations(_)),
  retractall(counter(_)),assertz(counter(0)),
  read_problog_program(File,Program),
  file_base_name(File,FileNameExt),
  string_concat(FileName,'.pl',FileNameExt),
  assertz(filename(FileName)),
  %nl,print(read_problog_program(File,Program)),nl,
  replace_vars(Program,ProgramVars),
  %nl,print(replace_vars(Program,ProgramVars)),nl,
  assert_clauses(ProgramVars),!.
  %query(Atom),

run(Q) :-
  findall(Trace,eval(0,Q,Trace),Traces),
  print_traces(Traces),
  write_traces(Traces).

write_traces(Trace) :-
  open('temp.txt',write,Stream),
  set_prolog_IO(user_input,Stream,user_error),
  (cli_output(raw) -> print(Trace); print_traces(Trace)),
  close(Stream).

print_traces([]).
print_traces([Trace]) :- 
  !, print_trace(Trace).
print_traces([Trace|R]) :-
  print_trace(Trace),
  print(';'),nl,
  print_traces(R).
print_trace([]).
print_trace([call(N,A)|R]) :-
  tab(N*3),format("~p~n",[A]),
  print_trace(R).

eval(_,[],[]).
eval(N,[ret|R],Trace) :-
   M is N-1,
  eval(M,R,Trace).
eval(N,[A|R],[call(N,A)|Trace]) :- 
  cl(_,A,Body),
  append(Body,[ret|R],RB),
  M is N+1,
  eval(M,RB,Trace).

  %% findall(SortedExplanation,
  %%         (cl(P,Atom,Body),
  %%          add_empty_ancestors(Body,BodyAn),
  %%          explanation([cl(P,Atom,[],[],BodyAn)],[],Explanation,[]),
  %%          sort(Explanation,SortedExplanation)
  %%         ),List),
  %% %cl(Probability,Head,NotUnfoldable,Variants,Unfoldable)
  %% %nl,print(Explanation),nl, 
  %% assertz(explanations(List)),
  %% print_explanations(List),
  %% format("Output files can be found in folder \"explanations\"."),!.


no_call([]).
no_call([res(_)|R]) :- no_call(R).
no_call([nounf(_)|R]) :- no_call(R).

remove_marks([],[]).
remove_marks([call(H,_)|R],[H|RR]) :- remove_marks(R,RR).
remove_marks([res(H)|R],[H|RR]) :- remove_marks(R,RR).
remove_marks([nounf(H)|R],[H|RR]) :- remove_marks(R,RR).

add_empty_ancestors([],[]).
add_empty_ancestors([H|R],[call(H,[])|RR]) :- add_empty_ancestors(R,RR).

add_ancestors([],_,[]).
add_ancestors([B|R],Ancestors,[call(B,Ancestors)|RA]) :-
  add_ancestors(R,Ancestors,RA).

member_variant(H,[A|_]) :- variant(H,A),!.
member_variant(H,[_|R]) :- member_variant(H,R).

%% read_problog_program(file,program)
%% reads and processes the structure of the ProBlog program from a given file:
%%   * extracts the list of tokens (Tokens) from the file
%%     (with help from tokenizer.pl)
%%   * removes unwanted tokens from Tokens (CleanTokens)
%%   * generates the data structure CleanProg by parsing (phrase)
%%     the list of tokens with the DCG specified in parser.pl
%%   * performs a post-processing ...

read_problog_program(File,Program) :-
  tokenize_file(File,Tokens,[cased(true),spaces(false),to(strings)]),
  %lists:subtract(Tokens,[cntrl("\n")],CleanTokens),
  %print(Tokens),nl,
  phrase(program(Program),Tokens),
  %nl,print(Program),nl,
  %vars_ctrs(CleanCtrs,Vars),
  %funs_ctrs(CleanCtrs,Funs),
  %post(CleanCtrs,Vars,Funs,PostCtrs),
  !.

assert_clauses([]).
assert_clauses([visible(Preds)|R]) :-
  !,assert_visible_preds(Preds),
  assert_clauses(R).
assert_clauses([unsafe(Preds)|R]) :-
  !,assert_unsafe_preds(Preds),
  assert_clauses(R).
assert_clauses([Cl|R]) :-
  assertz(Cl),
  assert_clauses(R).

assert_visible_preds([]).
assert_visible_preds([(Pred,N)|R]) :-
  functor(Atom,Pred,N),
  assertz(visible(Atom)),
  assert_visible_preds(R).

assert_unsafe_preds([]).
assert_unsafe_preds([(Pred,N)|R]) :-
  functor(Atom,Pred,N),
  assertz(unsafe(Atom)),
  assert_unsafe_preds(R).

replace_vars([],[]).
replace_vars([visible(L)|R],[visible(L)|RT]) :- 
  replace_vars(R,RT).
replace_vars([unsafe(L)|R],[unsafe(L)|RT]) :- 
  replace_vars(R,RT).
replace_vars([comment(C)|R],[comment(C)|RT]) :- 
  replace_vars(R,RT).
replace_vars([query(Q)|R],[query(Q3)|RT]) :- 
  rvars([Q],[Q2],[],-1),
  varnumbers([Q2],[Q3]),
  replace_vars(R,RT).
replace_vars([cl(Prob,Head,Body)|R],[cl(Prob,Head3,Body3)|RT]) :- 
  rvars([Head|Body],[Head2|Body2],[],-1),
  varnumbers([Head2|Body2],[Head3|Body3]),
  replace_vars(R,RT).

rvars([],[],_,_).
rvars([Atom|R],[NAtom|RR],VarList,N) :-
  Atom=..[Pred|Args],
  rvars_args(Args,VarList,N,ArgsVars,NVarList,NN),
  NAtom =.. [Pred|ArgsVars],
  rvars(R,RR,NVarList,NN).

rvars_args([],VarList,N,[],VarList,N).
rvars_args([var(X)|R],VarList,N,['$VAR'(Num)|RR],NVarList,NN) :-
  member((X,Num),VarList),!,
  rvars_args(R,VarList,N,RR,NVarList,NN).
rvars_args([var(X)|R],VarList,N,['$VAR'(M)|RR],NVarList,NN) :-
  !, M is N+1,
  rvars_args(R,[(X,M)|VarList],M,RR,NVarList,NN).
rvars_args([T|R],VarList,N,[T|RR],NVarList,NN) :-
  rvars_args(R,VarList,N,RR,NVarList,NN).



