:- use_module(utils).
:- use_module(parser).
:- use_module(tokenize).

:- dynamic fresh_vars/1.
:- dynamic verbose/0.

%%%%%%%%%%%%%%%%%%%%% tracer.pl %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic cl/3.
:- dynamic reading/3.
:- dynamic comment/1.
:- dynamic visible/1.
:- dynamic unsafe/1.
:- dynamic explanations/1.
:- dynamic filename/1.
:- dynamic query/1.
:- dynamic counter/1.

:- dynamic passed/2.
:- dynamic possible_passed/1.

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
  retractall(reading(_,_,_)),
  retractall(visible(_)),
  retractall(unsafe(_)),
  retractall(comment(_)),
  retractall(query(_)),
  retractall(filename(_)),
  retractall(explanations(_)),
  retractall(counter(_)),assertz(counter(0)),
  read_problog_program(File,Program),
  %print(Program),
  file_base_name(File,FileNameExt),
  string_concat(FileName,'.pl',FileNameExt),
  assertz(filename(FileName)),
  %nl,print(read_problog_program(File,Program)),nl,
  replace_vars(Program,ProgramVars),
  %nl,print(replace_vars(Program,ProgramVars)),nl,
  assert_clauses(ProgramVars),
  %query(Atom),

  retractall(passed(_,_)),
  retractall(possible_passed(_)),
  findall(cl(1,Head,Body), cl(1,Head,Body), Clauses), 
  phrase(add_assertion(0, Possible_passed), Clauses),
  assertz(possible_passed(Possible_passed)), !.

add_assertion(_, []) --> [].
add_assertion(Curr, [(Name / Arity - Curr, 0) | T]) --> 
  [cl(1, H, B)], 
  {
    retract(cl(1,H,B)),
    functor(H, Name, Arity),
    assertz(cl(1, H, [assertz(passed(Name / Arity, Curr))|B])),
    Next is Curr + 1
  },
  add_assertion(Next, T).

process_traces([],[]).
process_traces([T|R],[TT|RR]) :-
  process_trace(T,TT),
  process_traces(R,RR).
 
process_trace([],[]).
process_trace([call(N,A)|R],[call(N,A,S)|RT]) :-
  (reading(A,String,Vars) -> format(string(S),String,Vars)
    ; term_string(A,S)),
  process_trace(R,RT).
process_trace([not_call(N,A)|R],[not_call(N,A,S)|RT]) :-
  (reading(not(A),String,Vars) -> format(string(S),String,Vars)
    ; term_string(A,AS), string_concat("not ",AS,S)),
  process_trace(R,RT).
process_trace([failed_call(N,A)|R],[failed_call(N,A,S2)|RT]) :-
  (reading(A,String,Vars) -> format(string(S),String,Vars)
    ; term_string(A,S)),
  string_concat("(failed) ", S, S2),
  process_trace(R,RT).

run(Q) :-
  
  retractall(passed(_,_)),

  findall(Trace,eval(0,Q,Trace),Traces),
  %print(Traces),nl,
  process_traces(Traces,_Traces2),  
  %print(Traces2),nl,
  %print_traces(Traces2),
  %write_traces(Traces2),

  findall(F/A - Order, passed(F/A, Order), Calls), 
  process_calls(Calls, Result),
  header,
  pretty(Result), !.


process_calls(Calls, Calls_perc) :- 
  possible_passed(Pos_passed),
  phrase(count_duplicates(Calls), [Pos_passed], [Calls_set]),
  to_key_value(Calls_set, Calls_keys),
  keysort(Calls_keys, Calls_sorted),
  global_percentage(Calls_sorted, Calls_gperc),
  phrase(local_percentage(Calls_gperc, Calls_perc), [[]], [_Local_Occ]).

count_duplicates([]) --> [].
count_duplicates([E | T]) --> 
  state(Original, Modified),
  {
    append(Start, [(E, Num) | End], Original),
    M is Num + 1,
    append(Start, [(E, M) | End], Modified)
  },
  !,
  count_duplicates(T).
% This last instance should never happen:
count_duplicates([E | T]) --> state(S, [(E, 1) | S]), count_duplicates(T).

to_key_value([],[]).
to_key_value([(F/A - Order, Amount) | Ta], [Order - (F/A - Amount) | Tb]) :- to_key_value(Ta, Tb).

global_percentage(In, Out) :- global_percentage(In, Out, 0, _Total).
global_percentage([], [], Acc, Acc).
global_percentage([Line - (Functor / Arity - Occ) | Ta], [Line - (Functor / Arity - Occ, G_percentage) | Tb], Acc, Res) :-
  New_acc is Acc + Occ,
  global_percentage(Ta, Tb, New_acc, Res),
  (Res =:= 0 -> G_percentage = 0; G_percentage is Occ / Res).

local_percentage([], []) --> [].
local_percentage([Line - (Functor / Arity - Occ, G_percentage) | T], [Line - (Functor / Arity - Occ, G_percentage, L_percentage) | T_local]) --> 
  state(Original, Modified),
  {
    append(Start, [(Functor / Arity, Num) | End], Original),
    M is Num + Occ,
    append(Start, [(Functor / Arity, M) | End], Modified)
  },
  !,
  local_percentage(T, T_local),
  state(Final),
  {
    member((Functor / Arity, L_Occ), Final),
    (L_Occ =:= 0 -> L_percentage = 0; L_percentage is Occ / L_Occ)
  }.
local_percentage([Line - (Functor / Arity - Occ, G_percentage) | T], [Line - (Functor / Arity - Occ, G_percentage, L_percentage) | T_local]) --> 
  state(S, [(Functor / Arity, Occ) | S]),
  local_percentage(T, T_local),
  state(Final),
  {
    member((Functor / Arity, L_Occ), Final),
    (L_Occ =:= 0 -> L_percentage = 0; L_percentage is Occ / L_Occ)
  }.

header :-
  format('~n~|~95t~20+ ~w ~|~95t~20+~n~n', ['CODE COVERAGE']).

pretty([]).
pretty([Line - (Functor / Arity - Occ, G_percentage, L_percentage) | T]) :- 
  format('~|~t~d~3+  ~w/~w~25+# ~w~10+G ~2f~10+L ~2f~n',[Line, Functor, Arity, Occ, G_percentage, L_percentage]),
  pretty(T).

state(S0),     [S0] --> [S0].
state(S0, S1), [S1] --> [S0].



write_traces(Trace) :-
  open('temp.txt',write,Stream),
  set_prolog_IO(user_input,Stream,user_error),
  print(Trace),
  %print_traces(Trace),
  close(Stream).

print_traces([]).
print_traces([Trace]) :- 
  !, print_trace(Trace).
print_traces([Trace|R]) :-
  print_trace(Trace),
  print(';'),nl,
  print_traces(R).

print_trace([]).
print_trace([call(N,_A,S)|R]) :-
%  tab(N*3),format("~p~n",[A]),
  tab(N*3),format(S),nl,
  print_trace(R).
print_trace([not_call(N,_A,S)|R]) :-
%  tab(N*3),format("~p~n",[A]),
  tab(N*3),format(S),nl,
  print_trace(R).
print_trace([failed_call(N,_A,S)|R]) :-
%  tab(N*3),format("~p~n",[A]),
  tab(N*3),format(S),nl,
  print_trace(R).


eval(_,[],[]).

eval(N,[ret|R],Trace) :-
  % No ! means if program has 'ret.' then the automatic backtracking will throw an error
   M is N-1,
  eval(M,R,Trace).

eval(N,[A|R],[call(N,A)|Trace]) :-
  predicate_property(A,built_in),
  \+ predicate_name(A,"not/1"), 
  !,
  call(A),
  eval(N,R,Trace).

eval(N,[not(A)|R],[not_call(N,A)|Trace]) :- 
  ground(A),
  \+ cl(_,A,_Body),
  eval(N,R,Trace).

eval(N,[not(A)|R],[not_call(N,A)|Trace]) :- 
  ground(A),
  cl(_,A,Body),
  M is N+1,
  eval_not(M,Body,Trace1),
  !,
  eval(N,R,Trace2),
  append(Trace1,Trace2,Trace).

eval(N,[A|R],[call(N,A)|Trace]) :- 
  cl(_,A,Body),
  append(Body,[ret|R],RB),
  M is N+1,
  eval(M,RB,Trace).

eval_not(N,[ret|R],Trace) :-
   M is N-1,
  eval_not(M,R,Trace).

eval_not(N,[not(A)|_R],[call(N,A)|Trace]) :- 
  ground(A),
  cl(_,A,Body),
  M is N+1,
  eval(M,Body,Trace).

eval_not(N,[not(A)|R],[not_call(N,A)|Trace]) :- 
  ground(A),
  cl(_,A,Body),
  M is N+1,
  \+ eval(M,Body,_Trace),
  eval_not(N,R,Trace).

eval_not(N,[A|_R],[failed_call(N,A)]) :- 
  \+ cl(_,A,_Body).

eval_not(N,[A|R],[call(N,A)|Trace]) :- 
  ground(A),
  cl(_,A,Body),
  M is N+1,
  eval(M,Body,Trace1),!,
  eval_not(N,R,Trace2),
  append(Trace1,Trace2,Trace).


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

% Repeat code
assert_unsafe_preds([]).
assert_unsafe_preds([(Pred,N)|R]) :-
  functor(Atom,Pred,N),
  assertz(unsafe(Atom)),
  assert_unsafe_preds(R).

% I don't understand the need for this method
replace_vars([],[]).

replace_vars([visible(L)|R],[visible(L)|RT]) :- 
  replace_vars(R,RT).

replace_vars([unsafe(L)|R],[unsafe(L)|RT]) :- 
  replace_vars(R,RT).

replace_vars([reading(Q,String,Vars)|R],[reading(Q3,String,V4)|RT]) :- 
  V =.. [foo|Vars],
  rvars([Q,V],[Q2,V2],[],-1),
  varnumbers([Q2,V2],[Q3,V3]),
  V3 =.. [_|V4],
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

rvars([not(Atom)|R],[not(NAtom)|RR],VarList,N) :-
  !,
  Atom=..[Pred|Args],
  rvars_args(Args,VarList,N,ArgsVars,NVarList,NN),
  NAtom =.. [Pred|ArgsVars],
  rvars(R,RR,NVarList,NN).

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

% does not consider variables inside terms
rvars_args([T|R],VarList,N,[T|RR],NVarList,NN) :-
  rvars_args(R,VarList,N,RR,NVarList,NN).



