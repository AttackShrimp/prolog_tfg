:- module(utils,
         [printv/1,
          print_explanations/1,
          print_explanation/1,
          combine_explanations/2,
          problog_exec/2]).
  
:- use_module(parser).

problog_exec(Args, Prob) :-
        process_create(path(problog), Args, [stdout(pipe(Stream)),
                                             stderr(pipe(Stream2))]),
        %copy_stream_data(Stream, current_output),
        read_string(Stream,_,Prob),
        % the process may terminate with any exit code.
        catch(close(Stream), error(process_error(_,exit(_)), _), true),
        catch(close(Stream2), error(process_error(_,exit(_)), _), true).

get_prob(FileName,Prob) :-
  problog_exec([FileName], Output),
  tokenize(Output,Tokens,[cased(true),spaces(false),to(strings)]),
  (Tokens = [word("DSharpError")|_], !, get_prob(FileName,Prob)
   ; 
   phrase(problog_prob(prob(Prob)),Tokens)
  ).

printv(P) :- verbose,!,print(P),nl,get_code(_) ; true.

print_explanations(Exps) :-
  (exists_directory("explanations") ; make_directory("explanations")),!,
  print_exps(Exps,1).

print_exps([],_) :-
  format("% No more explanations...~n~n"),
  filename(FN), 
  string_concat('explanations/',FN,Temp),
  string_concat(Temp,'_all.pl',FileName),
  open(FileName,write,Stream),
  set_prolog_IO(user_input,Stream,user_error),
  format("% Combined explanations:~n~n"),
  explanations(Exps),
  combine_explanations(Exps,Combined),
  print_explanation(Combined),
  query(Query),
  format("~nquery(~p).~n",[Query]),nl,
  get_prob(FileName,Prob),!,
  format("% Success probability: ~p~n",[Prob]),nl,  
  close(Stream),
  %
  set_prolog_IO(user_input,user_output,user_error),
  format("% Combined explanations:~n~n"),
  explanations(Exps),
  combine_explanations(Exps,Combined),
  print_explanation(Combined),
  query(Query),
  format("~nquery(~p).~n",[Query]),nl,
  format("% Success probability: ~p~n",[Prob]),nl.


print_exps([Exp|R],N) :-
  term_string(N,NS),
  filename(FN), 
  string_concat('explanations/',FN,Temp),
  string_concat(Temp,"_",Temp2),
  string_concat(Temp2,NS,TempNS),
  string_concat(TempNS,'.pl',FileName),
%  string_concat("explanations/exp",NS,Name),
%  string_concat(Name,".pl",NameExt),
  %
  %current_input(Input),current_output(Output),
  open(FileName,write,Stream),
  set_prolog_IO(user_input,Stream,user_error),
  format("% Explanation #~p:~n~n",[N]),
  print_explanation(Exp),
  query(Query),
  format("~nquery(~p).~n",[Query]),nl,
  get_prob(FileName,Prob),!,
  format("% Success probability: ~p~n",[Prob]),nl,
  close(Stream),
  %
  set_prolog_IO(user_input,user_output,user_error),
  format("% Explanation #~p:~n~n",[N]),
  print_explanation(Exp),
  query(Query),
  format("~nquery(~p).~n",[Query]),nl,
  format("% Success probability: ~p~n",[Prob]),nl,
  M is N+1,
  print_exps(R,M).

combine_explanations([],[]).
combine_explanations([Exp|R],Sorted) :-
  combine_explanations(R,Comb),
  append(Exp,Comb,All),
  sort(All,Sorted).

print_explanation([]).
print_explanation([cl(1,Head,[])|R]) :-
  print_fact(Head),!,
  nl,
  print_explanation(R).
print_explanation([cl(P,Head,[])|R]) :-
  P<1,!,
  print_prob_fact(P,Head),
  nl,
  print_explanation(R).
print_explanation([cl(P,Head,Body)|R]) :-
  P<1,!,
  print_prob_rule(P,Head,Body),
  nl,
  print_explanation(R).
print_explanation([cl(1,Head,Body)|R]) :-
  print_rule(Head,Body),
  nl,
  print_explanation(R).

print_fact(Head) :- format("~p.",[Head]).

print_prob_fact(P,Head) :- 
  format("~p::~p.",[P,Head]).

print_prob_rule(P,Head,[H|T]) :- 
  format("~p::~p :- ~p",[P,Head,H]),
  print_body(T),  %what about using join instead?
  format(".").

print_rule(Head,[H|T]) :- 
  format("~p :- ~p",[Head,H]),
  print_body(T),  %what about using join instead?
  format(".").

print_body([]).
print_body([H|T]) :- 
  format(",~p",[H]),
  print_body(T).



%----------

writeRules([]).  
writeRules([R|Rs]) :-
  writeln(R),
  writeRules(Rs).

%% pos(term,pos)
%% returns the set of positions of a term

pos(_,[]).
pos(cons(_,X), [P|Ps]) :-
  nth1(P,X,T),
  pos(T,Ps).
pos(funs(_,X), [P|Ps]) :-
  nth1(P,X,T),
  pos(T,Ps).

pretty(trs,P) :- pretty(P).
pretty(pl,P) :- prettypl(P).

%%pretty2 is similar to pretty but excludes the rules
%%defining source update functions

pretty2(trs,P) :- pretty2(P).
pretty2(pl,P) :- prettypl2(P).

pretty2(ctrs(_,R)) :-
  pretty2(R),
  nl.
pretty2(rules(Rs)) :-
  pretty2(Rs).
pretty2([]).
pretty2([R|Rs]) :-
  (def_u(R) -> true; pretty(R)),
  pretty2(Rs).

prettypl2(ctrs(_,R)) :-
  prettypl2(R),
  nl.
prettypl2(rules(Rs)) :-
  prettypl2(Rs).
prettypl2([]).
prettypl2([R|Rs]) :-
  (def_u(R) -> true; prettypl(R)),
  prettypl2(Rs).


def_u(rule(_,fun("u",_),_,_)).

%% pretty(trs_object)
%% pretty printing of a trs_object
%% a trs_object can be a trs or a component of a trs
%% (rules, defined symbols, variables...)

pretty(ctrs(_,R)) :-
  pretty(R),
  nl.
pretty(rules(Rs)) :-
  pretty(Rs).
pretty([]).
pretty([R|Rs]) :-
  pretty(R),
  pretty(Rs).
pretty(rule(_B,L,R,C)) :-
  %pretty(B),
  %format(" : "),
  pretty(L),
  format(" -> "),
  pretty(R),
  pretty_conds(C),
  nl.
pretty(beta(N)) :-
  format("b_~d",[N]).
pretty(cond(L,R)) :-
  pretty(L),
  format(" ->> "),
  pretty(R).
pretty(fun(N,[])) :-
  format("~s()",[N]).
pretty(fun(N,[T|Ts])) :-
  format("~s(",[N]),
  pretty_args([T|Ts]),
  format(")").
pretty(cons(N,[])) :-
  format("~s",[N]).
pretty(cons(N,[T|Ts])) :-
  format("~s(",[N]),
  pretty_args([T|Ts]),
  format(")").
pretty(var(N,_)) :-
  format("~s",[N]).
pretty(tuple(Arg)) :- !,
  pretty(Arg).
pretty(TupleArgs) :-
  TupleArgs =.. [tuple|Args],
  format("<"),
  pretty_args(Args),
  format(">").

pretty_args([]).
pretty_args([A|As]) :-
  pretty(A),
  pretty_commas(As).

pretty_commas([]).
pretty_commas([A|As]) :-
  format(", "),
  pretty(A),
  pretty_commas(As).

pretty_conds([]).
pretty_conds([C|Cs]) :-
  format(" <= "),
  pretty_args([C|Cs]).

%%%%%%%%%%%%%%%%%%%%%%

prettypl(ctrs(_,R)) :-
  prettypl(R),
  nl.
prettypl(rules(Rs)) :-
  prettypl(Rs).
prettypl([]).
prettypl([R|Rs]) :-
  prettypl(R),
  prettypl(Rs).
prettypl(rule(_B,fun(F,Args),R,C)) :-
  %pretty(B),
  %format(" : "),
  append(Args,[R],ArgsR),
  prettypl(fun(F,ArgsR)),
  prettypl_conds(C),
  format("."),
  nl.
prettypl(beta(N)) :-
  format("b_~d",[N]).
prettypl(cond(fun(F,Args),R)) :-
  append(Args,[R],ArgsR),
  prettypl(fun(F,ArgsR)).
prettypl(fun(N,[])) :-
  format("~s()",[N]).
prettypl(fun(N,[T|Ts])) :-
  format("~s(",[N]),
  prettypl_args([T|Ts]),
  format(")").
prettypl(cons(N,[])) :-
  format("~s",[N]).
prettypl(cons(N,[T|Ts])) :-
  format("~s(",[N]),
  prettypl_args([T|Ts]),
  format(")").
prettypl(var(N,_)) :-
  string_upper(N,NN),
  format("~s",[NN]).
prettypl(tuple(Arg)) :- !,
  prettypl(Arg).
prettypl(TupleArgs) :-
  TupleArgs =.. [tuple|Args],
  format("tuple("),
  prettypl_args(Args),
  format(")").

prettypl_args([]).
prettypl_args([A|As]) :-
  prettypl(A),
  prettypl_commas(As).

prettypl_commas([]).
prettypl_commas([A|As]) :-
  format(", "),
  prettypl(A),
  prettypl_commas(As).

prettypl_conds([]).
prettypl_conds([C|Cs]) :-
  format(" :- "),
  prettypl_args([C|Cs]).
