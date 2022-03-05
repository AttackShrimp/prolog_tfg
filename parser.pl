:- module(parser,
         [program/3,
          problog_prob/3]).

:- use_module(library(dcg/basics)).
:- use_module(tokenize).

program(X) -->
  clauses(X).

clauses(C) -->
  [cntrl("\n")],
  clauses(C).
clauses([H|T]) -->
  clause(H),
  clauses(T).
clauses([]) --> [].

clause(reading(Atom,String,ListVars)) -->
  [punct("%")],
  [punct("!")],
  [word("read")],
  atom_ast(Atom),
  [word("as")],
  [punct(":")],
  comment_ast(C),
  [cntrl("\n")],
  {process_text(Atom,C,StringList,ListVars),atomics_to_string(StringList," ",String)}.

clause(visible([(Pred,N)|R])) -->
  [punct("%")],
  [word("visible")],
  [punct(":")],
  [word(PredS)],
  [punct("/")],
  [word(NS)],
  {atom_string(Pred,PredS),term_string(N,NS)},
  pred_ast(R),
  [cntrl("\n")].

clause(unsafe([(Pred,N)|R])) -->
  [punct("%")],
  [word("unsafe")],
  [punct(":")],
  [word(PredS)],
  [punct("/")],
  [word(NS)],
  {atom_string(Pred,PredS),term_string(N,NS)},
  pred_ast(R),
  [cntrl("\n")].

clause(comment(C)) -->
  [punct("%")],
  comment_ast(C),
  [cntrl("\n")].

clause(comment(C)) -->
  [punct("%")],
  comment_ast(C).

clause(query(Atom)) -->
  [word("query")],
  [punct("(")],
  atom_ast(Atom),
  [punct(")")],
  [punct(".")].

clause(cl(Prob,Atom,[])) -->
  [word(N1)],
  [punct(".")],
  [word(N2)],
  [punct(":")],
  [punct(":")],
  {string_concat(N1,".",Temp),string_concat(Temp,N2,N),term_string(Prob,N)},
  atom_ast(Atom),
  [punct(".")].  

clause(cl(1,Atom,[])) -->
  atom_ast(Atom),
  [punct(".")].

clause(cl(Prob,H,[B|T])) -->
  [word(N1)],
  [punct(".")],
  [word(N2)],
  [punct(":")],
  [punct(":")],
  {string_concat(N1,".",Temp),string_concat(Temp,N2,N),term_string(Prob,N)},
  atom_ast(H),
  [punct(":")],
  [punct("-")],
  sep,
  literal(B),
  sep,
  body_ast(T),
  [punct(".")].

clause(cl(1,H,[B|T])) -->
  atom_ast(H),
  [punct(":")],
  [punct("-")],
  sep,
  literal(B),
  body_ast(T),
  [punct(".")].

body_ast([]) --> [].
body_ast([Atom|T]) -->
  sep,
  [punct(",")],
  sep,
  literal(Atom),
  sep,
  body_ast(T).

sep -->
  {true}.
sep -->
  [cntrl("\n")].

literal(Atom) -->
  atom_ast(Atom).
literal(not(Atom)) -->
  [word("not")],
  atom_ast(Atom).

atom_ast(Atom) -->
  [word(X)],
  {atom_codes(Atom,X)}.
atom_ast(Atom) -->
  [word(X)],
  [punct(A)],
  [word(Y)],
  {word_to_term(X,Atom1),word_to_term(Y,Atom2),atom_codes(Op,A),
    Atom =.. [(Op)|[Atom1,Atom2]]}.
atom_ast(Atom) -->
  [word(X)],
  [punct(A)],
  [punct(B)],
  [word(Y)],
  {word_to_term(X,Atom1),word_to_term(Y,Atom2),
    string_concat(A,B,C),atom_codes(Op,C),
    Atom =.. [(Op)|[Atom1,Atom2]]}.
atom_ast(Atom) -->
  [word(P)],
  [punct("(")],
  args_ast(Args),
  [punct(")")],
  {atom_codes(Pred,P),Atom=..[Pred|Args]}.

%args_ast([]) --> [].
args_ast([Arg]) --> 
  [word(H)],
  {word_to_term(H,Arg)}.
args_ast([Arg|T]) --> 
  [word(H)],
  {word_to_term(H,Arg)},
  [punct(",")],
  args_ast(T).
%missing: lists and functions in general...

comment_ast([]) --> [].
comment_ast([H|T]) -->
  [word(H)],
  comment_ast(T).
comment_ast([H|T]) -->
  [punct(H)],
  comment_ast(T).
comment_ast([H|T]) -->
  [other(H)],
  comment_ast(T).

pred_ast([]) --> [].
pred_ast([(Pred,N)|R]) -->
  [punct(",")],
  [word(PredS)],
  [punct("/")],
  [word(NS)],
  {atom_string(Pred,PredS),term_string(N,NS)},
  pred_ast(R).

word_to_term(H,var(H)) :-
  term_string(Term,H),
  var(Term).
word_to_term(H,Term) :-
  term_string(Term,H),
  nonvar(Term).

problog_prob(prob(Prob)) -->
  atom_ast(_),
  [punct(":")],
  [cntrl("\t")],
  [word(N1)],
  [punct(".")],
  [word(N2)],
  {string_concat(N1,".",Temp),string_concat(Temp,N2,N),term_string(Prob,N)},
  [cntrl("\n")].

process_text(Atom,C,String,ListVars) :-
  vars_atom(Atom,Vars),
  pt(Vars,C,String,ListVars).

pt(_Vars,[],[],[]).
pt(Vars,[V|R],["~p"|String],[var(V)|ListVars]) :-
  member(var(V),Vars),!,
  pt(Vars,R,String,ListVars).
pt(Vars,[W|R],[W|String],ListVars) :-
  pt(Vars,R,String,ListVars).


vars_atom(Atom,Vars) :-
  Atom =.. [_|Vars].