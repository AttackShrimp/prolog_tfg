% visible: 
% unsafe: smokes/1

0.3::stress(X) :- person(X).
0.2::influences(X,Y) :- person(X), person(Y).

smokes(X) :- stress(X).
smokes(X) :- friend(X,Y), influences(Y,X), smokes(Y).

0.4::asthma(X) :- smokes(X).

person(john).
person(rose).
person(peter).
person(anna).

friend(john,rose).
friend(rose,john).
friend(rose,anna).
friend(peter,rose).
friend(anna,rose).

query(asthma(john)).