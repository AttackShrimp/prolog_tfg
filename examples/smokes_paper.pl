% running example 
% visible: smokes/1

0.8::stress(X) :- person(X).
person(ann). 
person(bob).

0.3::influences(bob,carl). 
0.1::influences(ann,bob).

smokes(X) :- stress(X).
smokes(X) :- influences(Y,X),smokes(Y).

query(smokes(carl)).
