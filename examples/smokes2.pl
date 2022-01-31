% visible:

0.3::stressp(X).
stress(X) :- person(X),stressp(X).

0.2::influencesp(X,Y).
influences(X,Y) :- person(X), person(Y), influencesp(X,Y).

smokes(X) :- stress(X).
smokes(X) :- friend(X,Y), influences(Y,X), smokes(Y).

0.4::asthmap(X).
asthma(X) :- smokes(X), asthmap(X).

person(1).
person(2).
person(3).
person(4).

friend(1,2).
friend(2,1).
friend(2,4).
friend(3,2).
friend(4,2).

query(asthma(1)).