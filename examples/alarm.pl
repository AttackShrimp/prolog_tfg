0.1::burglary.
0.2::earthquake.
0.7::hears_alarm(X) :- person(X).

alarm :- burglary.
alarm :- earthquake.

calls(X) :- alarm, hears_alarm(X).

person(mary). 
person(john).

query(calls(mary)).