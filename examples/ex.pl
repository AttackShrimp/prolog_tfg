disease(X) :- not symptom(X), registered(X).

symptom(X) :- observed(X),checked(X).

observed(a).
observed(b).
checked(b).

registered(a).
registered(b).
registered(c).