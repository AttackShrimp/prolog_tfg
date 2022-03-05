old(Name,Age) :-
  student(Name,NameAge),
  NameAge >= Age.

student(john,19).
student(rose,24).
student(peter,20).
student(susan,21).

%% query: old(john,18)
