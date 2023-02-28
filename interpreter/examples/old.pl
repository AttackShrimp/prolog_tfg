%!read old(A,B) as: A is over B years old
%!read student(A,B) as: A is a student with age = B

old(Name,Age) :-
  student(Name,NameAge),
  NameAge >= Age.

student(john,19).
student(rose,24).
student(peter,20).
student(susan,21).

%% query: old(john,18)
