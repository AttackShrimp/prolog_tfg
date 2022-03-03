%!read mother(A,B) as: A is the mother of B
%!read father(A,B) as: A is the father of B
%!read parent(A,B) as: A is the parent of B
%!read ancestor(A,B) as: A is the ancestor of B

mother(tim, anna).
mother(anna, fanny).
mother(daniel, fanny).
mother(celine, gertrude).
father(tim, bernd).
father(anna, ephraim).
father(daniel, ephraim).
father(celine, daniel).

parent(X,Y) :- mother(X,Y).
parent(X,Y) :- father(X,Y).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

%% query: ancestor(A,fanny)
