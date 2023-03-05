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