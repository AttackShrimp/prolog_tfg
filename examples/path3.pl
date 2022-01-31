edge(1,2).
edge(1,3).
edge(2,5).
edge(2,6).
edge(3,4).
edge(4,5).
edge(5,6).

path(X,Y) :- edge(X,Y).
path(X,Y) :- edge(X,Z), path(Z,Y).

%%query(path(1,6))
