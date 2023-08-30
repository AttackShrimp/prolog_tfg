contiene(paella, arroz).
contiene(paella, conejo).
contiene(hervido, patata).

carne(conejo).

receta_con_carne(X) :- contiene(X,Y), carne(Y).