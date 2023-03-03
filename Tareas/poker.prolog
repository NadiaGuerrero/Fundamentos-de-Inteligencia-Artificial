palo(P) :- member(P, ['\u2664','\u2665','\u2667','\u2666']).
personaje(P):- member(P, ['A','J','Q','K']).
valor(V):- between(2,10,V).