% Definir los símbolos de los palos, personajes y valores de las cartas, también los comodines
palo(P) :- member(P, ['\u2664','\u2665','\u2667','\u2666']).
personaje(P):- member(P, ['A','J','Q','K']).
valor(V):- between(2,10,V).

% las cartas están compuestas por un valor numérico o personaje y un palo, o son comodines
carta_personaje(Personaje-Palo):-
    personaje(Personaje),
    palo(Palo).

carta_valor(Valor-Palo):-
    valor(Valor),
    palo(Palo).

comodín(C):- member(C,['JK1','JK2','JK3','JK4']).

baraja(B):-
    findall(C, comodín(C), Comodines),
    findall(C, carta_personaje(C), Personajes),
    findall(C, carta_valor(C), Valores),
    append(Personajes,Valores, Cartas),
    append(Comodines, Cartas,B).

barajar(Baraja1,Baraja2):- random_permutation(Baraja1,Baraja2).
