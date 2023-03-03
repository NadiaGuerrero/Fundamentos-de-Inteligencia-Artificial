% Definir los símbolos de los palos, personajes y valores de las cartas, también los comodines
palo(P) :- member(P, ['\u2664','\u2665','\u2667','\u2666']).
personaje(P):- member(P, ['A','K','Q','J']).
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
    findall(C, carta_personaje(C), Personajes),
    findall(C, carta_valor(C), Valores),
    findall(C, comodín(C), Comodines),
    append(Personajes,Valores, Cartas),
    append(Cartas,Comodines,B).

% definir un predicado para un mazo

barajar(BarajaOriginal,BarajaDesordenada):- random_permutation(BarajaOriginal,BarajaDesordenada).

carta_al_azar(Mazo, Carta) :-
    length(Mazo,N),
    N >= 2,
    random_between(1, N, Pos),
    nth1(Pos,Mazo,Carta).

% Definir predicado para mano donde sí se eliminen las cartas del mazo
