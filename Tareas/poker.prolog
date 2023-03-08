%                       Fundamentos de Inteligencia artificial      
%                           Nadia Itzel Guerrero Sánchez      
%
%   Tarea #2 Figuras de poker              
%
%   Construir un programa en prolog que reparta cartas a 4 jugadores de un mazo de 108 cartas,
%   identifique la mejor figura en cada mano y los ordene descendentemente, reportando el
%   resultado en la consola.
%
%   Predicados relevantes:
%   - 
%
%   Recomendaciones
%

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%   Los siguientes predicados tienen aridad 1 y se utilizan para identificar elementos 
%   pertenecientes a un conjunto específico, A continuación se enlista cada predicado acompañado
%   de la lista de elementos con los que es válido y una breve descripción.

%                                                                       Representan a los palos de 
%   - palo/1.       palo(<Palo>).               [♤, ♥, ♧, ♦]            la baraja inglesa: picas, 
%                                                                       corazones, tréboles y diamantes.

%                                                                       Corresponden a los personajes 
%   - personaje/1   personaje(<Personaje>).     [A, K, Q, J]            de las cartas más altas de la 
%                                                                       baraja: As, Rey, Reina y Jota.

%
%   - valor/1       valor(<Valor>).             [2-10]                  Son los valores de las cartas 
%                                                                       con número.

%                                                                       Son los 4 comodines que 
%   - comodín/1     comodín(<Comodín>).         [JK1, JK2, JK3, JK4]    se tienen en un mazo de 
%                                                                       2 barajas.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

palo(P) :- member(P, ['\u2664','\u2665','\u2667','\u2666']).

personaje(P) :- member(P, ['A','K','Q','J']).

valor(V) :- between(2,10,V).

comodín(C) :- member(C,['JK1','JK2','JK3','JK4']).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                       carta_personaje/2  carta_personaje(<Personaje>-<Palo>).
%                           carta_valor/2  carta_valor(<Valor>-<Palo>).

%   Son los dos tipos de cartas que hay en la baraja (además de los comodines), están conformadas
%   por un par, donde el primer elemento es el valor o personaje de la carta y el segundo el palo.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

carta_personaje(Personaje-Palo) :-
    personaje(Personaje),
    palo(Palo).

carta_valor(Valor-Palo) :-
    valor(Valor),
    palo(Palo).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                                   baraja/1    baraja(<Baraja>).
%                                       mazo/1  mazo(<Mazo>).

%   Generan una lista con 52 y 108 cartas, respectivamente. La baraja solo incluye las cartas con 
%   valor o personaje, pues los comodines se añaden al crear el mazo con dos barajas.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

baraja(B) :-
    findall(C, carta_personaje(C), Personajes),
    findall(C, carta_valor(C), Valores),
    append(Personajes, Valores, B),

mazo(M) :-
    baraja(B1),baraja(B2),
    findall(C,comodín(C),Comodines),
    append(B1,B2,Cartas),
    append(Comodines,Cartas,B).