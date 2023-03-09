﻿%                                 Fundamentos de Inteligencia artificial      
%                                     Nadia Itzel Guerrero Sánchez      
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
%   pertenecientes a un conjunto específico, A continuación se muestra cada predicado acompañado
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
    append(Personajes, Valores, B).

mazo(M) :-
    baraja(B1),baraja(B2),
    findall(C,comodín(C),Comodines),
    append(B1,B2,Cartas),
    append(Comodines,Cartas,M).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                     barajar/2    barajar(<BarajaOriginal>,<BarajaDesordenada>).

%   Recibe una lista de cartas y la devuelve en un orden distinto, puede utilizarse para barajas o
%   mazos completos.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

barajar(BarajaOriginal,BarajaDesordenada):- random_permutation(BarajaOriginal,BarajaDesordenada).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                  carta_al_azar/3    carta_al_azar(<Mazo>,<MazoRestante>,<Carta>).
%                       mano/4  mano(<Mazo>,<MazoRestante>,<TamañoMano>,<Mano>).

%   Estos predicados sirven para seleccionar cartas al azar y retirarlas del mazo. Con el predicado
%   carta_al_azar/3 únicamente se retira una carta, mientras que con mano/4 se pueden retirar las 
%   cartas que se deseen, incluso cuando la cqntidad de cartas (tamaño de mano) sea igual al tamaño
%   del mazo.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

carta_al_azar(Mazo,MazoRestante,Carta) :-
    length(Mazo,N),
    N >= 2,
    random_between(1, N, Pos),
    nth1(Pos,Mazo,Carta),
    select(Carta,Mazo,MazoRestante).

carta_al_azar(Mazo,MazoRestante,Carta) :-
    Mazo = [Carta],
    select(Carta,Mazo,MazoRestante).

mano(Mazo,Mazo,0,[]).

mano(Mazo,MazoRestante,TamañoMano,Mano) :-
    TamañoMano > 0,
    carta_al_azar(Mazo,RestoMazo,Carta),
    append([Carta],RestoMano,Mano),
    TamañoRestoMano is TamañoMano - 1,
    mano(RestoMazo,MazoRestante,TamañoRestoMano,RestoMano).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%   reparte_n_manos/5   reparte_n_manos(<Mazo>,<MazoRestante>,<CantidadManos>,<TamañoMano>,<Manos>).

%   Con este predicado se seleccionan y retiran del mazo las manos que se deseen, todas son del
%   mismo tamaño y se almacenan en una lista de listas. 

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

reparte_n_manos(Mazo,Mazo,0,_,[]).

reparte_n_manos(Mazo,MazoRestante,CantidadManos,TamañoMano,Manos) :-
    CantidadManos > 0,
    mano(Mazo,RestoMazo,TamañoMano,Mano),
    append([Mano],ManosAnteriores,Manos),
    CantidadManosRestante is CantidadManos - 1,
    reparte_n_manos(RestoMazo,MazoRestante,CantidadManosRestante,TamañoMano,ManosAnteriores).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                                 figura/2    figura(<Mano>,<Figura>).

%   Este predicado identifica las figuras presentes en una mano, nótese que como no se utiliza
%   el corte, se puede obtener más de una respuesta.
%   A continuación se enlistan las figuras en orden descendente acompañadas de una descripción:

%   - Flor imperial     Las cinco cartas más altas de un solo palo
%   - Flor              Cinco cartas consecutivas de un mismo palo
%   - Poker             Cuatro cartas con el mismo valor o personaje
%   - Full house        Una tercia y un par
%   - Color             Cinco cartas no consecutivas del mismo palo
%   - Escalera          Cinco cartas consecutivas
%   - Doble par         Dos pares distintos
%   - Par               Dos cartas con el mismo valor
%   - Nada              Ninguna de las figuras anteriores

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%   FLOR IMPERIAL
%   FLOR
%   POKER
%   FULL HOUSE
%   COLOR
%   ESCALERA
%   DOBLE PAR

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
%   PAR

%   Sin comodines:
%   Encuentra una carta con valor V y la retira de la mano, busca una segunda carta con el mismo
%   valor y también la retira, comprueba que ya no haya más cartas con el mismo valor y finalmente
%   revisa que las tres cartas restantes tengan valores distintos entre sí.

%   Con un comodín:
%   Encuentra un comodín en la mano y lo retira, luego comprueba que queden 4 cartas con valores 
%   distintos entre sí.
%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

figura(Mano,'Par') :-
    member(V-_,Mano),select(V-_,Mano,Resto1),
    member(V-_,Resto1),select(V-_,Resto1,Resto2),
    \+ member(V-_,Resto2),
    Resto2 = [V1-_,V2-_,V3-_],
    V3 \== V2, V3 \== V1, V1 \== V2.

figura(Mano,'Par') :-
    comodín(Comodín),
    member(Comodín,Mano),select(Comodín,Mano,Resto),
    Resto = [V1-_,V2-_,V3-_,V4-_],
    V1 \== V2, V1 \== V3, V1 \== V4,
    V2 \== V3, V2 \== V4, V3 \== V4.

%   NADA