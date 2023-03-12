%                                 Fundamentos de Inteligencia artificial      
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
%                      mano/4  mano(<Mazo>,<MazoRestante>,<TamañoMano>,<Mano>).

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

%                   mano_separada/3   mano_separada(<Mano>,<Comodines>,<Cartas>).

%   Genera dos listas a partir de la mano original, la primera contiene los coodines y la segunda 
%   el resto de las cartas.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

mano_separada([],[],[]).

mano_separada([X|Mano],[X|Comodines],Cartas) :- 
    comodín(X),
    mano_separada(Mano,Comodines,Cartas).

mano_separada([X|Mano],Comodines,[X|Cartas]) :- 
    \+ comodín(X),
    mano_separada(Mano,Comodines,Cartas).
    
%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                                               FIGURAS

%   Los siguientes predicados identifican las figuras presentes en las manos de cada jugador, 
%   las manos se procesan después de haber retirado los comodines con mano_separada/3.
%   A continuación se enlistan las figuras en orden descendente acompañadas de una descripción:

%   - Flor imperial     Las cinco cartas más altas de un solo palo
%   - Flor              Cinco cartas consecutivas de un mismo palo
%   - Poker             Cuatro cartas con el mismo valor o personaje
%   - Full house        Una tercia y un par
%   - Color             Cinco cartas no consecutivas de un mismo palo
%   - Escalera          Cinco cartas consecutivas
%   - Tercia            Tres cartas con el mismo valor
%   - Doble par         Dos pares distintos
%   - Par               Dos cartas con el mismo valor
%   - Nada              Ninguna de las figuras anteriores

%   NOTA: Cada figura incluye una explicación del algoritmo que se utiliza para identificarla.
%   Además, los predicados de una misma figura están ordenados de acuerdo a la cantidad de 
%   comodines que contenía la mano original, de 0 a 4.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%   FLOR IMPERIAL - 0 a 4 comodines

florImperial(Mano) :-
    sort(2,@<,Mano,Palos),                  %   Revisa que todas las cartas
    length(Palos,1),                        %   sean del mismo palo.

    sort(1,@<,Mano,ValoresCartas),          %   Verifica que no haya 
    length(Mano,TamañoMano),                %   cartas con valores 
    length(ValoresCartas,TamañoMano),       %   duplicados.

    FlorImperial = ['A','K','Q','J',10],
    forall(member(V-_,Mano),member(V,FlorImperial)).

%   FLOR - 0 a 4 comodines

flor(Mano) :-
    sort(2,@<,Mano,Palos),                  %   Revisa que todas las cartas
    length(Palos,1),                        %   sean del mismo palo.

    maplist(valor(),Mano,Valores),          %   Calcula los valores de 
    sort(Valores,ValoresCartas),            %   las cartas, los ordena 
    length(Mano,TamañoMano),                %   y verifica que no haya 
    length(ValoresCartas,TamañoMano),       %   duplicados.

    last(ValoresCartas,Mayor),              
    Mayor \== 14,                           %   Revisa que todos los elementos 
    ValoresCartas = [Menor|_],              %   estén en un rango de máximo 5 y
    Diferencia is Mayor - Menor,            %   que no sea una flor imperial.
    Diferencia < 5.

%   POKER - 0 a 3 comodines

poker(Mano) :-
    select(_,Mano,RestoMano),               %   Retira una carta (el kicker) y
    sort(1,@<,RestoMano,Valores),           %   se cerciora de que las cartas 
    length(Valores,1).                      %   restantes tengan el mismo valor.

%   FULL HOUSE
%   COLOR - 0 a 3 comodines

color(Mano) :-
    length(Mano,TamañoMano),                %   Verifica que tenga menos de 4
    TamañoMano > 1,                         %   comodines.

    sort(2,@<,Mano,Palos),                  %   Revisa que todas las cartas
    length(Palos,1).                        %   sean del mismo palo.

%   NOTA: se restringe la cantidad de comodines a 3 porque aunque es posible
%   tener los 4, una combinación de ese tipo sería también una flor, que es
%   una figura con más peso.

%   ESCALERA - 0 a 3 comodines

escalera(Mano) :-
    length(Mano,TamañoMano),                %   Verifica que tenga menos de 4
    TamañoMano > 1,                         %   comodines.

    maplist(valor(),Mano,Valores),          %   Calcula los valores de 
    sort(Valores,ValoresCartas),            %   las cartas, los ordena 
    length(Mano,TamañoMano),                %   y verifica que no haya 
    length(ValoresCartas,TamañoMano),       %   duplicados.

    last(ValoresCartas,Mayor),              
    Mayor \== 14,                           %   Revisa que todos los valores 
    ValoresCartas = [Menor|_],              %   estén en un rango de máximo 5 y
    Diferencia is Mayor - Menor,            %   que no sea una flor imperial.
    Diferencia < 5.

%   NOTA: se limita la cantidad de comodines porque al tener 4 en la mano se 
%   asegura por lo menos una flor y en caso de que se tenga una de las cartas 
%   más altas, una flor imperial.

%   TERCIA - 0 a 2 comodines
%Revisar que no haya pokers en vez de tercias, ása cuando son 5 elementos y hay un poker, lo mismo con los pares
tercia(Mano) :-
    select(V1-_,Mano,Resto1),               %   Retira dos cartas, verifica que
    \+ member(V1-_,Resto1),                 %   tengan valores distintos entre 
    select(V2-_,Resto1,RestoMano),          %   sí y de las que siguen en la mano.
    \+ member(V2-_,RestoMano),

    sort(1,@<,RestoMano,Valores),           %   Revisa que el resto de las cartas
    length(Valores,1).                      %   tengan el mismo valor.

%   NOTA: se limitan los comodines a 2 porque al tener 3 ya se garantiza un poker,
%   por lo menos.

%   DOBLE PAR
%   PAR - 0 o 1 comodín

par(Mano) :-
    select(_,Mano,Resto1),
    select(_,Resto1,Resto2),                %   Retira tres cartas y verifica que
    select(_,Resto2,RestoMano),             %   el resto tengan el mismo valor.
    sort(1,@<,RestoMano,Valores),           
    length(Valores,1). 

%   NOTA: se considera sólo un comodín porque al tener más ya es posible formar
%   figuras de mayor valor.

%   NADA

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                                   valor/2   valor(<Carta>,<Valor>).

%   Se utiliza para asignar un valor numérico a cada carta, para las cartas numéricas únicamente 
%   se recupera su valor, mientras que a las cartas de personaje se les asigna el número que le
%   corresponde al ordenarlas de manera ascendente. A los comodines se les asigna el 0.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

valor('A'-_,14).
valor('K'-_,13).
valor('Q'-_,12).
valor('J'-_,11).

valor(Valor-_,Valor) :-
    number(Valor).

valor(Comodín,0) :-
    comodín(Comodín).