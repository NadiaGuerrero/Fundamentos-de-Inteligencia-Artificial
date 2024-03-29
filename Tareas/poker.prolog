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
%   - repartir/0  repartir().
%   - figurasManos/3 figurasManos(<Manos>,<Figuras>,<NumJugador>).
%
%   Recomendaciones
%   Ejecutar únicamente repartir/0 las veces que se desee, los resultados serán siempre 
%   independientes uno de otro.

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

%   Los siguientes predicados tienen aridad 1, reciben como único argumento una lista de cartas 
%   <Mano> y son verdaderos cuando la figura está presente en la mano.Todas las manos se procesan 
%   después de haber retirado los comodines con mano_separada/3.
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

%   NOTA: Todas las figuras soportan comodines.

%   El segundo predicado de cada figura con aridad 2 calcula un valor numérico basado en la cantidad
%   de comodines que tiene y la carta más alta, de forma que las figuras con más comodines y cartas
%   de menor valor tengan un número alto, mientras que las figuras altas tengan un valor menor.
%   La figura con el menor valor posible es una flor imperial sin comodines con una puntuación de 86.

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

florImperial(Mano,Valor) :-
    florImperial(Mano),
    length(Mano,T),
    maplist(valor(),Mano,Valores),
    max_list(Valores,Max),
    Valor is 200 - (T*20) - Max.

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

flor(Mano,Valor) :-
    length(Mano,T),
    flor(Mano),
    maplist(valor(),Mano,Valores),
    max_list(Valores,Max),
    Valor is 400 - (T*20) - Max.

%   POKER - 0 a 3 comodines

poker(Mano) :-
    select(_,Mano,RestoMano),               %   Retira una carta (el kicker) y
    sort(1,@<,RestoMano,Valores),           %   se cerciora de que las cartas 
    length(Valores,1).                      %   restantes tengan el mismo valor.

poker(Mano,Valor) :-
    poker(Mano),
    length(Mano,T),
    (
        (Mano = [V-_,V-_,V-_,V-_,V-_],Mano = Resto);
        (Mano = [V-_,V-_,V-_,V-_],Mano = Resto);
        (Mano = [V-_,V-_,V-_],Mano = Resto);
        (T =< 5,T > 2,select(V-_,Mano,Resto),\+ member(V-_,Resto));
        (T = 2, Mano = Resto)
    ),
    maplist(valor(),Resto,Valores),
    max_list(Valores,Max),

    Valor is 600 - (T*20) - Max.

%   FULL HOUSE - 0 o 1 comodín

fullHouse(Mano) :-
    select(V1-_,Mano,Resto),                %   0 comodines
    select(V1-_,Resto,Resto1),
    Resto1 = [V2-_,V2-_,V2-_],
    V1 \== V2.

fullHouse(Mano) :-
    sort(1,@=<,Mano,ManoOrdenada),          %   1 comodín
    ManoOrdenada = [V1-_,V1-_,V2-_,V2-_],   %   Recibe 2 pares de cartas.
    V1 \== V2.

fullHouse(Mano) :-
    select(V1-_,Mano,Resto),                %   1 comodín
    Resto = [V2-_,V2-_,V2-_],               %   Recibe una tercia y una carta distinta.
    V1 \== V2.

fullHouse(Mano,Valor) :-
    fullHouse(Mano),
    length(Mano,T),
    (
        (T = 5,select(V1-_,Mano,Resto1),select(V1-_,Resto1,Resto),\+ member(V1-_,Resto));
        (T = 4,select(V1-_,Mano,Resto1),select(V1-_,Resto1,Resto2),\+ member(V1-_,Resto2),
        Resto2 = [V2-_,V2-_],Mano = Resto);
        (T = 4,select(V-_,Mano,Resto),\+ member(V-_,Resto))
    ),
    maplist(valor(),Resto,Valores),
    max_list(Valores,Max),
    Valor is 800 - (T*20) - Max.

%   COLOR - 0 a 3 comodines

color(Mano) :-
    length(Mano,TamañoMano),                %   Verifica que tenga menos de 4
    TamañoMano > 1,                         %   comodines.

    sort(2,@<,Mano,Palos),                  %   Revisa que todas las cartas
    length(Palos,1).                        %   sean del mismo palo.

color(Mano,Valor) :-
    length(Mano,T),
    color(Mano),
    maplist(valor(),Mano,Valores),
    max_list(Valores,Max),
    Valor is 1000 - (T*20) - Max.

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

escalera(Mano,Valor) :-
    escalera(Mano),
    length(Mano,T),
    maplist(valor(),Mano,Valores),
    max_list(Valores,Max),
    Valor is 1200 - (T*20) - Max.

%   NOTA: se limita la cantidad de comodines porque al tener 4 en la mano se 
%   asegura por lo menos una flor y en caso de que se tenga una de las cartas 
%   más altas, una flor imperial.

%   TERCIA - 0 a 2 comodines

tercia(Mano) :-
    select(V1-_,Mano,Resto1),               %   Retira dos cartas, verifica que
    \+ member(V1-_,Resto1),                 %   tengan valores distintos entre 
    select(V2-_,Resto1,RestoMano),          %   sí y de las que siguen en la mano.
    \+ member(V2-_,RestoMano),

    sort(1,@<,RestoMano,Valores),           %   Revisa que el resto de las cartas
    length(Valores,1).                      %   tengan el mismo valor.

tercia(Mano,Valor) :-
    tercia(Mano),
    length(Mano,T),
    (
        (T = 5,select(V1-_,Mano,Resto1),\+ member(V1-_,Resto1),
        select(V2-_,Resto1,Resto),\+ member(V2-_,Resto));
        (T = 4,select(V-_,Mano,Resto),\+ member(V-_,Resto));
        (Mano = Resto)
    ),
    maplist(valor(),Resto,Valores),
    max_list(Valores,Max),
    Valor is 1400 - (T*20) - Max.

%   NOTA: se limitan los comodines a 2 porque al tener 3 ya se garantiza un poker,
%   por lo menos.

%   DOBLE PAR 0 a 2 comodines

doblePar(Mano) :-
    Mano = [V1-_,V2-_,V3-_],                %   2 comodines
    V3 \== V2, V3 \== V1, V1 \== V2.

doblePar(Mano) :-
    select(V1-_,Mano,Resto1),               %   1 comodín
    \+ member(V1-_,Resto1),                 
    select(V2-_,Resto1,Resto2),             %   Verifica que haya dos cartas iguales 
    \+ member(V2-_,Resto2),                 %   y dos diferentes.
    Resto2 = [V-_,V-_].

doblePar(Mano) :-
    select(V1-_,Mano,Resto1),               %   0 comodines
    \+ member(V1-_,Resto1),
    select(V2-_,Resto1,Resto2),             %   Se tienen los dos pares y una 
    select(V2-_,Resto2,Resto3),             %   carta diferente.
    \+ member(V2-_,Resto3),
    Resto3 = [V-_,V-_].

doblePar(Mano,Valor) :-
    doblePar(Mano),
    length(Mano,T),
    (
        (T = 5,select(V-_,Mano,Resto),\+ member(V-_,Resto));
        (Mano = Resto)
    ),
    maplist(valor(),Resto,Valores),
    max_list(Valores,Max),
    Valor is 1600 - (T*20) - Max.    

%   PAR - 0 o 1 comodín

par(Mano) :-
    select(V1-_,Mano,Resto1),               %   Retira tres cartas, verifica que
    \+ member(V1-_,Resto1),                 %   tengan valores distintos entre sí
    select(V2-_,Resto1,Resto2),             %   y que ya no estén presentes en la mano.
    \+ member(V2-_,Resto2),
    select(V3-_,Resto2,RestoMano),             
    \+ member(V3-_,RestoMano),
    
    sort(1,@<,RestoMano,Valores),           %   Verifica que las cartas que quedan 
    length(Valores,1).                      %   en la mano sean iguales.

par(Mano,Valor) :-
    par(Mano),
    length(Mano,T),
    (
        (T = 5,select(V-_,Mano,Resto),select(V-_,Resto,_),valor(V-_,Max));
        (T = 4,maplist(valor(),Mano,Valores),max_list(Valores,Max))
    ),
    Valor is 1800 - (T*20) - Max.

%   NOTA: se considera sólo un comodín porque al tener más ya es posible formar
%   figuras de mayor valor.

%   NADA

nada(Mano) :-
    \+ (
        florImperial(Mano);
        flor(Mano);
        poker(Mano);
        fullHouse(Mano);
        color(Mano);
        escalera(Mano);
        tercia(Mano);
        doblePar(Mano);
        par(Mano)
    ).

nada(Mano,Valor) :-
    nada(Mano),
    maplist(valor(),Mano,Valores),
    max_list(Valores,Max),
    Valor is 2000 - Max.

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

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                           figurasManos/2  figurasManos(<Manos>,<Figuras>,<Jugador>).

%   Este predicado recorre la lista de manos y por cada una crea un par donde el primer elemento
%   es un número que corresponde a la figura y el segundo es la mano. Estos pares se almacenan en
%   la lista <Figuras>. El tercer argumento <NumJugador> se utiliza para asignar un número a la mano
%   que se está analizando, es sólo para facilitar la impresión de los resultados.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

figurasManos([],[],_).
figurasManos([Mano|RestoManos],[Figura|RestoFiguras],NumJugador) :-
    NumJugadorA is NumJugador - 1,
    atom_concat('Jugador - ',NumJugador,Jugador),
    mano_separada(Mano,_,ManoS),
    (
        (florImperial(ManoS,Valor), Figura = Valor-[Jugador,'Flor imperial',Mano]);
        (flor(ManoS,Valor), Figura = Valor-[Jugador,'Flor',Mano]);
        (poker(ManoS,Valor), Figura = Valor-[Jugador,'Poker',Mano]);
        (fullHouse(ManoS,Valor), Figura = Valor-[Jugador,'Full house',Mano]);
        (color(ManoS,Valor), Figura = Valor-[Jugador,'Color',Mano]);
        (escalera(ManoS,Valor), Figura = Valor-[Jugador,'Escalera',Mano]);
        (tercia(ManoS,Valor), Figura = Valor-[Jugador,'Tercia',Mano]);
        (doblePar(ManoS,Valor), Figura = Valor-[Jugador,'Doble par',Mano]);
        (par(ManoS,Valor), Figura = Valor-[Jugador,'Par',Mano]);
        (nada(ManoS,Valor), Figura = Valor-[Jugador,'Nada',Mano])
    ),
    figurasManos(RestoManos,RestoFiguras,NumJugadorA).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                                       repartir/0  repartir().

%   repartir/0 es el predicado principal, no recibe parámetros y está limitado a entregar un solo 
%   resultado. Como cada vez que se ejecuta genera un nuevo mazo, basta con volver a llamarlo en
%   consola para obtener un nuevo resultado independiente del anterior.

%   Esta verión hace un desempate entre manos con figuras iguales, incluso cuando no se tiene 
%   ninguna las ordena de forma decendente según su carta más alta.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

repartir() :-
    mazo(Mazo),
    reparte_n_manos(Mazo,_,10,5,Manos),
    once(figurasManos(Manos,Figuras,10)),
    sort(Figuras,FigurasOrdenadas),
    format('~nGenerando manos...~n'),
    format('~tResultados~t~72|~n~n'),
    format('~`*t ~72|~`*t~n~n'),
    format('~tLugar ~3|~tJugador ~15| ~t~t~t~t       Figura ~25| ~t Mano ~37|~n~n'),
    nth1(1,FigurasOrdenadas,_-Jugador1),
    imprimeJugador(1,Jugador1),
    nth1(2,FigurasOrdenadas,_-Jugador2),
    imprimeJugador(2,Jugador2),
    nth1(3,FigurasOrdenadas,_-Jugador3),
    imprimeJugador(3,Jugador3),
    nth1(4,FigurasOrdenadas,_-Jugador4),
    imprimeJugador(4,Jugador4),
    nth1(5,FigurasOrdenadas,_-Jugador5),
    imprimeJugador(5,Jugador5),
    nth1(6,FigurasOrdenadas,_-Jugador6),
    imprimeJugador(6,Jugador6),
    nth1(7,FigurasOrdenadas,_-Jugador7),
    imprimeJugador(7,Jugador7),
    nth1(8,FigurasOrdenadas,_-Jugador8),
    imprimeJugador(8,Jugador8),
    nth1(9,FigurasOrdenadas,_-Jugador9),
    imprimeJugador(9,Jugador9),
    nth1(10,FigurasOrdenadas,_-Jugador10),
    imprimeJugador(10,Jugador10),
    !.

imprimeJugador(Lugar,[Jugador,Figura,Mano]) :-
    format(' ~a ~t ~a ~22| ~t ~a ~33| ~t~t~t~40|',[Lugar,Jugador,Figura]),
    format('~w ~w ~w ~w ~w ~n~n',Mano).