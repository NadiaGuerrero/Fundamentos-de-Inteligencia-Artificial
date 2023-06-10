﻿%                                 Fundamentos de Inteligencia artificial      
%                                     Nadia Itzel Guerrero Sánchez      
%
%   PROYECTO FINAL Agente jugador de mancala           
%
%   Construir un agente jugador de mancala que sea capaz de jugar contra un oponente humano.
%   Debe utilizar al menos una heurística auxiliar, ya sea propia o existente.

/* 
cd("D:/ESCOM/IA/Fundamentos-de-Inteligencia-Artificial/Tareas").
[mancala].
 */

:- use_module(library(ansi_term)).
:- use_module(library(clpfd)).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                         colorJugador/2  colorJugador(<Jugador>,<Color>).

%   Indica de qué color será el tablero de cada jugador.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

colorJugador(1,blue).
colorJugador(2,magenta).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                                   ficha/1  ficha(<Caracter>).

%   Establece el caracter que se empleará para representar las fichas dentro del tablero.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

ficha('⬤').

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                                           IMPRIMIR

%   A continuación se presenta una lista de predicados auxiliares que se utilizan para 
%   imprimir los diversos elementos del tablero.

%   - imprimeLeyenda/1  imprimeLeyenda(<Posición>).
%   - imprimeSangría/0  imprimeSangría().
%   - imprimeEspacio/1  imprimeEspacio(<Cantidad>).
%   - imprimeBorde/2 imprimeBorde(<Jugador>,<Texto>).
%   - imprimeBorde/1 imprimeBorde(<Posición>).
%   - imprimeFichas/2 imprimeFichas(<Color>,<Cantidad>).
%   - imprimeRenglónCasilla/3 imprimeRenglónCasilla(<Jugador>,<Fichas>,<Resto>).
%   - imprimeRenglónTablero/3 RenglónTablero(<Jugador>,<Tablero>,<Resto>).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

imprimeLeyenda(superior) :-
    format('     Jugador 1       6           5           4           3           2           1       Jugador 2'), nl.

imprimeLeyenda(inferior) :-
    format('     Jugador 1       1           2           3           4           5           6       Jugador 2'), nl.

imprimeSangría() :-
    format('    ').

imprimeEspacio(0).

imprimeEspacio(Cantidad) :-
    Cantidad #> 0,
    format('  '),
    NuevaCantidad #= Cantidad - 1,
    imprimeEspacio(NuevaCantidad).

imprimeBorde(Jugador,Texto) :- 
    colorJugador(Jugador,ColorTexto),
    ansi_format([bold,fg(ColorTexto)],Texto,[]).

imprimeBorde(centro,Jugador) :-
    imprimeBorde(Jugador,' ---------   ---------   ---------   ---------   ---------   ---------  ').

imprimeBorde(superior) :-
    imprimeSangría(),
    imprimeBorde(1,' ---------   ---------   ---------   ---------   ---------   ---------   ---------'),
    imprimeBorde(2,'   ---------'), nl.

imprimeBorde(inferior) :-
    imprimeSangría(),
    imprimeBorde(1,' ---------'),
    imprimeBorde(2,'   ---------   ---------   ---------   ---------   ---------   ---------   ---------'), nl.

imprimeFichas(_,0).

imprimeFichas(Color,Cantidad) :-
    Cantidad #> 0,
    ficha(Ficha),
    ansi_format([fg(Color)],"~w ",[Ficha]),
    NuevaCantidad #= Cantidad - 1,
    imprimeFichas(Color,NuevaCantidad).

imprimeRenglónCasilla(Jugador,Fichas,[0,0,0]) :-
    Fichas = [Amarillas,Verdes,Rojas],
    Amarillas + Verdes + Rojas #=< 4, % 4 es el ancho de las casillas, por lo que no puede haber más de esa cantidad en un solo renglón
    
    imprimeBorde(Jugador,'| '),

    once(imprimeFichas(yellow,Amarillas)),
    once(imprimeFichas(green,Verdes)),
    once(imprimeFichas(red,Rojas)),

    Espacios #= 4 - (Amarillas + Verdes + Rojas),
    once(imprimeEspacio(Espacios)),

    imprimeBorde(Jugador,'| ').

imprimeRenglónCasilla(Jugador,Fichas,Resto) :-
    Fichas = [Amarillas,Verdes,Rojas],
    Fichas ins 0..12,
    Amarillas + Verdes + Rojas #> 4,

    Renglón = [NuevasAmarillas,NuevasVerdes,NuevasRojas],
    Renglón ins 0..4,
    NuevasAmarillas + NuevasVerdes + NuevasRojas #= 4,

    Resto = [RestoAmarillas,RestoVerdes,RestoRojas],
    Resto ins 0..12,
    Amarillas #= NuevasAmarillas + RestoAmarillas,
    Verdes #= NuevasVerdes + RestoVerdes,
    Rojas #= NuevasRojas + RestoRojas,
    
    imprimeBorde(Jugador,'| '),

    once(imprimeFichas(yellow,NuevasAmarillas)),
    once(imprimeFichas(green,NuevasVerdes)),
    once(imprimeFichas(red,NuevasRojas)),

    imprimeBorde(Jugador,'| ').

imprimeRenglónTablero(1,Tablero,Resto) :-
    Tablero = [CasillasJ1,BaseJ1,CasillasJ2,BaseJ2],
    CasillasJ1 = [C1,C2,C3,C4,C5,C6],

    imprimeSangría(),
    imprimeRenglónCasilla(1,BaseJ1,RestoBaseJ1),
    imprimeRenglónCasilla(1,C1,RC1),
    imprimeRenglónCasilla(1,C2,RC2),
    imprimeRenglónCasilla(1,C3,RC3),
    imprimeRenglónCasilla(1,C4,RC4),
    imprimeRenglónCasilla(1,C5,RC5),
    imprimeRenglónCasilla(1,C6,RC6),
    imprimeRenglónCasilla(2,BaseJ2,RestoBaseJ2),

    Resto = [[RC1,RC2,RC3,RC4,RC5,RC6],RestoBaseJ1,CasillasJ2,RestoBaseJ2],

    nl.

imprimeRenglónTablero(2,Tablero,Resto) :-
    Tablero = [CasillasJ1,BaseJ1,CasillasJ2,BaseJ2],
    CasillasJ2 = [C1,C2,C3,C4,C5,C6],

    imprimeSangría(),
    imprimeRenglónCasilla(1,BaseJ1,RestoBaseJ1),
    imprimeRenglónCasilla(2,C1,RC1),
    imprimeRenglónCasilla(2,C2,RC2),
    imprimeRenglónCasilla(2,C3,RC3),
    imprimeRenglónCasilla(2,C4,RC4),
    imprimeRenglónCasilla(2,C5,RC5),
    imprimeRenglónCasilla(2,C6,RC6),
    imprimeRenglónCasilla(2,BaseJ2,RestoBaseJ2),

    Resto = [CasillasJ1,RestoBaseJ1,[RC1,RC2,RC3,RC4,RC5,RC6],RestoBaseJ2],

    nl.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                           imprimeTablero/1  imprimeTablero(<Tablero>).

%   Este predicado recibe cualquier tablero y lo imprime a color, la estructura de un tablero
%   es la siguiente:

%   - Tablero = [CasillasJ1, BaseJ1, CasillasJ2, BaseJ2].

%   Cada jugador tiene 6 casillas y una base, las dos se representan como una lista de tres
%   elementos que contiene la cantidad de fichas amarillas, verdes y rojas que hay en la 
%   casilla o base respectivamente. Por ejemplo: [1,2,5].

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

imprimeTablero(Tablero) :- nl,

    imprimeLeyenda(superior),
    imprimeBorde(superior),

    imprimeRenglónTablero(1,Tablero,Resto1),
    imprimeRenglónTablero(1,Resto1,Resto2),
    imprimeRenglónTablero(1,Resto2,Resto3),
    imprimeRenglónTablero(1,Resto3,Resto4),

    Resto4 = [CasillasJ1,BaseJ1,CasillasJ2,BaseJ2],
    Resto5 = [CasillasJ1,R3,CasillasJ2,R4],

    imprimeSangría(),imprimeRenglónCasilla(1,BaseJ1,R1),imprimeBorde(centro,1),imprimeRenglónCasilla(2,BaseJ2,R2), nl,

    imprimeSangría(),imprimeRenglónCasilla(1,R1,R3),imprimeBorde(centro,2),imprimeRenglónCasilla(2,R2,R4), nl,

    imprimeRenglónTablero(2,Resto5,Resto6),
    imprimeRenglónTablero(2,Resto6,Resto7),
    imprimeRenglónTablero(2,Resto7,Resto8),
    imprimeRenglónTablero(2,Resto8,_),

    imprimeBorde(inferior),
    imprimeLeyenda(inferior),
    
    nl.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%   tableroVacío/1  tableroVacío(<Tablero>).    tableroInicial/1  tableroInicial(<Tablero>).

%   Estos predicados sirven para recuperar un tablero concreto, ya sea vacío o con las fichas 
%   en su posición inicial.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

tableroVacío(Tablero) :-
    Tablero =   [[[0,0,0],[0,0,0],[0,0,0],[0,0,0],[0,0,0],[0,0,0]], % Casillas Jugador 1
                [0,0,0],    % Base Jugador 1
                [[0,0,0],[0,0,0],[0,0,0],[0,0,0],[0,0,0],[0,0,0]],  % Casillas Jugador 2
                [0,0,0]].   % Base Jugador 2

tableroInicial(Tablero) :-
    Tablero =   [[[1,1,1],[1,1,1],[1,1,1],[1,1,1],[1,1,1],[1,1,1]], % Casillas Jugador 1
                [0,0,0],    % Base Jugador 1
                [[1,1,1],[1,1,1],[1,1,1],[1,1,1],[1,1,1],[1,1,1]],  % Casillas Jugador 2
                [0,0,0]].   % Base Jugador 2