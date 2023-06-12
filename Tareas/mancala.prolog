%                                 Fundamentos de Inteligencia artificial      
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
    Tablero =   [C1,C2,C3,C4,C5,C6,BaseJ1,
                C7,C8,C9,C10,C11,C12,BaseJ2],

    imprimeSangría(),
    imprimeRenglónCasilla(1,BaseJ1,RestoBaseJ1),
    imprimeRenglónCasilla(1,C6,RC6),
    imprimeRenglónCasilla(1,C5,RC5),
    imprimeRenglónCasilla(1,C4,RC4),
    imprimeRenglónCasilla(1,C3,RC3),
    imprimeRenglónCasilla(1,C2,RC2),
    imprimeRenglónCasilla(1,C1,RC1),
    imprimeRenglónCasilla(2,BaseJ2,RestoBaseJ2),

    Resto = [RC1,RC2,RC3,RC4,RC5,RC6,RestoBaseJ1,
            C7,C8,C9,C10,C11,C12,RestoBaseJ2],

    nl.

imprimeRenglónTablero(2,Tablero,Resto) :-
    Tablero =   [C1,C2,C3,C4,C5,C6,BaseJ1,
                C7,C8,C9,C10,C11,C12,BaseJ2],

    imprimeSangría(),
    imprimeRenglónCasilla(1,BaseJ1,RestoBaseJ1),
    imprimeRenglónCasilla(2,C7,RC7),
    imprimeRenglónCasilla(2,C8,RC8),
    imprimeRenglónCasilla(2,C9,RC9),
    imprimeRenglónCasilla(2,C10,RC10),
    imprimeRenglónCasilla(2,C11,RC11),
    imprimeRenglónCasilla(2,C12,RC12),
    imprimeRenglónCasilla(2,BaseJ2,RestoBaseJ2),

    Resto = [C1,C2,C3,C4,C5,C6,RestoBaseJ1,
            RC7,RC8,RC9,RC10,RC11,RC12,RestoBaseJ2],

    nl.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                           imprimeTablero/1  imprimeTablero(<Tablero>).

%   Este predicado recibe cualquier tablero y lo imprime a color, la estructura de un tablero
%   es la siguiente:

%   - Tablero = [C1,C2,C3,C4,C5,C6, BaseJ1, C7,C8,C9,C10,C11,C12, BaseJ2].
%               Casillas Jugador 1          Casillas Jugador 2

%   Cada jugador tiene 6 casillas y una base, todas se representan como una lista de tres
%   elementos que contiene la cantidad de fichas amarillas, verdes y rojas que hay en la 
%   casilla o base respectivamente.

%   Por ejemplo: [1,2,5] significa que hay 1 ficha amarilla, 2 verdes y 5 rojas.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

imprimeTablero(Tablero) :- nl,

    imprimeLeyenda(superior),
    imprimeBorde(superior),

    imprimeRenglónTablero(1,Tablero,Resto1),
    imprimeRenglónTablero(1,Resto1,Resto2),
    imprimeRenglónTablero(1,Resto2,Resto3),
    imprimeRenglónTablero(1,Resto3,Resto4),

    nth1(7,Resto4,BaseJ1,Contexto1),
    nth1(7,Resto5,R3,Contexto1),
    nth1(14,Resto4,BaseJ2,Contexto2),
    nth1(14,Resto5,R4,Contexto2),

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
    Tablero =   [[0,0,0],[0,0,0],[0,0,0],[0,0,0],[0,0,0],[0,0,0], % Casillas Jugador 1
                [0,0,0],    % Base Jugador 1
                [0,0,0],[0,0,0],[0,0,0],[0,0,0],[0,0,0],[0,0,0],  % Casillas Jugador 2
                [0,0,0]].   % Base Jugador 2

tableroInicial(Tablero) :-
    Tablero =   [[1,1,1],[1,1,1],[1,1,1],[1,1,1],[1,1,1],[1,1,1], % Casillas Jugador 1
                [0,0,0],    % Base Jugador 1
                [1,1,1],[1,1,1],[1,1,1],[1,1,1],[1,1,1],[1,1,1],  % Casillas Jugador 2
                [0,0,0]].   % Base Jugador 2

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                aumentarFicha/3  aumentarFicha(<Color>,<Casilla>,<NuevaCasilla>).

%   Agrega una ficha del color indicado a la casilla seleccionada. Los posibles colores son:
%   amarillo (a), verde (v) y rojo (r), aunque también se puede enviar una x para indicar
%   que esa casilla se debe omitir.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

aumentarFicha(x,Casilla,Casilla).

aumentarFicha(Color,Casilla,NuevaCasilla) :-
    Color \= x,

    ((Color = a, Posición = 1);
    (Color = v, Posición = 2);
    (Color = r, Posición = 3)),

    nth1(Posición,Casilla,Cantidad,Resto),
    nth1(Posición,NuevaCasilla,NuevaCantidad,Resto),

    NuevaCantidad #= Cantidad + 1.
    
%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%       modificarTablero/1  modificarTablero(<Casilla>,<Jugada>,<Tablero>,<NuevoTablero>).

%   Este predicado auxiliar recibe una lista de colores, <Jugada> y reparte esas fichas en 
%   las casillas posteriores a <Casilla>.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
modificarTablero(Casilla,Jugada,Tablero,NuevoTablero) :-
    % Permite hacer jugadas con casillas que contengan más de 12 fichas
    TamañoJugada #> 13,
    length(Jugada,TamañoJugada),

    TamañoExtra #= TamañoJugada - 13,
    length(Extra,TamañoExtra),
    append(NuevaJugada,Extra,Jugada),

    modificarTablero(Casilla,Extra,Tablero,NT),
    modificarTablero(Casilla,NuevaJugada,NT,NuevoTablero).

modificarTablero(Casilla,Jugada,Tablero,NuevoTablero) :-
    TamañoJugada #< 14,

    % Agrega x para hacer que la lista de jugadas coincida con el tamaño 
    % del tablero para poder emplear maplist/4
    length(Tablero,TamañoTablero),
    length(Jugada,TamañoJugada),
    TamañoRelleno #= TamañoTablero - TamañoJugada,
    length(Relleno, TamañoRelleno),
    maplist(=(x),Relleno),
    append(Jugada,Relleno,JugadaCompleta),

    % Divide el tablero en dos secciones y las intercambia de lugar para
    % que las casillas que reciben fichas queden al principio del tablero
    length(T1,Casilla),    
    append(T1,T2,Tablero),
    append(T2,T1,TableroReordenado),

    maplist(aumentarFicha,JugadaCompleta,TableroReordenado,Resultado),

    % Regresa el tablero (ya modificado) a su orden original
    length(T4,Casilla),
    append(T3,T4,Resultado),
    append(T4,T3,NuevoTablero).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%       jugada/5  jugada(<JugadorEnTurno>,<Casilla>,<Jugada>,<Tablero>,<NuevoTablero>).

%   Este predicado es el que refleja las jugadas dentro del tablero, requiere del número del
%   jugador en turno, la casilla de la que se repartirán las fichas y una jugada, que es una 
%   lista que contene el orden en que se repartirán las fichas.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

jugada(JugadorEnTurno,Casilla,Jugada,Tablero,NuevoTablero) :-
    Casilla in 1..6,

    Tablero = [C1,C2,C3,C4,C5,C6, BaseJ1, C7,C8,C9,C10,C11,C12, BaseJ2],

    ((JugadorEnTurno = 1, T1 = [C1,C2,C3,C4,C5,C6,BaseJ1,C7,C8,C9,C10,C11,C12]) ;
    (JugadorEnTurno = 2, T1 = [C7,C8,C9,C10,C11,C12,BaseJ2,C1,C2,C3,C4,C5,C6])),

    nth1(Casilla,T1,[Amarillas,Verdes,Rojas]),
    contarFichas(Jugada,a,Amarillas), 
    contarFichas(Jugada,v,Verdes),
    contarFichas(Jugada,r,Rojas),
    CantidadFichas #= Amarillas + Verdes + Rojas,
    length(Jugada,CantidadFichas),

    nth1(Casilla,T1,_,Resto),
    nth1(Casilla,T,[0,0,0],Resto),

    modificarTablero(Casilla,Jugada,T,NT),
    
    ((JugadorEnTurno = 1, append(NT,[BaseJ2],NuevoTablero)) ;
    (JugadorEnTurno = 2, NT = [NC7,NC8,NC9,NC10,NC11,NC12,NBaseJ2,NC1,NC2,NC3,NC4,NC5,NC6],
    NuevoTablero = [NC1,NC2,NC3,NC4,NC5,NC6,BaseJ1,NC7,NC8,NC9,NC10,NC11,NC12,NBaseJ2])).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                  contarFichas/3  contarFichas(<Lista>,<Elemento>,<Cantidad>).

%   Este predicado auxiliar cuenta la cantidad de veces que aparece un elemento dentro de una
%   lista.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

contarFichas([],_,0).
contarFichas([X|Resto],X,Y) :-
    contarFichas(Resto,X,Z),
    Y is 1+Z.

contarFichas([Otro|Resto],X,Z):- 
    Otro \= X,
    contarFichas(Resto,X,Z).