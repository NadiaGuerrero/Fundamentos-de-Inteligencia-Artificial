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

%   imprime/1  imprime(<>).

%   

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

imprimeLeyenda() :-
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

imprimeRenglónCasilla(Jugador,Fichas) :-
    Fichas = [Amarillas,Verdes,Rojas],
    Amarillas + Verdes + Rojas #=< 4, % 4 es el ancho de las casillas, por lo que no puede haber más de esa cantidad en un solo renglón
    
    imprimeBorde(Jugador,'| '),

    once(imprimeFichas(yellow,Amarillas)),
    once(imprimeFichas(green,Verdes)),
    once(imprimeFichas(red,Rojas)),

    Espacios #= 4 - (Amarillas + Verdes + Rojas),
    once(imprimeEspacio(Espacios)),

    imprimeBorde(Jugador,'| ').

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%   s/1  s(<>).

%   

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

imprimeTablero() :- nl,
    imprimeLeyenda(),
    imprimeBorde(superior),

    imprimeSangría(),imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(1,[0,0,0]),
    imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(1,[0,0,0]),
    imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]), nl,

    imprimeSangría(),imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(1,[0,0,0]),
    imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(1,[0,0,0]),
    imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]), nl,

    imprimeSangría(),imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(1,[0,0,0]),
    imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(1,[0,0,0]),
    imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]), nl,

    imprimeSangría(),imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(1,[0,0,0]),
    imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(1,[0,0,0]),
    imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]), nl,

    imprimeSangría(),imprimeRenglónCasilla(1,[0,0,0]),imprimeBorde(centro,1),imprimeRenglónCasilla(2,[0,0,0]), nl,

    imprimeSangría(),imprimeRenglónCasilla(1,[0,0,0]),imprimeBorde(centro,2),imprimeRenglónCasilla(2,[0,0,0]), nl,

    imprimeSangría(),imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]),
    imprimeRenglónCasilla(2,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]),
    imprimeRenglónCasilla(2,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]), nl,

    imprimeSangría(),imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]),
    imprimeRenglónCasilla(2,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]),
    imprimeRenglónCasilla(2,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]), nl,

    imprimeSangría(),imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]),
    imprimeRenglónCasilla(2,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]),
    imprimeRenglónCasilla(2,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]), nl,

    imprimeSangría(),imprimeRenglónCasilla(1,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]),
    imprimeRenglónCasilla(2,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]),
    imprimeRenglónCasilla(2,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]),imprimeRenglónCasilla(2,[0,0,0]), nl,

    imprimeBorde(inferior),
    imprimeLeyenda().