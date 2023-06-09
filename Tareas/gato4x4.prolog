%                                 Fundamentos de Inteligencia artificial      
%                                     Nadia Itzel Guerrero Sánchez      
%
%   Tarea #7 Agente jugador de gato 4X4           
%
%   Construir un agente jugador de gato 4X4 que sea capaz de jugar contra un oponente humano.
%   Debe utilizar el algoritmo MiniMax con podas α-β.
%
%   Predicados relevantes:
%   - 
%
%   Recomendaciones:
%   

:- use_module(library(clpfd)).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                       símbolo/2  símbolo(<Identificador>,<Símbolo>).

%   Es un predicado auxiliar que sirve para almacenar y recuperar los símbolos válidos dentro  
%   del juego, además de asociarlos a un número.

%                         símboloVacío/2  símboloVacío(<Símbolo>).

%   Indica qué símbolo se va a utilizar para representar que una casilla está vacía.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

símbolo(1, x).
símbolo(2, o).

símboloVacío(-). % Indica que la casilla está desocupada

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                           tableroVacío/1  tableroVacío(<Tablero>).

%   Genera un tablero en el que todas las casillas están desocupadas.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

tableroVacío(Tablero) :-
    símboloVacío(Símbolo),
    Renglón = [Símbolo, Símbolo, Símbolo, Símbolo],
    Tablero = [Renglón, Renglón, Renglón, Renglón].

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                           imprimirTablero/1  imprimirTablero(<Tablero>).

%   Este predicado recibe un tablero y lo imprime en pantalla en forma de texto, las columnas 
%   están etiquetadas con números del 1 al 4 y los renglones con letras de la A a la D.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

imprimirTablero(Tablero) :-
    Tablero = [A,B,C,D], % Cada letra es una fila del tablero

    format('~n           1     2     3     4~n~n'),
    format('    A   |  ~w  |  ~w  |  ~w  |  ~w  |~n',A),
    format('        +-----+-----+-----+-----+~n'),
    format('    B   |  ~w  |  ~w  |  ~w  |  ~w  |~n',B),
    format('        +-----+-----+-----+-----+~n'),
    format('    C   |  ~w  |  ~w  |  ~w  |  ~w  |~n',C),
    format('        +-----+-----+-----+-----+~n'),
    format('    D   |  ~w  |  ~w  |  ~w  |  ~w  |~n~n',D).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                              siguiente_turno/1  siguiente_turno(<>).

%   Este es un predicado auxiliar que registra la secuencia de turnos intercalados del juego.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

siguiente_turno(1, 2).
siguiente_turno(2, 1).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%           jugada/2  jugada((<Tablero>,<Jugador>),(<NuevoTablero>,<NuevoJugador>)).

%   Este predicado es el encargado de expadir los estados. En su forma canónica recibe un 
%   tablero y un número de jugador, valida el número, recupera su símbolo y devuelve una de 
%   los posibles movimientos que puede hacer el jugador en turno acompañado del número del 
%   jugador siguiente.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

jugada((Tablero,Jugador),(NuevoTablero,NuevoJugador)) :-
    siguiente_turno(Jugador,NuevoJugador),
    símbolo(Jugador,Símbolo),
    símboloVacío(Vacío),
    
    nth1(NumRenglón,Tablero,Renglón,RestoTablero),
    nth1(NumRenglón,NuevoTablero,NuevoRenglón,RestoTablero),
    nth1(Posición,Renglón,Vacío,RestoRenglón),
    nth1(Posición,NuevoRenglón,Símbolo,RestoRenglón).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                           victoria/2  victoria(<Tablero>,<Ganador>).

%   Valida que exista una fila, columna o diagonal de símbolos iguales y devuelve el número 
%   del jugador al que corresponde el símbolo ganador. Es no determinístico y puede detectar 
%   más de una victoria (incluso de jugadores distintos) encaso de que se cumpla la condición 
%   mas de una vez.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

victoria(Tablero,Ganador) :-
    Tablero = [A,B,C,D],

    (
    (member([S,S,S,S],Tablero)); % Horizontal
    (nth1(P,A,S), nth1(P,B,S), nth1(P,C,S), nth1(P,D,S)); % Vertical
    (nth1(1,A,S), nth1(2,B,S), nth1(3,C,S), nth1(4,D,S)); % Diagonal de izquierda a derecha
    (nth1(4,A,S), nth1(3,B,S), nth1(2,C,S), nth1(1,D,S))  % Diagonal de derecha a izquierda
    ),
    
    símbolo(Ganador,S).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                                   empate/1  empate(<Tablero>).

%   Valida que exista un empate en el tablero que se le proporciona. Para ello es necesario 
%   que en TODAS las filas, columnas y diagonales haya símbolos de ambos jugadores, lo que 
%   imposibilita que cualquiera de los dos gane.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

empate(Tablero) :-

    símbolo(1,Jugador1),
    símbolo(2,Jugador2),

    % Deben existir ambos símbolos en todas las filas
    maplist(member(Jugador1),Tablero),
    maplist(member(Jugador2),Tablero),

    transpose(Tablero,TableroTranspuesto),
    maplist(member(Jugador1),TableroTranspuesto),
    maplist(member(Jugador2),TableroTranspuesto),

    Tablero =   [[I1,_,_,D4],
                [_,I2,D3,_],
                [_,D2,I3,_],
                [D1,_,_,I4]],

    DiagonalI = [I1,I2,I3,I4],
    DiagonalD = [D1,D2,D3,D4],

    member(Jugador1,DiagonalI), member(Jugador2,DiagonalI),
    member(Jugador1,DiagonalD), member(Jugador2,DiagonalD).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                    estadoActual/2  estadoActual(<Tablero>,<JugadorEnTurno>).

%   Esta es la representación de los estados del juego, contiene el tablero y jugador en turno.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

:- dynamic(estadoActual/2).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                       ventaja/3  ventaja(<Tablero>,<Jugador>,<Ventaja>).

%   <Ventaja> es la cantidad de líneas verticales, horizontales y diagonales en las que el 
%   jugador tiene oportunidad de ganar y su oponente no (las líneas vacías no se cuentan).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

ventaja(Tablero,Jugador,Ventaja) :-

    Tablero =   [[I1,_,_,D4],
                [_,I2,D3,_],
                [_,D2,I3,_],
                [D1,_,_,I4]],

    DiagonalI = [I1,I2,I3,I4],
    DiagonalD = [D1,D2,D3,D4],
    
    siguiente_turno(Jugador,Oponente),
    símbolo(Jugador,Símbolo),
    símbolo(Oponente,SímboloOponente),

    % Esta consulta encuentra los renglones en los que sólo hay símbolos del jugador en turno
    findall(R,(member(R,Tablero),once(member(Símbolo,R)),(\+member(SímboloOponente,R))),Renglones),
    length(Renglones,CantidadRenglones),

    transpose(Tablero,TableroTranspuesto),
    findall(C,(member(C,TableroTranspuesto),once(member(Símbolo,C)),(\+member(SímboloOponente,C))),Columnas),
    length(Columnas,CantidadColumnas),

    findall(D,(member(D,[DiagonalI,DiagonalD]),once(member(Símbolo,D)),(\+member(SímboloOponente,D))),Diagonales),
    length(Diagonales,CantidadDiagonales),

    Ventaja #= CantidadRenglones + CantidadColumnas + CantidadDiagonales.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                   aptitud/3  aptitud(<Tablero>,<JugadorEnTurno>,<Aptitud>).

%   <Aptitud> es la diferencia entre la ventaja del jugador en turno y el jugador siguiente.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

aptitud(Tablero,JugadorEnTurno,Aptitud) :-

    siguiente_turno(JugadorEnTurno,JugadorSiguiente),

    ventaja(Tablero,JugadorEnTurno,VentajaJugador),
    ventaja(Tablero,JugadorSiguiente,VentajaOponente),
    
    Aptitud #= VentajaJugador - VentajaOponente.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                           horizonte/1  horizonte(<Horizonte>).

%   Determina la profundidad de las búsquedas que realizará el agente jugador para decidir sus
%   jugadas.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

:- dynamic(horizonte/1).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%   negamax/1  negamax(<>).

%   Es el predicado encargado de realiza la búsqueda y seleccionar la mejor jugada, es una 
%   modificación del algoritmo minimax e incluye podas a-b para reducir el efecto horizonte.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

negamax(Tablero,JugadorEnTurno,ProfundidadActual,MejorMovimiento) :-
    (/* victoria(Tablero,_);
    empate(Tablero); */
    (horizonte(Horizonte), ProfundidadActual >= Horizonte))

   ->   MejorMovimiento = Tablero
   
   ;    (Alfa = -10,
        jugada((Tablero,JugadorEnTurno),(NuevoTablero,NuevoJugador)),
        NuevaProfundidad #= ProfundidadActual + 1,
        negamax(NuevoTablero,NuevoJugador,NuevaProfundidad,NuevoMovimiento),
        aptitud(NuevoMovimiento,JugadorEnTurno,Aptitud),
        Valor #= Aptitud * -1,

        ((Valor #> Alfa, MejorMovimiento = NuevoMovimiento)/* ;
        (Valor #=< Alfa, MejorMovimiento = Tablero) */)

        ).




minimax(Profundidad,Siguiente_jugada,Evaluación) :-
    estadoActual(Tablero,Jugador),
    horizonte(Horizonte),
    Profundidad < Horizonte,
    N is Profundidad + 1,
    listar_jugadas((Tablero,Jugador),Jugadas),
    mejor_jugada(Jugadas,N,Siguiente_jugada,Evaluación),!.

minimax(_,_,Evaluación) :-
    estadoActual(Tablero,Jugador),
    aptitud(Tablero,Jugador,Evaluación).

mejor_jugada([Jugada],Profundidad,Jugada,Evaluación) :-
    minimax(Profundidad,_,Evaluación),!.

mejor_jugada([Jugada_1|Jugadas],Profundidad,Mejor_jugada,Mejor_evaluación) :-
    minimax(Profundidad,_,Evaluación_1),
    mejor_jugada(Jugadas,Profundidad,Jugada_2,Evaluación_2),
    mejor_entre(Jugada_1,Evaluación_1,Jugada_2,Evaluación_2,Mejor_jugada,Mejor_evaluación).

mejor_entre(Jugada_1,Evaluación_1,_,Evaluación_2,Jugada_1,Evaluación_1) :-
    Jugada_1 = (_,Jugador),
    (min(Jugador),
    Evaluación_1 >= Evaluación_2, ! ;
    max(Jugador),
    Evaluación_1 =< Evaluación_2, !).

mejor_entre(_,_,Jugada,Evaluación,Jugada,Evaluación).

listar_jugadas(Estado,Jugadas) :-
    findall((NuevoTablero,NuevoJugador),jugada(Estado,(NuevoTablero,NuevoJugador)),Jugadas).






inicia_juego(Horizonte) :-
    retractall(horizonte(_)),
    assert(horizonte(Horizonte)),

    retractall(estadoActual(_,_)),
    tableroVacío(TableroVacío),
    assert(estadoActual(TableroVacío,1)),

    símbolo(1,SímboloHumano),
    símbolo(2,SímboloAgente),
    
    format('~n~n~tBienvenido al gato 4X4 ~60|~n'),
    format('
    A continuación algunos puntos importantes:

    - Para tirar deberás utilizar el predicado tiro(<Renglón>,<Columna>), los renglones 
    se indican con letras minúsculas y las columnas con números.
    - El jugador 1 es quien inicia todos los juegos.
    - Tú eres el jugador ~w y tu símbolo es ~w.
    - Yo soy el jugador ~w y mi símbolo es ~w.
    - Después de cada turno te mostraré el tablero.
    - Cuando termine el juego imprimiré el tablero e indicaré el resultado final.

    Buena suerte y que gane el mejor :D
    ',[1,SímboloHumano,2,SímboloAgente]),
    
    estadoActual(Tablero,1),
    imprimirTablero(Tablero).

tiro(LetraRenglón,Columna) :-
    renglón(LetraRenglón,Renglón),
    estadoActual(Tablero,JugadorEnTurno),
    símbolo(JugadorEnTurno,Símbolo),
    
    nth1(Renglón,Tablero,R,RestoTablero),
    nth1(Columna,R,_,RestoRenglón),
    
    nth1(Renglón,NuevoTablero,NR,RestoTablero),
    nth1(Columna,NR,Símbolo,RestoRenglón),
    
    jugada((Tablero,JugadorEnTurno),(NuevoTablero,NuevoJugador)),
    retractall(estadoActual(_,_)),
    assert(estadoActual(NuevoTablero,NuevoJugador)),
    
    ((victoria(NuevoTablero,_); empate(NuevoTablero))
     -> once(anunciarResultado(NuevoTablero))
     ;  tiroAgente()).

tiroAgente() :-
    estadoActual(Tablero,JugadorEnTurno),
    agente(JugadorEnTurno),

    negamax(Tablero,JugadorEnTurno,0,NuevoTablero),

    jugada((Tablero,JugadorEnTurno),(NuevoTablero,NuevoJugador)),
    retractall(estadoActual(_,_)),
    assert(estadoActual(NuevoTablero,NuevoJugador)),
    
    ((victoria(NuevoTablero,_); empate(NuevoTablero))
     -> once(anunciarResultado(NuevoTablero))
     ;  tiroAgente()).

anunciarResultado(Tablero) :-
    victoria(Tablero,Ganador),

    ((humano(Ganador),
    format('
    Felicidades, humano
    Ganaste la partida C:~n~n'));
    
    (agente(Ganador),
    format('
    Lo siento mucho, yo gané
    Más suerte para la próxima UuU~n~n'))),

    imprimirTablero(Tablero).

anunciarResultado(Tablero) :-
    once(empate(Tablero)),

    format('
    Ni tú, ni yo, esto es un empate~n~n'),

    imprimirTablero(Tablero).

    renglón(a,1).
    renglón(b,2).
    renglón(c,3).
    renglón(d,4).

humano(1).
agente(2).