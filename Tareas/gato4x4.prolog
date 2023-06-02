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

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                       símbolo/2  símbolo(<Identificador>,<Símbolo>).

%   Es un predicado auxiliar que sirve para almacenar y recuperar los símbolos válidos dentro  
%   del juego, además de asociarlos a un número. El 1 y 2 son los jugadores, mientras que el 
%   0 es el símbolo que estará en las casillas desocupadas.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

símbolo(0, -). % Indica que la casilla está desocupada
símbolo(1, x).
símbolo(2, o).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                           tableroVacío/1  tableroVacío(<Tablero>).

%   Genera un tablero en el que todas las casillas están desocupadas.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

tableroVacío(Tablero) :-
    símbolo(0,Símbolo),
    Renglón = [Símbolo, Símbolo, Símbolo, Símbolo],
    Tablero = [Renglón, Renglón, Renglón, Renglón].

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                           imprimirTablero/1  imprimirTablero(<Tablero>).

%   Este predicado recibe un tablero y lo imprime en pantalla en forma de texto, las columnas 
%   están etiquetadas con números del 1 al 4 y los renglones con letras de la A a la D.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

imprimirTablero(Tablero) :-
    Tablero = [A,B,C,D], % Cada letra es una fila del tablero

    format('~n       1     2     3     4~n~n'),
    format('A   |  ~w  |  ~w  |  ~w  |  ~w  |~n',A),
    format('    +-----+-----+-----+-----+~n'),
    format('B   |  ~w  |  ~w  |  ~w  |  ~w  |~n',B),
    format('    +-----+-----+-----+-----+~n'),
    format('C   |  ~w  |  ~w  |  ~w  |  ~w  |~n',C),
    format('    +-----+-----+-----+-----+~n'),
    format('D   |  ~w  |  ~w  |  ~w  |  ~w  |~n~n',D).

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
    símbolo(0,Vacío),
    
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
    
    símbolo(Ganador,S), 
    (Ganador = 1 ; Ganador = 2).

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

    \+ victoria(Tablero,_).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                    estadoActual/2  estadoActual(<Tablero>,<JugadorEnTurno>).

%   Esta es la representación de los estados del juego, verdadero cuando el tablero es una 
%   lista de 4 listas con 4 elementos cada una y el jugador en turno es un número recuperado
%   con el predicado símbolo/2, ya sea 1 o 2.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

estadoActual(Tablero,JugadorEnTurno) :-
    Tablero = [[_,_,_,_],[_,_,_,_],[_,_,_,_],[_,_,_,_]],
    símbolo(JugadorEnTurno,_),
    JugadorEnTurno \= 0.
