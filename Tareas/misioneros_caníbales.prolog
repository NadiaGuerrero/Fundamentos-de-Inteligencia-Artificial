﻿%                                 Fundamentos de Inteligencia artificial      
%                                     Nadia Itzel Guerrero Sánchez      
%
%   Tarea #5 Misioneros y caníbales             
%
%   Resolver el siguiente problema:
%   
%   - 3 misioneros y 3 caníbales deben cruzar un río.
%   - La barca disponible sólo tiene capacidad para 2 ocupantes incluyendo el remero.
%   - Tanto misioneros como caníbales pueden remar.
%   - En ningún momento deben encontrase, en orilla alguna, más caníbales que misioneros (para
%     evitar que ocurra canibalismo).
%
%   Cada estado se representa como una lista:
%
%   [<MisionerosOrigen>,<CaníbalesOrigen>,<MisionerosDestino>,<CaníbalesDestino>,<PosiciónBarca>]
%   
%   Dadas las condiciones del problema, únicamente puede haber 5 movimientos posibles en 
%   cualquiera de las dos direcciones.
%   
%   - Dos misioneros
%   - Un misionero
%   - Un misionero y un caníbal
%   - Dos caníbales
%   - Un caníbal
%
%   Predicados relevantes:
%   - busca_DFS(<EstadoInicial>,<EstadoMeta>,<Plan>).
%   - busca_BFS(<EstadoInicial>,<EstadoMeta>,<Plan>).
%   - busca_IDS(<EstadoInicial>,<EstadoMeta>,<Plan>).
%   - despliega(<Plan>).
%
%   Recomendaciones: Ejecutar una búsqueda seguida del predicado despliega/1.
%   
%   - busca_DFS([3,3,0,0,o],[0,0,3,3,d],P),despliega(P).
%   - busca_BFS([3,3,0,0,o],[1,1,2,2,d],P),despliega(P).
%   - busca_IDS([3,3,0,0,o],[1,1,2,2,d],P),despliega(P).

/* 
cd("D:/ESCOM/IA/Fundamentos-de-Inteligencia-Artificial/Tareas").
["misioneros_caníbales"].
 */
:- use_module( library(clpfd) ).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                              edo_válido/1  edo_válido(<Estado>).

%   Este predicado valida que se tengan 3 caníbales y 3 misioneros en total y que no haya más
%   caníbales que misioneros en ninguna de las dos orillas. Devuelve true cuando se cumplen
%   ambas condiciones.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

edo_válido(Estado) :-
    Estado = [MisionerosOrigen,CaníbalesOrigen,MisionerosDestino,CaníbalesDestino],
    Cantidad = 3, % Se tienen <Cantidad> tanto de misioneros como de caníbales
    Estado ins 0..Cantidad,
    MisionerosOrigen + MisionerosDestino #= Cantidad,
    CaníbalesOrigen + CaníbalesDestino #= Cantidad,
    ((MisionerosOrigen #>= CaníbalesOrigen, MisionerosOrigen #> 0) ; MisionerosOrigen #= 0),
    ((MisionerosDestino #>= CaníbalesDestino, MisionerosDestino #> 0) ; MisionerosDestino #= 0).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                       movimiento/2    movimiento(<Estado1>,<Estado2>).

%   Este predicado recibe un estado cualesquiera, lo valida y devuelve un estado válido  
%   al que se pueda llegar con un solo movimiento. Se puede utilizar de forma no canónica 
%   proporcionando dos variables o dos constantes. Para la forma canónica se requiere de 
%   una variable <Estado1> y una constante <Estado2>, sin embargo el orden se puede invertir 
%   sin afectar al resultado.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

movimiento([MO1,CO1,MD1,CD1,L1],[MO2,CO2,MD2,CD2,L2]) :-

    edo_válido([MO1,CO1,MD1,CD1]),  % Se validan ambos estados para aprovechar las 
    edo_válido([MO2,CO2,MD2,CD2]),  % restricciones en este predicado.

    (

    % Un misionero
    (CO1 = CO2,
    ((MO1 #= MO2 + 1, L1 = o, L2 = d) ; (MO1 #= MO2 - 1, L1 = d, L2 = o))) ;

    % Dos misioneros
    (CO1 = CO2,
    ((MO1 #= MO2 + 2, L1 = o, L2 = d) ; (MO1 #= MO2 - 2, L1 = d, L2 = o))) ;

    % Un misionero y un caníbal
    ((MO1 #= MO2 + 1, CO1 #= CO2 + 1, L1 = o, L2 = d); (MO1 #= MO2 - 1, CO1 #= CO2 - 1, L1 = d, L2 = o)) ;

    % Un caníbal
    (MO1 = MO2,
    ((CO1 #= CO2 + 1, L1 = o, L2 = d) ; (CO1 #= CO2 - 1, L1 = d, L2 = o))) ;

    % Dos caníbales
    (MO1 = MO2,
    ((CO1 #= CO2 + 2, L1 = o, L2 = d) ; (CO1 #= CO2 - 2, L1 = d, L2 = o)))

    ).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                       sucesores/2 sucesores(<Frontera>,<Sucesores>).

%   Este predicado encuentra todos los estados adyacentes al primer elemento de la <Frontera>, 
%   y los guarda en la lista <Sucesores> siempre y cuando estos no hayan sido visitados antes.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

sucesores([Estado|Resto],Sucesores) :-
    findall([S,Estado|Resto],
            (movimiento(Estado,S), \+ member(S,[Estado|Resto])),
            Sucesores).

sucesor([Estado|Resto],[Sucesor,Estado|Resto]) :-
    movimiento(Estado,Sucesor),
    \+ member(Sucesor,[Estado|Resto]).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                               edo_meta/1  edo_meta(<Estado>).

%   Este predicado tiene como objetivo almacenar el estado meta de la búsqueda actual.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

- dynamic(edo_meta/1).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                   busca_DFS/3 busca_DFS(<EstadoInicial>,<EstadoMeta>,<Plan>).

%   Este predicado encuentra una ruta entre el estado inicial y el estado final utilizando la 
%   estrategia de búsqueda DFS (Depth-First Search).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

busca_DFS(EstadoInicial,EstadoMeta,Plan) :-
    retractall( edo_meta( _ ) ),
    assert(edo_meta(EstadoMeta)),
    dfs([[EstadoInicial]],P),
    reverse(P,Plan).

dfs([[EstadoMeta|T]|_],[EstadoMeta|T]) :- edo_meta(EstadoMeta).

%   Utilizando sucesores/2, que saca todos los sucesores al mismo tiempo en una lista
dfs([Candidato|Frontera],Ruta) :-
    sucesores(Candidato,Sucesores),
    append(Sucesores,Frontera,NuevaAgenda),
    dfs(NuevaAgenda,Ruta).

/* %   Utilizando sucesor/2 que los encuentra de forma individual y deja puntos de backtracking
dfs([Candidato|Frontera],Ruta) :-
    sucesor(Candidato,Sucesor),
    NuevaAgenda = [Sucesor|Frontera],
    dfs(NuevaAgenda,Ruta). */

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                   busca_BFS/3 busca_BFS(<EstadoInicial>,<EstadoMeta>,<Plan>).

%   Este predicado encuentra una ruta entre el estado inicial y el estado final utilizando la 
%   estrategia de búsqueda BFS (Breath-First Search).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

busca_BFS(EstadoInicial,EstadoMeta,Plan) :-
    retractall( edo_meta( _ ) ),
    assert(edo_meta(EstadoMeta)),
    bfs([[EstadoInicial]],P),
    reverse(P,Plan).

bfs([[EstadoMeta|T]|_],[EstadoMeta|T]) :- edo_meta(EstadoMeta).

% sucesores/2
bfs([Candidato|Frontera],Ruta) :-
    sucesores(Candidato,Sucesores),
    append(Frontera,Sucesores,NuevaAgenda),
    bfs(NuevaAgenda,Ruta).

/* % sucesor/2 Esta cláusula aprovecha los puntos de backtracking para revisar todas las rutas
bfs([Candidato|Frontera],Ruta) :-
    sucesor(Candidato,Sucesor),
    NuevaAgenda = [Frontera|Sucesor],
    bfs(NuevaAgenda,Ruta). */

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                   busca_IDS/3 busca_IDS(<EstadoInicial>,<EstadoMeta>,<Plan>).

%   Este predicado encuentra una ruta entre el estado inicial y el estado final utilizando la 
%   estrategia de búsqueda IDS (Iterative Deepening Search). Para ello reutiliza el predicado
%   de búsqueda dfs/2. Este algoritmo se comporta de manera similar a BFS.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%   Este primer intento utiliza el "truco" visto en clase, aprovecha el predicado length/2
%   para fijar un límite a la longitud del <Plan>

busca_IDS(EstadoInicial,EstadoMeta,Plan) :-
    retractall( edo_meta( _ ) ),
    assert(edo_meta(EstadoMeta)),
    dfs([[EstadoInicial]],_),       % Repetir esta instrucción con variable anónima permite  
    length(P,_),                    % que el predicado falle si es que no existen rutas.
    dfs([[EstadoInicial]],P),
    reverse(P,Plan).

%   Encuentra las rutas de forma ascendente utilizando el enfoque de Bratko, funciona pero
%   se sigue ciclando una vez que encuentra todas las rutas posibles. 

busca_IDS(EstadoInicial,EstadoMeta,Plan) :-
    retractall( edo_meta( _ ) ),
    assert(edo_meta(EstadoMeta)),
    ids(EstadoInicial,EstadoMeta,P),
    reverse(P,Plan).

ids(Estado,Estado,[Estado]).

ids(EstadoInicial,EstadoFinal,[EstadoFinal|Plan]) :-
    ids(EstadoInicial,PenúltimoEstado,Plan),
    movimiento(PenúltimoEstado,EstadoFinal),
    \+ member(EstadoFinal,Plan).

%   IDS Bratko
%   Este es el código tal cual viene en el libro, se utilizó a manera de referencia

s(a, b).
s(a, c).
s(b, d).
s(b, e).
s(d, h).
s(d, i).
s(e, j).
s(e, k).
s(c, f).
s(c, g).
s(f, l).
s(f, m).
s(g, n).
s(g, o).

goal(n).

depth_first_iterative_deepening(Node, Solution):-
	path(Node, GoalNode,Solution),
	goal(GoalNode).

path(Node, Node, [Node]).

path(FirstNode, LastNode, [LastNode|Path]) :- 
    path(FirstNode, OneButLast, Path),
    s(OneButLast, LastNode),
    not(member(LastNode, Path)).

% Estas cláusulas devuelven la ruta en orden pero se sigue ciclando

path(FirstNode, LastNode, [FirstNode|Path]) :- 
    path(SecondNode, LastNode, Path),
    s(FirstNode, SecondNode),
    not(member(FirstNode, Path)).

%   Esta versión no se cicla pero entrega las cláusulas en orden descendente; primero las de 
%   mayor tamaño y luego las más pequeñas, es como un IDS pero al revés y al igual que el anterior, 
%   entrega la ruta en orden, por lo que ya no es necesario invertirla.

path(FirstNode, LastNode, [FirstNode|Path]) :- 
    s(FirstNode, SecondNode),    
    path(SecondNode, LastNode, Path),
    not(member(FirstNode, Path)).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                               despliega/1 despliega(<Plan>).

%   Este predicado únicamente despliega los movimientos de un plan en forma de lista, requiere
%   una búsqueda para funcionar. Se puede realizar con cualquiera de los tres algoritmos.

%   Ejemplos de consultas:
%   - busca_DFS([3,3,0,0,o],[0,0,3,3,d],P),despliega(P).
%   - busca_BFS([3,3,0,0,o],[1,1,2,2,d],P),despliega(P).
%   - busca_IDS([3,3,0,0,o],[1,1,2,2,d],P),despliega(P).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

despliega(Plan) :-
    format('~n~n~tÉxito, meta encontrada. ~72|~n'),
    length(Plan,L), Pasos #= L - 1,
    format('~tSolución con ~w pasos entre los estados: ~n~t- ',[Pasos]), 
    Plan = [EstadoInicial|_],
    append(_,[EstadoFinal],Plan),
    imprimirEstado(EstadoInicial),
    format('~t~t- '),
    imprimirEstado(EstadoFinal),
    format('~n~tInicio en '),
    imprimirEstado(EstadoInicial),
    format('~n'),
    once(imprimirPasos(1,Plan)).

imprimirEstado([MO,CO,MD,CD,Barca]) :-
    ((PosiciónBarca = 'Origen', Barca = o) ;
    (PosiciónBarca = 'Destino', Barca = d)), 
    format('((~w ~w) (~w ~w) ~w)~n',[MO,CO,MD,CD,PosiciónBarca]).

imprimirPasos(Num,[E1,E2]) :-
    format('~t~w) Moviendo ',[Num]),
    imprimirMovimiento(E1,E2),
    format(' se llega a '),
    imprimirEstado(E2),
    format('~n~n').

imprimirPasos(Num,[E1,E2|Resto]) :-
    format('~t~w) Moviendo ',[Num]),
    imprimirMovimiento(E1,E2),
    format(' se llega a '),
    imprimirEstado(E2),
    NumS #= Num + 1,
    imprimirPasos(NumS,[E2|Resto]).

imprimirMovimiento([MO1,CO1,_,_,_],[MO2,CO2,_,_,_]) :-
    % Un misionero
    (CO1 = CO2,
    ((MO1 #= MO2 + 1) ; (MO1 #= MO2 - 1)),
    format('           un misionero         '))
    ;

    % Dos misioneros
    (CO1 = CO2,
    ((MO1 #= MO2 + 2) ; (MO1 #= MO2 - 2)),
    format('          dos misioneros        '))
    ;

    % Un misionero y un caníbal
    (((MO1 #= MO2 + 1, CO1 #= CO2 + 1);
    (MO1 #= MO2 - 1, CO1 #= CO2 - 1)),
    format('    un misionero y un caníbal   '))
    ;

    % Un caníbal
    (MO1 = MO2,
    ((CO1 #= CO2 + 1) ; (CO1 #= CO2 - 1)),
    format('           un caníbal           '))
    ;

    % Dos caníbales
    (MO1 = MO2,
    ((CO1 #= CO2 + 2) ; (CO1 #= CO2 - 2)),
    format('          dos caníbales         ')).