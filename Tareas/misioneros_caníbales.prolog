%                                 Fundamentos de Inteligencia artificial      
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
%   Recomendaciones: 
%   

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
    ((MO1 #= MO2 + 1, L1 = o, L2 = d) ;
    (MO1 #= MO2 - 1, L1 = d, L2 = o)))
    ;

    % Dos misioneros
    (CO1 = CO2,
    ((MO1 #= MO2 + 2, L1 = o, L2 = d) ;
    (MO1 #= MO2 - 2, L1 = d, L2 = o)))
    ;

    % Un misionero y un caníbal
    ((MO1 #= MO2 + 1, CO1 #= CO2 + 1, L1 = o, L2 = d);
    (MO1 #= MO2 - 1, CO1 #= CO2 - 1, L1 = d, L2 = o))
    ;

    % Un caníbal
    (MO1 = MO2,
    ((CO1 #= CO2 + 1, L1 = o, L2 = d) ;
    (CO1 #= CO2 - 1, L1 = d, L2 = o)))
    ;

    % Dos caníbales
    (MO1 = MO2,
    ((CO1 #= CO2 + 2, L1 = o, L2 = d) ;
    (CO1 #= CO2 - 2, L1 = d, L2 = o)))

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

dfs([Candidato|Frontera],Ruta) :-
    sucesores(Candidato,Sucesores),
    append(Sucesores,Frontera,NuevaAgenda),
    dfs(NuevaAgenda,Ruta).

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

bfs([Candidato|Frontera],Ruta) :-
    sucesores(Candidato,Sucesores),
    append(Frontera,Sucesores,NuevaAgenda),
    bfs(NuevaAgenda,Ruta).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                   busca_IDS/3 busca_IDS(<EstadoInicial>,<EstadoMeta>,<Plan>).

%   Este predicado encuentra una ruta entre el estado inicial y el estado final utilizando la 
%   estrategia de búsqueda IDS (Iterative Deepening Search). Para ello reutiliza el predicado
%   de búsqueda dfs/2. Este algoritmo se comporta de manera similar a BFS.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

busca_IDS(EstadoInicial,EstadoMeta,Plan) :-
    retractall( edo_meta( _ ) ),
    assert(edo_meta(EstadoMeta)),
    dfs([[EstadoInicial]],_),   % Repetir esta instrucción con variable anónima permite que el 
    length(P,_),                % predicado falle si es que no existen rutas.
    dfs([[EstadoInicial]],P),
    reverse(P,Plan).