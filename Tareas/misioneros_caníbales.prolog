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