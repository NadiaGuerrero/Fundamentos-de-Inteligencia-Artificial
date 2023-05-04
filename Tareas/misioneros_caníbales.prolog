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

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%   

%   

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *