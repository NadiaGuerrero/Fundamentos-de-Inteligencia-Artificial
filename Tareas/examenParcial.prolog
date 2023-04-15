%                                 Fundamentos de Inteligencia artificial      
%                                     Nadia Itzel Guerrero Sánchez      
%
%   Examen parcial Metro CdMX              
%
%   Utilizando el conocimiento recibido sobre la red del metro de la CDMX, programar los 
%   siguientes predicados:
%
%   1) mejor_ruta(<origen>, <destino>, <mejor-ruta>, <tiempo>).
%       
%       Encuentra la o las rutas con el menor tiempo de traslado entre dos estaciones
%       tomando en cuenta el tiempo necesario para ingresar a la primera estación, el 
%       trayecto entre estaciones y los transbordes entre líneas del metro.
%       Devuelve la ruta en forma de lista de estaciones y el tiempo total en formato 
%       de horas y minutos.
%
%   2) reporte_tiempo(<origen>, <destino>).
%
%       Imprime en la consola un reporte detallado del tiempo que toma el viaje para cada
%       par de estaciones tomando en cuenta transbordes y el grado de cada estación. También
%       incluye el tiempo necesario para ingresar y salir del metro.
%
%   3) reporte_simplificado(<origen>, <destino>).
%
%       Imprime en la consola instrucciones en lenguaje natural para la mejor ruta entre dos
%       estaciones, únicamente proporciona la primera y última estación de cada tramo y la 
%       dirección. Un tramo es un conjunto de estaciones que se deben recorrer sin realizar
%       transbordes.
%
%   Los últimos dos predicados reportan sus resultados directamente en consola.
%
%   Predicados relevantes:
%   - mejor_ruta(<origen>, <destino>, <mejor-ruta>, <tiempo>).
%   - reporte_tiempo(<origen>, <destino>).
%   - reporte_simplificado(<origen>, <destino>).
%
%   Recomendaciones: 
%

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%      navegar/4  navegar(<EstaciónInicial>,<EstaciónFinal>,<EstacionesVisitadas>,<Ruta>).

%   Este predicado encuentra rutas hamiltonianas entre dos estaciones. En su forma canónica
%   recibe como parámetro las dos estaciones y la lista de aquellas que ya fueron visitadas. 
%   Es necesario que <EstacionesVisitadas> contenga a <EstaciónInicial> para evitar que se 
%   haya repeticiones. El predicado devuelve una lista que es la ruta a seguir.

%   Se puede emplear de formas no canónicas si se colocan variables en el lugar de las 
%   estaciones y/o se especifica una ruta válida. Sin embargo, en caso de no proporcionar 
%   valores constantes para las estaciones, es posible que haya repetición de estaciones 
%   dentro de las rutas, por lo que ya no serán estrictamente hamiltonianas.

%   También se pueden especificar estaciones por las que no se desee pasar al agregarlas a 
%   la lista <EstacionesVisitadas>.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

navegar(Estación,Estación,_,[Estación]).

navegar(EstaciónInicial,EstaciónFinal,_,[EstaciónInicial,EstaciónFinal]) :-
    EstaciónInicial \== EstaciónFinal,                       
    (sigue(EstaciónInicial,EstaciónFinal,_);        % Verifica que las estaciones sean      
    sigue(EstaciónFinal,EstaciónInicial,_)).        % contiguas sin importar el orden.

navegar(EstaciónInicial,EstaciónFinal,EstacionesVisitadas,[EstaciónInicial|Ruta]) :-
    EstaciónInicial \== EstaciónFinal,                       
    (sigue(EstaciónInicial,X,_);
    sigue(X,EstaciónInicial,_)),
    EstaciónInicial \== X,
    \+ member(X,EstacionesVisitadas),
    navegar(X,EstaciónFinal,[X|EstacionesVisitadas],Ruta).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                   ruta/3  ruta(<EstaciónInicial>,<EstaciónFinal>,<Ruta>).

%   Este es un predicado de interfaz para navegar/4, recibe los nombres de dos estaciones y
%   devueve una <Ruta> a seguir, que es una lista de estaciones.

%   Al igual que  navegar/4, se puede utilizar de formas no canónicas con las mismas 
%   limitaciones.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

ruta(EstaciónInicial,EstaciónFinal,Ruta) :-
    navegar(EstaciónInicial,EstaciónFinal,[EstaciónInicial],Ruta).