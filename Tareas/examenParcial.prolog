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

:- use_module( library(clpfd) ).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%      navegar/4  navegar(<EstaciónInicial>,<EstaciónDestino>,<EstacionesVisitadas>,<Ruta>).

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

navegar(EstaciónInicial,EstaciónDestino,_,[EstaciónInicial,EstaciónDestino]) :-
    EstaciónInicial \== EstaciónDestino,                       
    (sigue(EstaciónInicial,EstaciónDestino,_);        % Verifica que las estaciones sean      
    sigue(EstaciónDestino,EstaciónInicial,_)).        % contiguas sin importar el orden.

navegar(EstaciónInicial,EstaciónDestino,EstacionesVisitadas,[EstaciónInicial|Ruta]) :-
    EstaciónInicial \== EstaciónDestino,                       
    (sigue(EstaciónInicial,X,_);
    sigue(X,EstaciónInicial,_)),
    EstaciónInicial \== X,
    \+ member(X,EstacionesVisitadas),
    navegar(X,EstaciónDestino,[X|EstacionesVisitadas],Ruta).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                   ruta/3  ruta(<EstaciónInicial>,<EstaciónDestino>,<Ruta>).

%   Este es un predicado de interfaz para navegar/4, recibe los nombres de dos estaciones y
%   devuelve una <Ruta> a seguir, que es una lista de estaciones.

%   Al igual que  navegar/4, se puede utilizar de formas no canónicas con las mismas 
%   limitaciones.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

ruta(EstaciónInicial,EstaciónDestino,Ruta) :-
    navegar(EstaciónInicial,EstaciónDestino,[EstaciónInicial],Ruta).
    
grado(Estación,Grado) :-
    findall(EstaciónAdyacente,sigue(Estación,EstaciónAdyacente,_),A),
    findall(EstaciónAdyacente,sigue(EstaciónAdyacente,Estación,_),B),
    length(A,LA), length(B,LB),
    Grado #= LA + LB.

misma_línea(EstaciónInicial,EstaciónDestino) :-
    (sigue(EstaciónInicial,_,Línea);
    sigue(_,EstaciónInicial,Línea)),
    (sigue(EstaciónDestino,_,Línea);
    sigue(_,EstaciónDestino,Línea)),!.

tiempo_arista([_],_,0-no).

tiempo_arista([EstaciónDestino|_],EstaciónDestino,TiempoArista-no) :-
    valor_parámetro(tiempo_tramo,TiempoTramo),
    grado(EstaciónDestino,Grado),
    TiempoArista #= TiempoTramo * Grado.
    
tiempo_arista(Ruta,EstaciónDestino,TiempoArista-no) :-
    append(_,[EstaciónDestino],Ruta),
    valor_parámetro(tiempo_tramo,TiempoTramo),
    grado(EstaciónDestino,Grado),
    TiempoArista #= TiempoTramo * Grado.

tiempo_arista(Ruta,EstaciónDestino,TiempoArista-no) :-
    append(_,[EstaciónPrevia,EstaciónDestino,EstaciónSiguiente|_],Ruta),
    misma_línea(EstaciónPrevia,EstaciónSiguiente),
    valor_parámetro(tiempo_tramo,TiempoTramo),
    grado(EstaciónDestino,Grado),
    TiempoArista #= TiempoTramo * Grado.

tiempo_arista(Ruta,EstaciónDestino,TiempoArista-Transborde) :-
    append(_,[EstaciónPrevia,EstaciónDestino,EstaciónSiguiente|_],Ruta),
    \+ misma_línea(EstaciónPrevia,EstaciónSiguiente),
    valor_parámetro(tiempo_transbordo,TiempoTransborde),
    valor_parámetro(tiempo_tramo,TiempoTramo),
    grado(EstaciónDestino,Grado),
    TiempoArista #= TiempoTransborde + (TiempoTramo * Grado),
    (sigue(EstaciónSiguiente,EstaciónDestino,Transborde);
    sigue(EstaciónDestino,EstaciónSiguiente,Transborde)).

tiempo_ruta(Ruta,TiempoAristas,TiempoTotal) :-
    Ruta = [_|Aristas],
    maplist(tiempo_arista(Ruta),Aristas,TiempoAristas),
    suma_tiempos(TiempoAristas,TiempoTramos),
    valor_parámetro(tiempo_inicial,TiempoInicial),
    valor_parámetro(tiempo_final,TiempoFinal),
    TiempoTotal #= TiempoTramos + TiempoInicial + TiempoFinal.

suma_tiempos([TiempoActual-_],TiempoTotal) :-
    TiempoTotal = TiempoActual.

suma_tiempos([TiempoActual-_|TiempoAristas],TiempoTotal) :-
    TiempoTotal #= TiempoAnterior + TiempoActual,
    suma_tiempos(TiempoAristas,TiempoAnterior).