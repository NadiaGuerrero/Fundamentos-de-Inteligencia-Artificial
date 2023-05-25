%                                 Fundamentos de Inteligencia artificial      
%                                     Nadia Itzel Guerrero Sánchez      
%
%   Tarea #6 Laberinto            
%
%   Solucionar el laberinto proporcionado por el profesor aplicando tres tipos de búsqueda
%   BestFirst-Search considerando el número de niveles descendidos como la función de costo
%   y la distancia Manhattan como función de aptitud:
%   
%   - UniformCost 
%   - Greedy
%   - A*
%
%   Programar también el predicado intercambia_inicio_meta/0 para invertir la dirección de
%   la ruta intercambiando el punto de partida y la meta.
%
%   Predicados relevantes:
%   - buscaCosto/3
%   - buscaAptitud/3
%   - buscaA*/3
%   - intercambia_inicio_meta/0
%
%   Recomendaciones:
%   
%   - Para intercambiar el punto de partida con el estado meta: intercambia_inicio_meta().

:- consult('laberinto-01.prolog').

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                    intercambia_inicio_meta()/0  intercambia_inicio_meta().

%   Este predicado intercambia el estado inicial con el estado meta del laberinto y no recibe
%   ningún parámetro.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

intercambia_inicio_meta() :-
    estado_inicial(Inicial),
    estado_meta(Meta),
    assert(estado_inicial(Meta)),
    assert(estado_meta(Inicial)),
    retractall(estado_inicial(Inicial)),
    retractall(estado_meta(Meta)).