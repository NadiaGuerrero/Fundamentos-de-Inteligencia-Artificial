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

:- consult('laberinto-01.prolog').