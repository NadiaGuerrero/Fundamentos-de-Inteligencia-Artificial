%                                 Fundamentos de Inteligencia artificial      
%                                     Nadia Itzel Guerrero Sánchez      
%
%   Tarea #3 Acertijos tipo cebra              
%
%   Resolver una versión recortada del problema de la cebra con sólo 3 casas con 2 atributos
%   y 3 pistas:
%
%   1) El español vive junto a la casa roja
%   2) El noruego vive en la casa azul
%   3) Un italiano vive en la segunda casa
%
%   Predicados relevantes:
%   - visualiza_vecindario(<Vecindario>).
%   

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

visualiza_vecindario(V) :-
    V = [_,_,_],
    vecino(casa(_,español),casa(rojo,_),V),
    member(casa(azul,noruego),V),
    V = [_,casa(_,italiano),_].

vecino(C1,C2,V) :- append(_,[C1,C2|_],V).
vecino(C1,C2,V) :- append(_,[C2,C1|_],V).