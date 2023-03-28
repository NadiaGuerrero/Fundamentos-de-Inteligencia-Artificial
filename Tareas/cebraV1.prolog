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

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                   visualiza_vecindario/1  visualiza_vecindario(<Vecindario>).

%   Es el predicado principal, contiene todas las pistas que conforman al problema de manera
%   que <Vecindario> unifique con cada una.

%                               casa/2  casa(<Color>,<Nacionalidad>).

%   Se utiliza para representar las casas del vecindario, cada una cuenta con dos atributos:
%   el color de la casa y la nacionalidad de su ocupante. Todas las casas están contenidas en
%   una lista ordenada que representa al vecindario.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

visualiza_vecindario(V) :-
    V = [_,_,_],
    junto(casa(_,español),casa(rojo,_),V), % 1)
    member(casa(azul,noruego),V),           % 2)
    V = [_,casa(_,italiano),_].             % 3)

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                       junto/3  junto(<Casa1>,<Casa2>,<Vecindario>).

%   Valida que dos elementos pertenecientes a una lista sean adyacentes.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

junto(C1,C2,V) :- append(_,[C1,C2|_],V).
junto(C1,C2,V) :- append(_,[C2,C1|_],V).