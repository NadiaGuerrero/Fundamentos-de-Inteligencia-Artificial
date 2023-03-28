%                                 Fundamentos de Inteligencia artificial      
%                                     Nadia Itzel Guerrero Sánchez      
%
%   Tarea #3 Acertijos tipo cebra              
%
%   Resolver una versión recortada del problema de la cebra con sólo 3 casas con 4 atributos
%   y 6 pistas:
%
%   1) El brasileño NO vive en la segunda casa
%   2) El dueño de perros juega baloncesto
%   3) Hay una casa intermedia entre la del que juega fútbol y la casa roja
%   4) El dueño de peces vive junto al dueño de gatos
%   5) El dueño de perros vive junto a la casa verde
%   6) Un alemán vive en la tercera casa
%
%   NOTA: Se escogió un enfoque que mezcla las restricciones con el uso de member para 
%   mejorar la legibilidad de la respuesta, pues al utilizar únicamente números enteros
%   los valores se agrupan por categoría, no por casa. Además de que en la salida no se 
%   muestran los nombres de las variables y esto hace que sea difícil identificar las  
%   relaciones de atributos para una misma casa.
%
%   Predicados relevantes:
%   - visualiza_vecindario(<Vecindario>).
%
%   Recomendaciones: Utilizar la siguiente consulta para obtener un resultado fácil de leer.
%   - visualiza_vecindario(V),print_term(V,[]).

:- use_module( library(clpfd) ).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%               casa/5  casa(<Posición>,<Color>,<Nacionalidad>,<Deporte>,<Mascota>).

%   Se utiliza para representar las casas del vecindario, cada una cuenta con 5 atributos:
%   la posición de la casa, su color, la nacionalidad de su ocupante, el deporte que éste
%   practica y el animal que tiene de mascota. Todas las casas están contenidas en una lista
%   ordenada que representa al vecindario.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                   visualiza_vecindario/1  visualiza_vecindario(<Vecindario>).

%   Es el predicado principal, contiene todas las pistas que conforman al problema de manera
%   que <Vecindario> unifique con cada una.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

visualiza_vecindario(V) :-
    V = [casa(1,_,_,_,_),casa(2,_,_,_,_),casa(3,_,_,_,_)],

    member(casa(PosiciónBrasil,_,brasileño,_,_),V),         % 1)
    PosiciónBrasil #\= 2,

    member(casa(PosiciónPerros,_,_,baloncesto,perros),V),   % 2)

    member(casa(PosiciónFútbol,_,_,fútbol,_),V),            % 3)
    member(casa(PosiciónRojo,rojo,_,_,_),V),
    ((PosiciónFútbol #= PosiciónRojo + 2) #\/
    (PosiciónFútbol #= PosiciónRojo - 2)),

    member(casa(PosiciónPeces,_,_,_,peces),V),              % 4)
    member(casa(PosiciónGatos,_,_,_,gatos),V),
    ((PosiciónPeces #= PosiciónGatos + 1) #\/
    (PosiciónPeces #= PosiciónGatos - 1)),

    member(casa(PosiciónVerde,verde,_,_,_),V),              % 5)
    ((PosiciónPerros #= PosiciónVerde + 1) #\/
    (PosiciónPerros #= PosiciónVerde - 1)),
    
    member(casa(3,_,alemán,_,_),V).                         % 6)