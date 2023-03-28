%                                 Fundamentos de Inteligencia artificial      
%                                     Nadia Itzel Guerrero Sánchez      
%
%   Tarea #3 Acertijos tipo cebra              
%
%   Resolver una versión recortada del problema de la cebra con sólo 4 casas con 4 atributos
%   y 9 pistas:
%
%   1) Hay dos casas entre la del bolichista y la del nadador
%   2) Hay una casa entre la del irlandés y la del que juega voleyball
%   3) La segunda casa es negra
%   4) Hay una casa entre la del dueño de caballos y la casa roja
%   5) Un escocés vive junto al dueño de tortugas
%   6) Hay dos casas entre la del dueño de caballos y la casa del dueño de mariposas
%   7) El bolichista vive en algún lugar posterior a la casa del tenista
%   8) Hay una casa entre la del que juega voleyball y la casa blanca
%   9) Un ruso vive en la primera casa
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
    V = [casa(1,_,_,_,_),casa(2,_,_,_,_),casa(3,_,_,_,_),casa(4,_,_,_,_)],

    member(casa(PosiciónBoliche,_,_,boliche,_),V),          % 1)
    member(casa(PosiciónNatación,_,_,natación,_),V),
    ((PosiciónBoliche #= PosiciónNatación + 3) #\/
    (PosiciónBoliche #= PosiciónNatación - 3)),

    member(casa(PosiciónIrlandés,_,irlandés,_,_),V),        % 2)
    member(casa(PosiciónVolley,_,_,voleyball,_),V),
    ((PosiciónIrlandés #= PosiciónVolley + 2) #\/
    (PosiciónIrlandés #= PosiciónVolley - 2)),

    member(casa(2,negro,_,_,_),V),                          % 3)

    member(casa(PosiciónCaballos,_,_,_,caballos),V),        % 4)
    member(casa(PosiciónRojo,rojo,_,_,_),V),
    ((PosiciónCaballos #= PosiciónRojo + 2) #\/
    (PosiciónCaballos #= PosiciónRojo - 2)),

    member(casa(PosiciónTortugas,_,_,_,tortugas),V),        % 5)
    member(casa(PosiciónEscocés,_,escocés,_,_),V),
    ((PosiciónTortugas #= PosiciónEscocés + 1) #\/
    (PosiciónTortugas #= PosiciónEscocés - 1)),
    
    member(casa(PosiciónMariposas,_,_,_,mariposas),V),      % 6)
    ((PosiciónCaballos #= PosiciónMariposas + 3) #\/
    (PosiciónCaballos #= PosiciónMariposas - 3)),

    member(casa(PosiciónTenis,_,_,tenis,_),V),              % 7)
    PosiciónBoliche #>  PosiciónTenis,

    member(casa(PosiciónBlanco,blanco,_,_,_),V),            % 8)
    ((PosiciónVolley #= PosiciónBlanco + 2) #\/
    (PosiciónVolley #= PosiciónBlanco - 2)),
    
    member(casa(1,_,ruso,_,_),V).                           % 9)