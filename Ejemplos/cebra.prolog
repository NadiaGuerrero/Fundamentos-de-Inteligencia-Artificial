vecindario(V) :-
V = [_,_,_,_,_], % 1
member(casa(rojo,inglaterra,_,_,_), V), % 2
member(casa(_,españa,perro,_,_), V), % 3
member(casa(verde,_,_,café,_), V), % 4
member(casa(_,ukrania,_,te,_), V), % 5
junto(casa(blanco,_,_,_,_),casa(verde,_,_,_,_), V), % 6
member(casa(_,_,caracol,_,oldgold), V), % 7
member(casa(amarillo,_,_,_,kool), V), % 8
V = [_,_,casa(_,_,_,leche,_),_,_], % 9
V = [casa(_,noruega,_,_,_) | _ ], % 10
junto(casa(_,_,_,_,chesterfield), casa(_,_,zorro,_,_), V), % 11
junto(casa(_,_,_,_,kool), casa(_,_,caballo,_,_), V), % 12
member(casa(_,_,_,jugo,luckystrike), V), % 13
member(casa(_,japón,_,_,parliaments), V), % 14
junto(casa(_,noruega,_,_,_), casa(azul,_,_,_,_),V), % 15
member(casa(_,_,_,agua,_),V), % Alguien bebe agua...
member(casa(_,_,cebra,_,_),V). % Alguien tiene una cebra...

junto(C1,C2,Lista):- append(_, [C1,C2|_], Lista).
junto(C1,C2,Lista):- append(_, [C2,C1|_], Lista).