%   Consulta:
%   vecindario(V),V = [Países,Colores,Mascotas,Bebidas,Cigarros].

:- use_module( library(clpfd) ).

vecindario(V) :-
    Variables = [   Inglés, Español, Ukraniano, Japonés, Noruego,
                    Rojo, Verde, Amarillo, Azul, Blanco,
                    Perro, Caracol, Zorro, Caballo, Cebra,
                    Té, Café, Leche, Jugo, Agua,
                    Kools, Chesterfields, Parliaments, LuckyStrike, OldGold ],
    
    Variables ins 1..5, % Cada una de las 5 casas...

    V = [Países,Colores,Mascotas,Bebidas,Cigarros],

    Países = [Inglés, Español, Ukraniano, Japonés, Noruego], all_distinct(Países),
    Colores = [Rojo, Verde, Amarillo, Azul, Blanco], all_distinct(Colores),
    Mascotas = [Perro, Caracol, Zorro, Caballo, Cebra], all_distinct(Mascotas),
    Bebidas = [Té, Café, Leche, Jugo, Agua], all_distinct(Bebidas),
    Cigarros = [Kools, Chesterfields, Parliaments, LuckyStrike, OldGold], all_distinct(Cigarros),

    Inglés #= Rojo,
    Español #= Perro,
    Café #= Verde,
    Ukraniano #= Té,
    (Verde #= Blanco - 1) #\/ (Verde #= Blanco + 1),
    OldGold #= Caracol,
    Amarillo #= Kools,
    Leche #= 3,
    Noruego #= 1,
    (Chesterfields #= Zorro-1) #\/ (Chesterfields #= Zorro+1),
    (Caballo #= Kools-1) #\/ (Caballo #= Kools+1),
    LuckyStrike #= Jugo,
    Japonés #= Parliaments,
    (Noruego #= Azul-1) #\/ (Noruego #= Azul+1),

    maplist(label(),V).