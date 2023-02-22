%                       Fundamentos de Inteligencia artificial      
%                           Nadia Itzel Guerrero Sánchez      
%
%   Tarea #1 Árbol genealógico              
%
%   Representar el árbol genealógico que se encuentra en el siguiente sitio web: 
%   https://simpsons.fandom.com/wiki/Simpson_family?file=Simpsons_possible_family_tree.jpg
%
%   NOTA: A fin de comenzar las constantes con letras minúsculas y evitar el uso de espacios, 
%   los nombres están escritos en notación camel case, por ejemplo: monaOlsen, abrahamII.
%
%   Predicados relevantes:
%   Dado que todos los predicados a excepción de desciende/1 tienen versión tanto femenina
%   como masculina, se agrupan para facilitar la lectura. En los parámetros la letra <X> 
%   representa a la persona que se toma como referencia para el parentesco.
%
%   - desciende(<X>,<padre o madre>).
%   - padre_de(<X>,<padre>).                    - madre_de(<X>,<madre>).
%   - abuelo_de(<X>,<abuelo>).                  - abuela de(<X>,<abuela>).
%   - hermano_de(<X>,<hermano>).                - hermana_de(<X>,<hermana>).
%   - medio_hermano_de<X>,<medio hermano>).     - media_hermana_de<X>,<media hermana>).
%   - tío_de(<X>,<tío>).                        - tía_de(<X>,<tía>). 
%   - primo_de(<X>,<primo>).                    - prima_de(<X>,<prima>).
%   - cuñado_de(<X>,<cuñado>).                  - cuñada_de(<X>,<cuñada>).


mujer(mabelSimpson).
mujer(eliza).
mujer(hortense).
mujer(monaOlsen).
mujer(jacquelineGurney).
mujer(abbey).
mujer(margeBouvier).
mujer(selma).
mujer(patty).
mujer(maggie).
mujer(lisa).
mujer(ling).

hombre(virgilSimpson).
hombre(abrahamI).
hombre(hugo).
hombre(oldTut).
hombre(orville).
hombre(chet).
hombre(cyrus).
hombre(tyrone).
hombre(abrahamII).
hombre(clancyBouvier).
hombre(herbert).
hombre(homer).
hombre(bart).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                   desciende/2.    desciende(<padre o madre>, <X>).
%   Especifica de quién desciende <X>, se utiliza principalmente como hecho

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

desciende(mabelSimpson,eliza).
desciende(mabelSimpson,abrahamI).
desciende(virgilSimpson,abrahamI).
desciende(abrahamI,hugo).
desciende(abrahamI,oldTut).
desciende(oldTut,orville).
desciende(orville,chet).
desciende(orville,hortense).
desciende(orville,cyrus).
desciende(orville,tyrone).
desciende(orville,abrahamII).
desciende(abrahamII,herbert).
desciende(abrahamII,abbey).
desciende(abrahamII,homer).
desciende(monaOlsen,homer).
desciende(clancyBouvier,margeBouvier).
desciende(clancyBouvier,selma).
desciende(clancyBouvier,patty).
desciende(jacquelineGurney,margeBouvier).
desciende(jacquelineGurney,selma).
desciende(jacquelineGurney,patty).
desciende(homer,maggie).
desciende(homer,lisa).
desciende(homer,bart).
desciende(margeBouvier,maggie).
desciende(margeBouvier,lisa).
desciende(margeBouvier,bart).
desciende(selma,ling).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                   padre_de/2.    padre_de(<X>,<padre>).
%                   madre_de/2.    madre_de(<X>,<madre>).

%   Basándose en si el segundo parámetro es hombre o mujer, determina
%   si se trata de un padre o una madre.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

padre_de(X,Padre) :-
    hombre(Padre),
    desciende(Padre,X).

madre_de(X,Madre) :-
    mujer(Madre),
    desciende(Madre,X).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                   abuelo_de/2.    abuelo_de(<X>,<abuelo>).
%                   abuela_de/2.    abuela_de(<X>,<abuela>).

%   Verifica si alguno de los padres de <X> desciende del segundo parámetro y
%   determina si se trata de un abuelo o una abuela. 

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

abuelo_de(X,Abuelo) :-
    padre_de(Y,Abuelo),
    desciende(Y,X).
    
abuela_de(X,Abuela) :-
    madre_de(Y,Abuela),
    desciende(Y,X).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                   hermano_de/2.    hermano_de(<X>,<hermano>).
%                   hermana_de/2.    hermana_de(<X>,<hermana>).

%   Encuentra todas las personas con las que comparta al menos un padre.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

hermano_de(X,Hermano) :-
    hombre(Hermano),
    X \== Hermano,
    desciende(Z,X),
    desciende(Z,Hermano).

hermana_de(X,Hermana) :-
    mujer(Hermana),
    X \== Hermana,
    desciende(Z,X),
    desciende(Z,Hermana).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%           medio_hermano_de/2.    medio_hermano_de(<X>,<medio hermano>).
%           media_hermana_de/2.    media_hermana_de(<X>,<media hermana>).

%   Encuentra todos los hermanos con los que comparta únicamente un padre.
%
%   Para que pueda existir esta relación, al menos uno de los dos miembros, ya 
%   sea <X> o <medio hermano> debe tener registro de ambos padres.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

medio_hermano_de(X,MedioHermano) :-
    hermano_de(X,MedioHermano),
    ((madre_de(X,Madre), \+ madre_de(MedioHermano,Madre));
    (padre_de(X,Padre), \+ padre_de(MedioHermano,Padre))).

medio_hermano_de(X,MedioHermano) :-
    hermano_de(X,MedioHermano),
    ((madre_de(MedioHermano,Madre), \+ madre_de(X,Madre));
    (padre_de(MedioHermano,Padre), \+ padre_de(X,Padre))).

media_hermana_de(X,MediaHermana) :-
    hermana_de(X,MediaHermana),
    ((madre_de(X,Madre), \+ madre_de(MediaHermana,Madre));
    (padre_de(X,Padre), \+ padre_de(MediaHermana,Padre))).

media_hermana_de(X,MediaHermana) :-
    hermana_de(X,MediaHermana),
    ((madre_de(MediaHermana,Madre), \+ madre_de(X,Madre));
    (padre_de(MediaHermana,Padre), \+ padre_de(X,Padre))).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                       tío_de/2.    tío_de(<X>,<tío>).
%                       tía_de/2.    tía_de(<X>,<tía>).

%   Relaciona a <X> con los hermanos y cuñados de sus padres.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *


tío_de(X,Tío) :-
    desciende(Z,X),
    hermano_de(Z,Tío).

tío_de(X,Tío) :-
    cuñado_de(Y,Tío),
    desciende(Y,X).

tía_de(X,Tía) :-
    desciende(Z,X),
    hermana_de(Z,Tía).

tía_de(X,Tía) :-
    cuñada_de(Y,Tía),
    desciende(Y,X).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                       primo_de/2.    primo_de(<X>,<primo>).
%                       prima_de/2.    prima_de(<X>,<prima>).

%   Encuentra a todos aquellos que desciendan de un tío, siempre y cuando éste 
%   sea hermano de uno de los padres.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

primo_de(X,Primo) :-
    hombre(Primo),
    (tío_de(X,Tío);tía_de(X,Tío)),
    desciende(Y,X),
    desciende(Z,Y),
    desciende(Z,Tío),
    desciende(Tío,Primo).

prima_de(X,Prima) :-
    mujer(Prima),
    (tío_de(X,Tío);tía_de(X,Tío)),
    desciende(Y,X),
    desciende(Z,Y),
    desciende(Z,Tío),
    desciende(Tío,Prima).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                       cuñado_de/2.    cuñado_de(<X>,<cuñado>).
%                       cuñada_de/2.    cuñada_de(<X>,<cuñada>).

%   Busca a todas las personas que tienen uno o más hijos con alguno de sus hermanos 
%   o son hermanos de alguien con quien tienen al menos un hijo.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

cuñado_de(X,Cuñado) :-
    hermana_de(X,Hermana),
    Hermana \== X,
    desciende(Hermana,Sobrino),
    padre_de(Sobrino,Cuñado).

cuñado_de(X,Cuñado) :-
    desciende(X,Hijo),
    desciende(Z,Hijo),
    X \== Z,
    hermano_de(Z,Cuñado).

cuñada_de(X,Cuñada) :-
    madre_de(Sobrino,Cuñada),
    padre_de(Sobrino,Hermano),
    hermano_de(Hermano,X),
    Hermano \== X.

cuñada_de(X,Cuñada) :-
    desciende(X,Hijo),
    desciende(Z,Hijo),
    X \== Z,
    hermana_de(Z,Cuñada).