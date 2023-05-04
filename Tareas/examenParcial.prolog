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

%      navegar/4  navegar(<EstaciónOrigen>,<EstaciónDestino>,<EstacionesVisitadas>,<Ruta>).

%   Este predicado encuentra rutas hamiltonianas entre dos estaciones. En su forma canónica
%   recibe como parámetro las dos estaciones y la lista de aquellas que ya fueron visitadas. 
%   Es necesario que <EstacionesVisitadas> contenga a <EstaciónOrigen> para evitar que se 
%   haya repeticiones. El predicado devuelve una lista que es la ruta a seguir.

%   Se puede emplear de formas no canónicas si se colocan variables en el lugar de las 
%   estaciones y/o se especifica una ruta válida. Sin embargo, en caso de no proporcionar 
%   valores constantes para las estaciones, es posible que haya repetición de estaciones 
%   dentro de las rutas, por lo que ya no serán estrictamente hamiltonianas.

%   También se pueden especificar estaciones por las que no se desee pasar al agregarlas a 
%   la lista <EstacionesVisitadas>.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

navegar(Estación,Estación,_,[Estación]).

navegar(EstaciónOrigen,EstaciónDestino,_,[EstaciónOrigen,EstaciónDestino]) :-
    EstaciónOrigen \== EstaciónDestino,                       
    (sigue(EstaciónOrigen,EstaciónDestino,_);        % Verifica que las estaciones sean      
    sigue(EstaciónDestino,EstaciónOrigen,_)).        % contiguas sin importar el orden.

navegar(EstaciónOrigen,EstaciónDestino,EstacionesVisitadas,[EstaciónOrigen|Ruta]) :-
    EstaciónOrigen \== EstaciónDestino,
    \+ sigue(EstaciónOrigen,EstaciónDestino,_),
    \+ sigue(EstaciónDestino,EstaciónOrigen,_),    
    (sigue(EstaciónOrigen,X,_);
    sigue(X,EstaciónOrigen,_)),
    \+ member(X,EstacionesVisitadas),
    navegar(X,EstaciónDestino,[X|EstacionesVisitadas],Ruta).

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                   ruta/3  ruta(<EstaciónOrigen>,<EstaciónDestino>,<Ruta>).

%   Este es un predicado de interfaz para navegar/4, recibe los nombres de dos estaciones y
%   devuelve una <Ruta> a seguir, que es una lista de estaciones.

%   Al igual que  navegar/4, se puede utilizar de formas no canónicas con las mismas 
%   limitaciones.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

ruta(EstaciónOrigen,EstaciónDestino,Ruta) :-
    navegar(EstaciónOrigen,EstaciónDestino,[EstaciónOrigen],Ruta).
    
%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                           grado/2 grado(<Estación>,<Grado>).

%   Recibe una <Estación> y encuentra la cantidad de estaciones adyacentes a ella, es decir,
%   el <Grado>.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
    
grado(Estación,Grado) :-
    findall(EstaciónAdyacente,sigue(Estación,EstaciónAdyacente,_),A),
    findall(EstaciónAdyacente,sigue(EstaciónAdyacente,Estación,_),B),
    length(A,LA), length(B,LB),
    Grado #= LA + LB.
    
%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%               misma_línea/2   misma_línea(<EstaciónOrigen>,<EstaciónDestino>).

%   Verifica que las dos estaciones proporcionadas pertenezcan a la misma línea del metro.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

misma_línea(EstaciónOrigen,EstaciónDestino) :-
    (sigue(EstaciónOrigen,_,Línea);
    sigue(_,EstaciónOrigen,Línea)),
    (sigue(EstaciónDestino,_,Línea);
    sigue(_,EstaciónDestino,Línea)),!.
    
%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%          tiempo_arista/3 tiempo_arista(<Ruta>,<EstaciónDestino>,<TiempoArista-Transborde>).

%   Calcula el tiempo que tomará el trayecto entre dos estaciones contiguas de una misma ruta, 
%   para ello recibe la ruta completa en forma de lista y la estación destino, que es a la que  
%   se pretende llegar, el tercer argumento es un par ordenado que contiene la cantidad de 
%   minutos que se necesitan para llegar a la estación destino. El segundo elemento indica si  
%   hubo transborde en ese tramo. En caso de que no lo haya, <Transborde> tendrá el valor 'no', 
%   si sí lo hubo, <Transborde> será el nombre de la línea a la que se transbordó.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

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
    
%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%               tiempo_ruta/3   tiempo_ruta(<Ruta>,<TiempoAristas>,<TiempoTotal>).

%   Calcula el tiempo total que tomará recorrer una ruta completa, incluyendo los tiempos
%   inicial y final. Recibe una ruta y devuelve dos resultados: <TiempoAristas> y <TiempoTotal>
%   el primero es una lista de pares ordenados que contiene el tiempo que toma cada tramo de 
%   dos estaciones y si es que existe algún transborde. Por su parte <TiempoTotal> es un número
%   entero conformado por la suma de los valores de los tramos y los tiempos inicial y final.

%   Se considera que el tiempo para ir de una estación a sí misma es 0 y evidentemente no hay 
%   transbordes.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

tiempo_ruta([_],[0-no],0).

tiempo_ruta(Ruta,TiempoAristas,TiempoTotal) :-
    Ruta = [_|Aristas],
    maplist(tiempo_arista(Ruta),Aristas,TiempoAristas),
    suma_tiempos(TiempoAristas,TiempoTramos),
    valor_parámetro(tiempo_inicial,TiempoInicial),
    valor_parámetro(tiempo_final,TiempoFinal),
    TiempoTotal #= TiempoTramos + TiempoInicial + TiempoFinal.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

%                 suma_tiempos/2  suma_tiempos(<TiempoAristas>,<TiempoTotal>).

%   Este predicado recibe una lista de pares ordenados y devuelve la suma de todas las keys
%   de la lista.

%   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

suma_tiempos([TiempoActual-_],TiempoTotal) :-
    TiempoTotal = TiempoActual.

suma_tiempos([TiempoActual-_|TiempoAristas],TiempoTotal) :-
    TiempoTotal #= TiempoAnterior + TiempoActual,
    suma_tiempos(TiempoAristas,TiempoAnterior).

ruta_corta([MejorRuta], MejorRuta,ListaTramos,TiempoMínimo) :-
    tiempo_ruta(MejorRuta,ListaTramos,TiempoMínimo).

ruta_corta(Rutas,MejorRuta,ListaTramos,TiempoMínimo):- 
    Rutas = [_,_|_],
    maplist(tiempo_ruta,Rutas,TiempoAristas,TiempoTotal),
    min_list(TiempoTotal,TiempoMínimo),
    nth0(N,TiempoTotal,TiempoMínimo),
    nth0(N,Rutas,MejorRuta),
    nth0(N,TiempoAristas,ListaTramos).

calcula_mejor_ruta(EstaciónOrigen, EstaciónDestino, MejorRuta, Tiempo) :-
    findnsols(2000,R,ruta(EstaciónOrigen,EstaciónDestino,R),Rutas),
    ruta_corta(Rutas,MejorRuta,_,Tiempo).

mejor_ruta(EstaciónOrigen, EstaciónDestino, MejorRuta, Tiempo) :-
    calcula_mejor_ruta(EstaciónOrigen, EstaciónDestino, MejorRuta, Tiempo),
    format('Tiempo = '),
    imprimir_tiempo(Tiempo).

imprimir_tiempo(Tiempo) :-
    Tiempo #> 60,
    Horas #= Tiempo // 60,
    Minutos #> 0,
    Minutos #= Tiempo mod 60,
    format('~w horas y ~w minutos~n', [Horas,Minutos]).

imprimir_tiempo(Tiempo) :-
    Tiempo #< 60,
    format('~w minutos~n', [Tiempo]).

imprimir_tiempo(Tiempo) :-
    Tiempo #> 0,
    Tiempo mod 60 #= 0,
    Horas #= Tiempo // 60,
    format('~w horas ~n', [Horas]).

reporte_tiempo(EstaciónOrigen, EstaciónDestino) :-
    calcula_mejor_ruta(EstaciónOrigen, EstaciónDestino, MejorRuta, Tiempo),

    format('~nTiempo total de viaje: ~w minutos = ',[Tiempo]),
    imprimir_tiempo(Tiempo),

    valor_parámetro(tiempo_inicial,TiempoInicial),
    MejorRuta = [InicioN|_],
    convertir_átomo(InicioN,Inicio),
    format('Inicio: ~w, ~w minutos~n',[Inicio,TiempoInicial]),

    tiempo_ruta(MejorRuta,TiempoTramos,_),
    once(imprimir_tramos(1,MejorRuta,TiempoTramos)),

    valor_parámetro(tiempo_final,TiempoFinal),
    append(_,[FinN],MejorRuta),
    convertir_átomo(FinN,Fin),
    format('Fin: ~w, ~w minutos~n~n',[Fin,TiempoFinal]).

imprimir_tramos(_,[_],_).

imprimir_tramos(Posición,[E1,E2|Ruta],[Tiempo-Transborde|Tramos]) :-
    Ruta \== [],
    (sigue(E2,E1,L1) ; sigue(E1,E2,L1)),
    (sigue(E2,_,L2) ; sigue(_,E2,L2)),
    L1 \== L2, % hay posibilidad de transborde

    convertir_átomo(E1,E1N), convertir_átomo(E2,E2N),

    ((Transborde == no,
    format('~w) ~w a ~w, ~w minutos, sin transborde~n',[Posición,E1N,E2N,Tiempo]));
    
<<<<<<< HEAD
    (Transborde \== no, convertir_átomo(Transborde,TransbordeN),
    format('~w) ~w a ~w, ~w minutos, transborde a ~w~n',[Posición,E1N,E2N,Tiempo,TransbordeN]))),
=======
    (Transborde \== no,
    format('~w) ~w a ~w, ~w minutos, transborde a ~w~n',[Posición,E1N,E2N,Tiempo,Transborde]))),
>>>>>>> a116dedf69af6fbdeb8bf5d9e99a6631e8aa14dd
    
    PosiciónSiguiente #= Posición + 1,
    once(imprimir_tramos(PosiciónSiguiente,[E2|Ruta],Tramos)).

imprimir_tramos(Posición,[E1,E2|Ruta],[Tiempo-_|Tramos]) :-
    convertir_átomo(E1,E1N), convertir_átomo(E2,E2N),
    format('~w) ~w a ~w, ~w minutos~n',[Posición,E1N,E2N,Tiempo]),
    PosiciónSiguiente #= Posición + 1,
    once(imprimir_tramos(PosiciónSiguiente,[E2|Ruta],Tramos)).

convertir_átomo(Átomo,Texto) :-
    atom_chars(Átomo,ListaÁtomo),
    once(agregar_espacios(ListaÁtomo,ÁtomoConEspacio)),
    agregar_mayúsculas(ÁtomoConEspacio,ÁtomoConMayúsculas),
    atom_chars(Texto,ÁtomoConMayúsculas).

agregar_espacios(Átomo,Átomo) :-
    \+ member('_',Átomo).

agregar_espacios(Átomo,ÁtomoConEspacio) :-
    member('_',Átomo),
    nth0(N,Átomo,'_',Resto),
    nth0(N,NuevoÁtomo,' ',Resto),
    agregar_espacios(NuevoÁtomo,ÁtomoConEspacio).

agregar_mayúsculas(Átomo,ÁtomoConMayúsculas) :-
    \+ member(' ',Átomo),
    Átomo = [A|Resto],
    Letras = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],
    member(A,Letras),
    upcase_atom(A,Mayúscula),
    ÁtomoConMayúsculas = [Mayúscula|Resto].

agregar_mayúsculas(Átomo,ÁtomoConMayúsculas) :-
    member(' ',Átomo),
    append(Inicio,[' '|Resto],Átomo),
    agregar_mayúsculas(Inicio,InicioN),
    agregar_mayúsculas(Resto,RestoN),
    append(InicioN,[' '|RestoN],ÁtomoConMayúsculas).

agregar_mayúsculas(Átomo,Átomo) :-
    Átomo = [A|_],
    Letras = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],
    \+ member(A,Letras).
