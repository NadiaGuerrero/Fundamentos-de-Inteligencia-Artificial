# Fundamentos-de-Inteligencia-Artificial

## Tareas
- [Tarea #1](Tareas/familia.prolog) Árbol genealógico

  Representar el árbol genealógico que se encuentra en este [sitio web](https://simpsons.fandom.com/wiki/Simpson_family?file=Simpsons_possible_family_tree.jpg) utilizando prolog.
  
- [Tarea #2](Tareas/poker.prolog) Modelado de figuras de póker

  Construir un programa en prolog que reparta cartas a 4 jugadores de un mazo de 108 cartas, identifique la mejor figura en cada mano y los ordene descendentemente, reportando el resultado en la consola.
  
- Tarea #3 Acertijos tipo cebra
  
  Resolver versiones recortadas del problema de la cebra utilizando el razonamiento por unificación.
  - [Versión 1](Tareas/cebraV1.prolog): 3 casas con 2 atributos y 3 pistas.
  - [Versión 2](Tareas/cebraV2.prolog): 3 casas con 4 atributos y 6 pistas.
  - [Versión 3](Tareas/cebraV3.prolog): 4 casas con 4 atributos y 9 pistas.
 
- EXAMEN PARCIAL Red Metro de la CdMx

  Elaborar un programa para  asistir a un viajero en el la red del Sistema Colectivo Metro de la CdMx.
  El programa debe resolver 3 predicados fundamentales:  mejor_ruta/4, reporte_tiempo/2  y  reporte_simplificado/2  tal como lo indican las láminas de la presentación #19.
  
- Tarea #5 Misioneros y caníbales

  Resolver el problema de los misioneros y caníbales según se describe en la presentación #22  a  partir de la lámina #28, obteniendo cuando menos una solución por cada uno de los métodos de búsqueda  DFS, BFS  e  IDS.
  Se DEBE usar la representación y los predicados que descritos en clase.
  
- Tarea #6 Solución de laberinto

  Programar predicados para solucionar el laberinto que se indica en la presentación #24  y que está  codificado  en  el  archivo:
  laberinto-01.prolog
En  este  archivo  puede  consultar  el  sistema completo  de  coordenadas  renglón, columna  para  el  laberinto  a  resolver.
Solucionar buscando por los métodos  UniformCost, Greedy  y  A*...
Añadir también el predicado  _intercambia_inicio_meta_
  
- Tarea #7 Gato 4x4

  Construir un agente Prolog para jugar Gato 4x4 contra un oponente humano según se explica en las láminas #31 a #40  de  la  presentación #27 (Estrategia de juego).
  Incluir el predicado inicial _inicia_juego(<horizonte>)_ para establecer el horizonte de profundización.
  
- PROYECTO FINAL

  Programar un agente jugador completo (con por lo menos una heurística auxiliar) para alguno de los tres juegos descritos en la presentación #29 Proyecto final.

## Clases

### Conceptos iniciales

1. Presentación inicial
2. Inteligencia, lenguaje y símbolos
3. Pattern matching  y  Unificación

### Programación Prolog

4. Fundamentos de Prolog
5. Edición de la Base de Conocimiento
6. Razonamiento declarativo y procesamiento de listas
7. Recolección de soluciones
8. Razonamiento recursivo
9. Figuras del Poker (Tarea #2)
10. Control de backtracking
11. Control de backtrackig 2 (Patrones de uso)
12. El depurador de SWI-Prolog
13. Estructuras, ciclos y diferencia de listas

### Paradigma de Programación Lógica con Restricciones

14. Programación Lógica con Restricciones
15. Aritmética con restricciones
16. Razonamiento por unificación
17. Problemas lógicos
18. Razonamiento con grafos
19. EXAMEN PARCIAL

### Búsqueda heurística

20. Modelo de razonamiento
21. Búsqueda ordenada
22. Búsqueda ordenada 2
23. Búsqueda informada
24. Solución de laberintos

### Búsqueda adversarial

25. Búsqueda adversarial
26. Podas alpha-beta
27. Estrategia de juego
28. Heurísticas auxiliares
29. PROYECTO FINAL

### Planificación

30. Planificación estilo STRIPS
31. Destalles de un planificador
