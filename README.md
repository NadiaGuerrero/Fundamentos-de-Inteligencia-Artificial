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
     - [History of AI](https://www.coe.int/en/web/artificial-intelligence/history-of-ai#)
    - [Gardner's Theory of Multiple Intelligences](https://www.verywellmind.com/gardners-theory-of-multiple-intelligences-2795161)
    - [Frames of Mind - Howard Gardner](https://www.dropbox.com/s/g5g07g5cufijrqm/Frames%20Of%20Mind_%20The%20Theory%20Of%20Multiple%20Intelligences%20%281993%29.pdf?dl=0)
    - [Intelligence Reframed - Howard Gardner](https://www.dropbox.com/s/ps12gvfanswsshx/Intelligence%20Reframed_%20Multiple%20Intelligences%20for%20the%2021st%20Century%20%282000%29.pdf?dl=0)
    
2. Inteligencia, lenguaje y símbolos 
    - [The Role of Language in Intelligence](https://ase.tufts.edu/cogstud/dennett/papers/rolelang.htm)
    - [The Physical Symbol System Hypothesis](http://biometaheuristics.org/moodle/pluginfile.php/53/course/section/83/02-Physical%20Symbol%20System%20Hypothesis.pdf)
    
3. Pattern matching  y  Unificación 
    - [Unification](https://en.wikipedia.org/wiki/Unification_(computer_science))
    - [Unification Theory](http://biometaheuristics.org/moodle/pluginfile.php/53/course/section/83/%281999%29%20Unification%20Theory.pdf)
    
### Programación Prolog

4. Fundamentos de Prolog 
    - [Facts, Rules & Queries](http://www.let.rug.nl/bos/lpn//lpnpage.php?pagetype=html&pageid=lpn-htmlch1)
    - [Prolog: An Introduction](https://www.geeksforgeeks.org/prolog-an-introduction/)
    - [Getting started quickly](https://www.swi-prolog.org/pldoc/man?section=quickstart)
    
5. Edición de la Base de Conocimiento 
    - [Tutorials Point Prolog Relations](https://www.tutorialspoint.com/prolog/prolog_relations.htm)
    - [Jon Pearce Prolog Knowledge Base](http://www.cs.sjsu.edu/faculty/pearce/modules/lectures/prolog/kbase.htm)
    
6. Razonamiento declarativo y procesamiento de listas 
    - [Tutorials Point Lists](https://www.tutorialspoint.com/prolog/prolog_lists.htm)
    - [Lists in Prolog](https://www.geeksforgeeks.org/lists-in-prolog/)
    - [Chapter 4: Lists](https://www.let.rug.nl/bos/lpn//lpnpage.php?pagetype=html&pageid=lpn-htmlch4)
    - Logic Programming with Prolog - [Chapter 9: List processing](http://biometaheuristics.org/moodle/pluginfile.php/53/course/section/86/LPwP%20%20Chap%209-List%20processing.pdf)
    
7. Recolección de soluciones 
    - [11.2: Collecting Solutions](https://www.let.rug.nl/bos/lpn//lpnpage.php?pagetype=html&pageid=lpn-htmlse49)
    - [Finding all Solutions](https://semanticweb.cs.vu.nl/verrijktkoninkrijk/swish/pldoc/man?section=allsolutions)
    
8. Razonamiento recursivo 
    - [Introduction to Prolog Recursion](https://www.doc.gold.ac.uk/~mas02gw/prolog_tutorial/prologpages/recursion.html)
    - [Fun Facts about Prolog](https://www.metalevel.at/prolog/fun)
    
9. Figuras del Poker (Tarea #2) 
    - [UNICODE Characters Table](https://www.rapidtables.com/code/text/unicode-characters.html)
    - [Unicode Library](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/utf8proc.html%27))
    
10. Control de backtracking 
    - [Prolog examples of Cut](https://www.tutorialspoint.com/prolog/prolog_examples_of_cuts.htm)
    - [Prolog Cut and Negation](https://en.wikibooks.org/wiki/Prolog/Cuts_and_Negation)
    - [The Prolog Dictionary Cut, !](http://www.cse.unsw.edu.au/~billw/dictionaries/prolog/index.html)
    
11. Control de backtrackig 2 (Patrones de uso) 
    - [Cut with Failure](https://www.javatpoint.com/prolog-cut-with-failure)
    - [The use of Cut](https://www-users.cse.umn.edu/~gini/prolog/cut.html)
    
12. El depurador de SWI-Prolog 
    - [Debugging Prolog Programs](https://www.metalevel.at/prolog/debugging)
    - [Debugging and Testing Prolog](https://medium.com/@karandavuluri/debugging-and-testing-prolog-9d269c7296d1)
    
13. Estructuras, ciclos y diferencia de listas 
    - [Difference Lists](https://sodocumentation.net/prolog/topic/9414/difference-lists)
    - [Difference Lists Explored](https://pbrown.me/blog/difference-lists-explored/)
    
### Paradigma de Programación Lógica con Restricciones

14. Programación Lógica con Restricciones 
    - [Library(clpfd)](https://www.swi-prolog.org/pldoc/man?section=clpfd-intro)
    - [Prolog Integer Arithmetic](https://www.metalevel.at/prolog/clpz)
    
15. Aritmética con restricciones 
    - [Constraint Logic
Programming	](https://en.wikibooks.org/wiki/Prolog/Constraint_Logic_Programming#:~:text=We%27ve%20seen%20that%20in,rather%20than%20a%20specific%20value.)
    
16. Razonamiento por unificación  
    - [Zebra Puzzles](https://www.brainzilla.com/logic/zebra/)
    - [Using Prolog to solve logic puzzles](http://bennycheung.github.io/using-prolog-to-solve-logic-puzzles)
    
17. Problemas lógicos  
    - [What is the name of this book? - Raymond Smullyan](http://biometaheuristics.org/moodle/pluginfile.php/53/course/section/97/What%20Is%20the%20Name%20of%20This%20Book%20%282011%29.pdf)
    - [Knights and Knaves](https://www.popularmechanics.com/science/math/a14382121/riddle-of-the-week-43-knights-and-knaves-part-1/)
    - [All Riddles](https://www.popularmechanics.com/riddles-logic-puzzles/)
    - [Puzzles in Prolog](https://ypologist.com/mmalita17/HOMEPAGE/logic/index.html)
    
18. Razonamiento con grafos  
    - [Graph Theory Basics](https://www.geeksforgeeks.org/mathematics-graph-theory-basics-set-1/)
    - [Prolog and Graphs](https://rlgomes.github.io/work/prolog/2012/05/22/19.00-prolog-and-graphs.html)
    - [Prolog Site](https://sites.google.com/site/prologsite/prolog-problems/6)
    
19. EXAMEN PARCIAL
      
### Búsqueda heurística

20. Modelo de razonamiento 
    - [Heuristic](https://en.wikipedia.org/wiki/Heuristic_(computer_science))
    - [States and Seaching](https://artint.info/html/ArtInt_46.html)
    - [Search Algorithms in AI](https://www.geeksforgeeks.org/search-algorithms-in-ai/)
      
21. Búsqueda ordenada 
    - [DFS and BFS](https://www.tutorialspoint.com/difference-between-bfs-and-dfs)
    - [BFS and DFS](https://www.geeksforgeeks.org/difference-between-bfs-and-dfs/)
      
22. Búsqueda ordenada 2 
    - [States and Searching](https://artint.info/html/ArtInt_46.html)
    - [Graph Traversal for Problem Solving](https://swish.swi-prolog.org/p/Graphs1.swinb)
      
23. Búsqueda informada 
    - [BestFS in AI](https://www.mygreatlearning.com/blog/best-first-search-bfs/)
    - [Best First Search](https://www.geeksforgeeks.org/best-first-search-informed-search/)
      
24. Solución de laberintos

### Búsqueda adversarial

25. Búsqueda adversarial 
    - [Mini Max Search](https://en.wikibooks.org/wiki/Artificial_Intelligence/Search/Adversarial_search/Minimax_Search)
    - [Mini Max Algorithm](https://www.professional-ai.com/minimax-algorithm.html)
    - [Mini Max Algorithm in AI](https://tutorialforbeginner.com/mini-max-algorithm-in-ai)
    - [Understaning the Mini Max Algorithm](https://www.neverstopbuilding.com/blog/minimax)
      
26. Podas alpha-beta 
    - [Alpha-Beta Prunning](https://www.mygreatlearning.com/blog/alpha-beta-pruning-in-ai/)
    - [Alpha-Beta](https://www.chessprogramming.org/Alpha-Beta)
      
27. Estrategia de juego 
    - [Tic Tac Toe Variants](https://en.wikipedia.org/wiki/Tic-tac-toe_variants)
    - [Agente jugador Gato 4x4](https://emu.edu/gaming-hub/tic-tac-toe)
      
28. Heurísticas auxiliares 
    - [Killer move heuristic](https://rustic-chess.org/search/ordering/killers.html)
    - [History heuristic](https://www.chessprogramming.org/History_Heuristic)
    - [Null move prunning](https://www.chessprogramming.org/Null_Move_Pruning)
    - [Killer heuristic](https://www.chessprogramming.org/Killer_Heuristic)
    - [Move ordering](https://www.chessprogramming.org/Move_Ordering)
    - [NegaMax](https://en.wikipedia.org/wiki/Negamax)
      
29. PROYECTO FINAL
      
### Planificación

30. Planificación estilo STRIPS 
    - [Getting started with  PDDL](https://fareskalaboud.github.io/LearnPDDL/)
    - [Yarox:
PDDL Examples](https://github.com/yarox/pddl-examples)
    - [Automated Planning in a Nutshell](http://pddl4j.imag.fr/automated_planning_in_a_nutshell.html)
    - [Domain-independent planner (PDDL2.2)](https://lpg.unibs.it/lpg/)
    - [ICAPS 2023](https://www.icaps-conference.org/conference/icaps-2023/)
    - [International Planning Competition 2023](https://ipc2023.github.io/)
      
31. Detalles de un planificador 
    - [Sussman Anomaly](https://en.wikipedia.org/wiki/Sussman_anomaly)
    - [Partial Order Planning](https://en.wikipedia.org/wiki/Partial-order_planning)
    - [AI Planning](https://researcher.watson.ibm.com/researcher/view_group.php?id=8432)
    - [AAAI Tutorial on AI Planning (Youtube)](https://www.youtube.com/watch?v=q-mShBwHkc4)
