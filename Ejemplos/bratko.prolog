/* 
cd("D:/ESCOM/IA/Fundamentos-de-Inteligencia-Artificial/Ejemplos").
[bratko].
leash(-all),leash(+exit),leash(+fail),visible(+all).
depth_first_iterative_deepening(a, Solution).
 */

s(a, b).
s(a, c).
s(b, d).
s(b, e).
s(d, h).
s(d, i).
s(e, j).
s(e, k).
s(c, f).
s(c, g).
s(f, l).
s(f, m).
s(g, n).
s(g, o).

/* s(1, n).
s(c, n).
s(b, c).
s(h, n).
s(a, n).
s(1, 2).
s(2, 3).
s(3, a). */

goal(n).

depth_first_iterative_deepening(Node, Solution):-
	path(Node, GoalNode, Solution),
	goal(GoalNode).

/* path(Node, Node, [Node]). */

/* path(FirstNode, LastNode, [LastNode|Path]) :- 
    path(FirstNode, OneButLast, Path),
    s(OneButLast, LastNode),
    not(member(LastNode, Path)). */

/* path(FirstNode, LastNode, [FirstNode|Path]) :- 
    path(SecondNode, LastNode, Path),
    s(FirstNode, SecondNode),
    not(member(FirstNode, Path)). */

/* path(Node, Node, [Node]) :- goal(Node).

path(FirstNode, LastNode, [FirstNode|Path]) :- 
    s(FirstNode, SecondNode),    
    path(SecondNode, LastNode, Path),
    not(member(FirstNode, Path)). */

path(Node, Node, [Node]).

path(FirstNode, LastNode, [FirstNode|Path]) :- 
    s(FirstNode,N),
    path(SecondNode, LastNode, Path),
    N = SecondNode,
    %s(FirstNode, SecondNode),
    not(member(FirstNode, Path)).


