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

goal(n).

depth_first_iterative_deepening(Node, Solution):-
	path(Node, GoalNode,Solution),
	goal(GoalNode).

path(Node, Node, [Node]).

path(FirstNode, LastNode, [LastNode|Path]) :- 
    path(FirstNode, OneButLast, Path),
    s(OneButLast, LastNode),
    not(member(LastNode, Path)).
