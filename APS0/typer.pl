assoc(X, [(X,V)|_], V).
assoc(X, [_|XS], V) :- assoc(X, XS, V).

type_expr(_,num(X),int) :-
 	integer(X).
 	
type_expr(C,id(X),T) :-
	assoc(X,C,T).

type_expr(_,num(VAR), int).

type_expr(_,id(true), bool).
type_expr(_,id(false), bool).


type_expr(G, if(E1, E2, E3), T):-
    type_expr(G, E1, bool),
    type_expr(G, E2, T),
    type_expr(G, E3, T).

type_expr(G, and(E1, E2), T):-
    type_expr(G, E1, bool),
    type_expr(G, E2, bool).

g0([
    (true, bool),
    (false, bool),
    (not, arrow(bool, bool)),
    (eq, arrow(star(int, int), bool)),
    (lt, arrow(star(int, int), bool)),
    (add, arrow(star(int, int), int)),
    (sub, arrow(star(int, int), int)),
    (mul, arrow(star(int, int), int)),
    (div, arrow(star(int, int), int))
]).

type_prog(G, prog(X), void):-
    type_cmds(G, X, void).

type_cmds(G, cmds(S), void):-
    type_stat(G, S, void).





type_num(G, num(X), int):-
    type_num(G, X, int).