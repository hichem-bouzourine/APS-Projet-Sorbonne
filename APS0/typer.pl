/* quelques morceaux de code inspiré de (github.com/AdelTechCrafter/APS) --- /*! pas de copie coller car ils ont travaillé différemment */ */
chercher(X, [(X,V)|_], V).
chercher(X, [_|XS], V) :- chercher(X, XS, V).

/* ChatGPT nous a aidé à faire juste (get_type/recupererTypeArgs/check_args) -- le reste, nous l'avons fait */
get_type([],[]).
get_type([A|ARGS],[T|TYPES]) :-
	type_expr([],A,T),
	get_type(ARGS,TYPES).

recupererTypeArgs([],[]).
recupererTypeArgs([(_,T)|ARGS],[T|RES]) :-
	recupererTypeArgs(ARGS,RES).
		
check_args(_,[],[]).
check_args(G,[ARG|ARGS],[ARGTYPE|ARGSTYPE]) :-
	type_expr(G,ARG,ARGTYPE),
	check_args(G,ARGS,ARGSTYPE).


/* Le contexte G0 initial */
g0([
    (true, bool),
    (false, bool),
    (not, funcType([bool], bool)),
    (eq, funcType([int, int], bool)),
    (lt, funcType([int, int], bool)),
    (add, funcType([int, int], int)),
    (sub, funcType([int, int], int)),
    (mul, funcType([int, int], int)),
    (div, funcType([int, int], int))
]).

/* id */
type_expr(G,id(X),T):-
    chercher(X,G,T).
/* Num */
type_expr(_,num(N),int) :-
 	integer(N).

type_expr(_,id(true), bool).
type_expr(_,id(false), bool).

/* If  */
type_expr(G, if(E1, E2, E3), T):-
    type_expr(G, E1, bool),
    type_expr(G, E2, T),
    type_expr(G, E3, T).

/* And */
type_expr(G, and(E1, E2), bool):-
    type_expr(G, E1, bool),
    type_expr(G, E2, bool).

/* Or  */
type_expr(G, or(E1, E2), bool):-
    type_expr(G, E1, bool),
    type_expr(G, E2, bool).

/* Application */
type_expr(G,app(E1,ARGS), T):- 
    type_expr(G,E1, funcType(ARGSTYPE, T)),
    check_args(G,ARGS,ARGSTYPE).

/* Abs (aka lambda) */
type_expr(G,lambda(ARGS,E),T):- 
    append(ARGS,G,GI), 
    chercher(GI,E,T).

/* Stat - "ECHO" */
type_stat(G,echo(E),void) :-
	type_expr(G,E,int).	

/* Def Const */
type_def(G,const(X,T,E),[(X,T)|G]):-
	type_expr(G,E,T).	

/* Def Fun */

/* La règle `type_def` définit le type d'une fonction dans un environnement de types G. */
/* Elle fusionne les arguments avec G, détermine le type de l'expression de la fonction, */
/* extrait les types des arguments, puis met à jour l'environnement avec le nom de la fonction et son type.*/
type_def(G,fun(FUN,T,ARGS,E),GI):-
	append(ARGS,G,G2), 
	type_expr(G2,E,T),
	recupererTypeArgs(ARGS,RES),
	GI=[(FUN,funcType(RES,T))|G]. 

/* Def Fun Rec */

/* La règle `type_def` définit le type d'une fonction récursive dans un environnement de types G. */
/* Elle extrait d'abord les types des arguments, puis fusionne les arguments avec G. */
/* Ensuite, elle met à jour l'environnement temporaire avec le type de la fonction, */
/* analyse le type du corps de la fonction, */
/* et enfin met à jour l'environnement final avec le nom de la fonction et son type.*/
type_def(G,funRec(FUN,ARGS,T,E),GI):-
	recupererTypeArgs(ARGS,RES),
	append(ARGS,G,G2), 
    G3 = [(ID,funcType(RES,T))|G2],
	type_expr(G3,E,T),
	GI=[(FUN,funcType(RES,T))|G]. 

/* CMDS - Defs */
type_cmds(G,[cmds(X)|Y],void) :-
	type_def(G,X,G2),
	type_cmds(G2,Y,void).

/* CMDS - End  */
type_cmds(_,[],void).
type_cmds(G,[X|Y],void) :-
	type_stat(G,X,void),
	type_cmds(G,Y,void).

/* Prog */
type_prog(G, prog(X), void):-
    type_cmds(G, X, void).
