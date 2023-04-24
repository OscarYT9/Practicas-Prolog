:- op(1060, yfx, <->).  % doble implicación
:- op(1050, yfx, <-).   % implicación hacia la izquierda
:- op(800, yfx, xor).   % disyunción exclusiva
:- op(600, yfx, v).     % disyunción
:- op(400, yfx, &).     % conjunción
:- op(200, fy, ~ ).     % negación
:- op(800, yfx, nand).

% Definición de los átomos
define((F xor G), ~ (F <-> G)).
define((F <-> G), (F -> G) & (F <- G)).
define((F <- G), (G -> F)).
define((F -> G), ~ F v G).
define((F nand G), ~ (F & G)).


%?- unfold( p <-> q & ¬ r, G ).
%G =  (¬p v q& ¬r)&(¬ (q& ¬r)v p).

%?- unfold( (a -> b) xor c, G ).
%G = ¬ ((¬ (¬a v b)v c)&(¬c v (¬a v b))).


% Predicado unfold para transformar fórmulas en su equivalente con los operadores permitidos

unfold(F,G) :- atom(F), G=F. % Si F es un átomo, entonces G será igual a F.

unfold(~F, ~G) :- % Si F es una negación, se aplicará la equivalencia de la negación.
    unfold(F, G), !.

unfold((F & G), (H & J)) :- % Si F y G están unidos por la conjunción, se aplicará la equivalencia de la conjunción.
    unfold(F, H),
    unfold(G, J).

unfold((F v G), (H v J)) :- % Si F y G están unidos por la disyunción, se aplicará la equivalencia de la disyunción.
    unfold(F, H),
    unfold(G, J).

unfold(F, G) :- % Si F no es un átomo ni una fórmula con los operadores permitidos, se utilizará el predicado define(F, NewF) para definir F en términos de los operadores permitidos.
    define(F, NewF),
    unfold(NewF, G), !. % Se llama de manera recursiva al predicado unfold con la nueva fórmula NewF.


%unfold(F v G, H v K) :- unfold(F, H), unfold(G, K).
% ~a v ~ (~b v c).
tab(F, R) :-
    unfold(F, G),
    tab([G], [], R). % tab([~a v ~ (~b v c).], [], R)

tab([F], Ls1, [F|Ls1]) :- %tab([b], [], [b])
    atomic(F).

tab([~F], Ls1, [~F|Ls1]) :- %tab([~c], [b], [~c,b])
    atomic(F).

tab([~(~F)], Ls1, Ls2) :-
    tab([F], Ls1, Ls2). 

tab([F & G], Ls1, Ls3) :-
    tab([F], Ls1, Ls2), %tab([b], [], Ls2) Ls2 =[b]
    tab([G], Ls2, Ls3), %tab([~c], [b], Ls3) Ls3 =[~c, b]
    tab(Ls3). % de ultimo comprueba toda la rama, para ver si hay algún átomo y su negación

tab([~(F & G)], Ls1, Ls2) :-
    tab([~F v ~G], Ls1, Ls2).

tab([F v G], Ls1, Ls2) :-
    (tab([F], Ls1, Ls2) ; tab([G], Ls1, Ls2)). %F = tab([~a], [], Ls2) , tab([~ (~b v c).], [], Ls2)
    
tab([~(F v G)], Ls1, Ls2) :-
    tab([~F & ~G], Ls1, Ls2). %tab([(b & ~c).], [], Ls2)

tab(Ls3) :-
    \+ (select(A, Ls3, Rest), memberchk(~A, Rest)).


% Fs lista de fórmulas que aún tenemos que desdoblar en la tabla semántica, 
% Lits1 conjunto de literales que llevamos calculados en la rama actual hasta el momento, si llegamos al final (no quedan fórmulas por desdoblar) la lista
% Lits2 devuelve la rama que hemos encontrado abierta. Si todas las ramas están cerradas (fórmula inconsistente) tanto tab/3 como, por tanto, tab/2 deberán ser false.

% [1,2,3,4] = [1|[2|[3|[4|[]]]]

tab(((~p) & q) -> (p & (q v (~r))), R).












%unfold(F, G) :- F =.. [Op| [Arg1, Arg2]], unfold(Arg1, G1), unfold(Arg2, G2), G =.. [Op, G1, G2].

%unfold(F, X1 v X2) :- F=.. [Op|[F1|F2]] , unfold(F1,X1), unfold(F2,X2).

% 1+1=.. L.
% L=[+,1,1]

%familia(leonor, felipe, juancarlos)
%F=[familia, leonor, felipe, juancarlos]

%F= a v b, F=.. [0p, F1, F2]