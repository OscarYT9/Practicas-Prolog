:- op(1060, yfx, <->).  % doble implicación
:- op(1050, yfx, <-).   % implicación hacia la izquierda
:- op(800, yfx, xor).   % disyunción exclusiva
:- op(600, yfx, v).     % disyunción
:- op(400, yfx, &).     % conjunción
:- op(200, fy, ~ ).     % negación


% Definición de los átomos
define((F xor G), ~ (F <-> G)).
define((F <-> G), (F -> G) & (F <- G)).
define((F <- G), (G -> F)).
define((F -> G), ~ F v G).



%?- unfold( p <-> q & ¬ r, G ).
%G =  (¬p v q& ¬r)&(¬ (q& ¬r)v p).

%?- unfold( (a -> b) xor c, G ).
%G = ¬ ((¬ (¬a v b)v c)&(¬c v (¬a v b))).


% Predicado unfold para transformar fórmulas en su equivalente con los operadores permitidos
unfold(F,G) :- atom(F), G=F.

unfold(~F, ~G) :-
    unfold(F, G), !.

unfold((F & G), (H & J)) :-
    unfold(F, H),
    unfold(G, J).

unfold((F v G), (H v J)) :-
    unfold(F, H),
    unfold(G, J).

unfold(F, G) :-
    define(F, NewF),
    unfold(NewF, G), !. % Con el ! cuando acaba de hacer el unfold(NewF, G) que es recursivo por lo que itera varias veces, simplemente para sin devolver false, ya que cuando vuelve arriba y comprueba las funciones ninguna da True por lo que devuelve False %vuelve a llamar al método unfold cada vez hasta que F es un atom, averiguando en cada vez un valor para F, reduciendo la formula hasta  que solo queden ~, & , v


%unfold(F v G, H v K) :- unfold(F, H), unfold(G, K).

tab(F, R) :- 
    unfold(F, G), 
    tab([G], [], R).

tab([F], _, [F]) :- atomic(F).

tab([~F], _, [~F]) :- atomic(F).

tab([], _, _) :- !, fail.

tab([~(~F)], Ls1, Ls2) :- 
    tab([F], Ls1, Ls2).

tab([F & G|Fs], Ls1, Ls2) :- 
    tab([F|Fs], Ls1, Ls3),
    tab([G|Fs], Ls1, Ls4),
    append(Ls3, Ls4, Ls2).

tab([~(F & G)|Fs], Ls1, Ls2) :- 
    tab([~F v ~G|Fs], Ls1, Ls2).

tab([F v G|Fs], Ls1, Ls2) :- 
    (   tab([F|Fs], Ls1, Ls2)
    ;   tab([G|Fs], Ls1, Ls2)
    ).

tab([~(F v G)|Fs], Ls1, Ls2) :- 
    tab([~F & ~G|Fs], Ls1, Ls2).





% Fs lista de fórmulas que aún tenemos que desdoblar en la tabla semántica, 
% Lits1 conjunto de literales que llevamos calculados en la rama actual hasta el momento, si llegamos al final (no quedan fórmulas por desdoblar) la lista
% Lits2 devuelve la rama que hemos encontrado abierta. Si todas las ramas están cerradas (fórmula inconsistente) tanto tab/3 como, por tanto, tab/2 deberán ser false.

% [1,2,3,4] = [1|[2|[3|[4|[]]]]














%unfold(F, G) :- F =.. [Op| [Arg1, Arg2]], unfold(Arg1, G1), unfold(Arg2, G2), G =.. [Op, G1, G2].

%unfold(F, X1 v X2) :- F=.. [Op|[F1|F2]] , unfold(F1,X1), unfold(F2,X2).

% 1+1=.. L.
% L=[+,1,1]

%familia(leonor, felipe, juancarlos)
%F=[familia, leonor, felipe, juancarlos]

%F= a v b, F=.. [0p, F1, F2]