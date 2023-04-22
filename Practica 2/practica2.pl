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
    unfold(NewF, G), !. %vuelve a llamar al método unfold cada vez hasta que F es un atom, averiguando en cada vez un valor para F, reduciendo la formula hasta  que solo queden ~, & , v


%unfold(F v G, H v K) :- unfold(F, H), unfold(G, K).

tab(F, R) :- 
    unfold(F, G), 
    tab([G], [], R).

tab([F], _, [F]) :- atomic(F).
    
tab([~F], _, [~F]) :- atomic(F).

tab([], _, []).

tab([~(~F)], Ls1, Ls2) :- 
    tab([F], Ls1, Ls2).

tab([F & G], Ls1, Ls2) :- 
    tab([F], Ls1, Ls3),
    tab([G], Ls1, Ls4),
    append(Ls3, Ls4, Ls2).

tab([~(F & G)], Ls1, Ls2) :- 
    tab([~F v ~G], Ls1, Ls2).

tab([F v G], Ls1, Ls2) :- 
    tab([F], Ls1, Ls3),
    tab([G], Ls1, Ls4),
    append(Ls3, Ls4, Ls2).

tab([~(F v G)], Ls1, Ls2) :- 
    tab([~F], Ls1, Ls3),
    tab([~G], Ls1, Ls4),
    append(Ls3, Ls4, Ls2).




% [1,2,3,4] = [1|[2|[3|[4|[]]]]














%unfold(F, G) :- F =.. [Op| [Arg1, Arg2]], unfold(Arg1, G1), unfold(Arg2, G2), G =.. [Op, G1, G2].

%unfold(F, X1 v X2) :- F=.. [Op|[F1|F2]] , unfold(F1,X1), unfold(F2,X2).

% 1+1=.. L.
% L=[+,1,1]

%familia(leonor, felipe, juancarlos)
%F=[familia, leonor, felipe, juancarlos]

%F= a v b, F=.. [0p, F1, F2]