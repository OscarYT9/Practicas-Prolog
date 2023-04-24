% Oscar Vilela Rodriguez (oscar.vilela.rodriguez@udc.es)
% Guillermo García Engelmo (g.garcia2@udc.es)

% Operadores lógicos en prolog
:- op(1060, yfx, <->).  % doble implicación
:- op(1050, yfx, <-).   % implicación hacia la izquierda
:- op(800, yfx, xor).   % disyunción exclusiva
:- op(600, yfx, v).     % disyunción
:- op(400, yfx, &).     % conjunción
:- op(200, fy, ~ ).     % negación



% Apartado 1: operadores derivados (predicado unfold)
define((F xor G), ~ (F <-> G)).
define((F <-> G), (F -> G) & (F <- G)).
define((F <- G), (G -> F)).
define((F -> G), ~ F v G).


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


% Apartado 2: Tablas Semánticas


tab(F, R) :-
    unfold(F, G),   
    tab([G], [], R).    

tab([F], Ls1, [F|Ls1]) :-  % Si la fórmula F es atómica, entonces se agrega a la lista Ls1 que se devuelve como tercer argumento
    atomic(F).


tab([~F], Ls1, [~F|Ls1]) :- % Si F es una negación de una fórmula, se agrega a Ls1 como tercera entrada
    atomic(F).


tab([~(~F)], Ls1, Ls2) :-  % Si F es la doble negación de una fórmula, se llama al predicado 'tab/3' con F como entrada y Ls2 como salida
    tab([F], Ls1, Ls2).


tab([F & G], Ls1, Ls3) :-    % Si F y G son conjunciones de dos fórmulas, se llama a 'tab/3' con F y G como entrada, y se agrega el resultado a Ls3
     tab([F], Ls1, Ls2),     % Llama a 'tab/3' con F como entrada y Ls2 como salida
     tab([G], Ls2, Ls3),     % Llama a 'tab/3' con G como entrada y Ls3 como salida
     tab(Ls3).               % Llama a 'tab/1' con Ls3 como entrada


tab([~(F & G)], Ls1, Ls2) :- % Si F y G son conjunciones y se niegan, se llama al predicado 'tab/3' con las fórmulas ~F y ~G unidas por una disyunción como entrada, y se devuelve Ls2
    tab([~F v ~G], Ls1, Ls2).



tab([F v G], Ls1, Ls2) :-   % Si F y G son disyunciones, llama a 'tab/3' con F y G como entrada, y se agrega el resultado a Ls2
    (tab([F], Ls1, Ls2) ; tab([G], Ls1, Ls2)). % Llama a 'tab/3' con F o G como entrada y Ls2 como salida, dependiendo del resultado de la disyunción lógica


tab([~(F v G)], Ls1, Ls2) :- % Si F y G son disyunciones y se niegan, llama a 'tab/3' con ~F y ~G unidos por una conjunción como entrada y se devuelve Ls2
    tab([~F & ~G], Ls1, Ls2).


tab(Ls3) :-  % Si Ls3 es una lista de fórmulas, verifica si contiene una fórmula y su negación en Ls3
    \+ (select(A, Ls3, Rest), memberchk(~A, Rest)). % Si no hay tal par, devuelve verdadero; de lo contrario, devuelve falso