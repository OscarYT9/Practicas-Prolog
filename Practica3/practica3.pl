% Oscar Vilela Rodriguez (oscar.vilela.rodriguez@udc.es)
% Guillermo García Engelmo (g.garcia2@udc.es)

% Operadores lógicos en prolog
:- op(1060, yfx, <->).   % doble implicación
:- op(1050, yfx, <-).    % implicación hacia la izquierda
:- op(600, yfx, v).      % disyunción
:- op(400, yfx, &).      % conjunción
:- op(200, fy, ~ ).      % negación
:- op(200, fx, forall).  % universal
:- op(200, fx, exists).  % existencial
:- op(300, xfy, ::).     % separador para cuantificadores

%_______________________________________________________________________________________________________________________________________________________________________________
% Apartado 1. Predicado subs
% El predicado a implementar se denominará subs(X/T,F,G) donde X es la variable a reemplazar, T es el término por el que cambiaremos las apariciones libres de
% X, F es la expresión original sobre la que queremos realizar el reemplazo y, por último, G es el resultado del reemplazo.
% El predicado subs(X/T,F,G) deberá trabajar del mismo modo tanto si F es una fórmula como si es un término.

% Algunos ejemplos de ejecución serían:
% ?- subs(x/f(3), g(x), G).                                             G = g(f(3)).
% ?- subs( x/f(4), (forall x:: p(x,y) & p(x)), G).                      G = forall x::p(x, y)&p(f(4)).
% ?- subs(x/3, forall y:: ( p(x,y) -> exists x:: ~ q(y,x) ) , G).       G = forall y::(p(3, y)->exists x:: ~q(y, x)).

% Casos base

% Esta regla maneja el caso donde X y T son idénticos, por lo que no hay nada que sustituir.
% Por ejemplo, si X = x, T = f(3) y X = F,  entonces G = f(3).
subs(X/T, X, T):- !.

% Esta regla maneja el caso donde H es un átomo y no necesita ser sustituido.
% Por ejemplo, si X = x/f(3) y H = y, entonces G = y.
subs(X/T, H, H):- atom(H), !.

% casos para cuantificadores
% Estas dos reglas manejan los casos donde hay cuantificadores existenciales o universales en F.
% En ambos casos, la sustitución no afecta al cuantificador, por lo que G es idéntico a F.
subs(X/T, exists X::F, exists X::F):- !.
subs(X/T, forall X::F, forall X::F):- !.

% Casos recursivos

% Esta regla maneja los casos más complejos donde F es una función o predicado.
subs(X/T, F, G) :-                   
    F =.. [Op|Args],                              % Descomponer F en una lista de argumentos: Op es el nombre de la función/predicado, y Args es una lista de argumentos.
    % write('Args: '), write(Args), nl,
    maplist(subs(X/T), Args, NewArgs),            % Para cada argumento en Args, aplica la regla de sustitución subs(X/T) y almacena los nuevos argumentos en una nueva lista NewArgs.
    % write('NewArgs: '), write(NewArgs), nl,
    G =.. [Op|NewArgs].                           % Construye el nuevo término G utilizando el operador Op y la lista de nuevos argumentos NewArgs.

%_______________________________________________________________________________________________________________________________________________________________________________
% Apartado 2. Predicado subs_list
% Predicado que aplica una lista de sustituciones por orden de izquierda a derecha a una fórmula lógica F, devolviendo el resultado en G.

% Rs es la lista de sustituciones, donde cada elemento tiene la forma X/T, es decir, se sustituirá X por T.
% F es la fórmula lógica que se desea transformar.
% G es la fórmula lógica resultante de aplicar todas las sustituciones de Rs en F por orden de izquierda a derecha.

% Caso base: cuando no hay más sustituciones por aplicar, la fórmula resultante es la misma que la original.
subs_list([], F, F).

% Caso recursivo: se aplica la sustitución X/T a la fórmula F, obteniendo H, y se continúa aplicando las restantes sustituciones de la lista Rs a H.
subs_list([X/T|Rs], F, G) :-
    subs(X/T, F, H),
    subs_list(Rs, H, G).



%Otra implementación aparte del subs:
% Caso base
%subs(X/T, X, T) :- !.                  % z/f(3), y, G
%subs(X/T, H, H) :- atom(H), !.         % x/f(3), y, G
%subs(X/T, exists X::F, exists X::F) :- !. % casos para cuantificadores
%subs(X/T, forall X::F, forall X::F) :- !.

% Caso recursivo
%subs(X/T, F, G) :-
    %F =.. [Op|Args],
    %subs_args(X/T, Args, NewArgs),
    %G =.. [Op|NewArgs].

% subs_args(+X/T, +Args, -NewArgs)
%subs_args(_, [], []) :- !.
%subs_args(X/T, [Arg|Args], [NewArg|NewArgs]) :-
    %subs(X/T, Arg, NewArg),
    %subs_args(X/T, Args, NewArgs).