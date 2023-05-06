:- op(1060, yfx, <->).   % doble implicación
:- op(1050, yfx, <-).    % implicación hacia la izquierda
:- op(600, yfx, v).      % disyunción
:- op(400, yfx, &).      % conjunción
:- op(200, fy, ~ ).      % negación
:- op(200, fx, forall).  % universal
:- op(200, fx, exists).  % existencial
:- op(300, xfy, ::).     % separador para cuantificadores


%?- subs(x/f(3), g(x), G). %f(3) no es atomico, g(x) no es atomico
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
subs(X/T, X, T):- !.                      %  x/f(3), x, G = f(3).
subs(X/T, H, H):- atom(H), !.             %  x/f(3), y, G = y.
subs(X/T, exists X::F, exists X::F):- !.  % casos para cuantificadores
subs(X/T, forall X::F, forall X::F):- !.

% Casos recursivos
subs(X/T, F, G) :-                   
    F =.. [Op|Args],
    write('Args: '), write(Args), nl,
    maplist(subs(X/T), Args, NewArgs),          % Sustituir cada X por T en cada elemento de Arg:   [forall y  ,   (p(x,y)->exists x:: ~q(y,x))] , Arg =[y] NewArgs: [y] (Si no existe la x devuevle el atomo correspondiente)
    write('NewArgs: '), write(NewArgs), nl,     % Sustituir cada X por T en cada elemento de Arg:   [[p(x,y)   ,   exists x:: ~q(y,x)]] , NewArgs: [x,y] (Si no existe la x devuevle el atomo correspondiente)
    G =.. [Op|NewArgs].


% L =[subs, a/f(3), a+4], X=..L.
% L =[subs, a/f(3), a+4]
% X = subs(a/f(3), a+4).

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
