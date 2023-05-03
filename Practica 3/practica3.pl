:- op(1060, yfx, <->).   % doble implicación
:- op(1050, yfx, <-).    % implicación hacia la izquierda
:- op(600, yfx, v).      % disyunción
:- op(400, yfx, &).      % conjunción
:- op(200, fy, ~ ).      % negación
:- op(200, fx, forall).  % universal
:- op(200, fx, exists).  % existencial
:- op(300, xfy, ::).     % separador para cuantificadores


%?- subs(x/f(3), g(x), G). %f(3) no es atomico, g(x) no es atomico

%Casos base
subs(X/T, X, T):- !.                      %  z/f(3), y, G
subs(X/T, H, H):- atom(H), !.             %  x/f(3), y, G
subs(X/T, exists X::F, exists X::F):- !.  % casos para cuantificadores
subs(X/T, forall X::F, forall X::F):- !.

% Casos recursivos
subs(X/T, F, G) :-                   
    F =.. [Op|Args],
    substitute(subs(X/T), Args, NewArgs), %Aplica el subs a cada elemento de la lista
    G =.. [Op|NewArgs].


% L =[subs, a/f(3), a+4], X=..L.
% L =[subs, a/f(3), a+4]
% X = subs(a/f(3), a+4).

%Otra implementación:

% Caso base
subs(X/T, X, T):- !.                   % z/f(3), y, G
subs(X/T, H, H):- atom(H), !.          % x/f(3), y, G
subs(X/T, exists X::F, exists X::F):- !. % casos para cuantificadores
subs(X/T, forall X::F, forall X::F):- !.

% Caso recursivo
subs(X/T, F, G) :-                   
    F =.. [Op|Args],
    subs_list(X/T, Args, NewArgs),      % Aplica subs a la lista de argumentos
    G =.. [Op|NewArgs].

% subs_list(+X/T, +Args, -NewArgs)
% Aplica subs a cada elemento de Args y construye una nueva lista con los resultados
subs_list(_, [], []).
subs_list(X/T, [A|As], [B|Bs]) :-
    subs(X/T, A, B),
    subs_list(X/T, As, Bs).