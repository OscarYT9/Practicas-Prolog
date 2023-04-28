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
subs(X/T, X, T).
subs(X/T, F, G) :-
    F =.. [Op|Args],
    maplist(subs(X/T), Args, Args1), 
    G =.. [Op|Args1], !.
