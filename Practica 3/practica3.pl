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
subs(X/T, X, T):- !.                  %  w/f(3), w, G
subs(X/T, H, H):- atom(H), !.           %  x/f(3), g(x), G

subs(X/T, F, G) :-                   
    F =.. [Op|Arg],
    Arg = [H|C],
    subs(X/T, H, T).

    subs(X/T, L, T),
    G =.. T.

subs(X v Y,)

subs([L|Fs],L,T) :-
    subs().



% L =[subs, a/f(3), a+4], X=..L.
% L =[subs, a/f(3), a+4]
% X = subs(a/f(3), a+4).
