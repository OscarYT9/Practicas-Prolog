:- op(1060, yfx, <->).   % doble implicación
:- op(1050, yfx, <-).    % implicación hacia la izquierda
:- op(600, yfx, v).      % disyunción
:- op(400, yfx, &).      % conjunción
:- op(200, fy, ~ ).      % negación
:- op(200, fx, forall).  % universal
:- op(200, fx, exists).  % existencial
:- op(300, xfy, ::).     % separador para cuantificadores

subs(X/T,X,T). 
subs(X/T,Y,Y) :- atom(Y), X=Y. 
subs(X/T,F,G) :- F =.. [H|T1], subs_list(X/T,T1,T2), G =.. [H|T2]. 
subs(X/T,F,F) :- atomic(F), !. subs_list(_,[],[]). 
subs_list(X/T,[H|T1],[H2|T2]) :- subs(X/T,H,H2), subs_list(X/T,T1,T2).