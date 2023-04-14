:- op(1060, yfx, <->).  % doble implicación
:- op(1050, yfx, <-).   % implicación hacia la izquierda
:- op(800, yfx, xor).   % disyunción exclusiva
:- op(600, yfx, v).     % disyunción
:- op(400, yfx, &).     % conjunción
:- op(200, fy, ~ ).     % negación

define((F xor G), ~ (F <-> G)).
define((F <-> G), (F -> G) & (F <- G)).
define((F <- G), (G -> F)).
define((F -> G), ~ F v G).


%?- unfold( p <-> q & ¬ r, G ).
%G =  (¬p v q& ¬r)&(¬ (q& ¬r)v p).

%?- unfold( (a -> b) xor c, G ).
%G = ¬ ((¬ (¬a v b)v c)&(¬c v (¬a v b))).


unfold(F,G) :- atom(F), G=F.

unfold(F, G) :- 
    define(F, G).

%unfold(F -> G, S) :- S= ~ F v G.


%unfold(F, DG & DH) :- 
    %(F = (G & H); F = (G v H)),
    %define(G, DG),
    %define(H, DH).

unfold(~F,~G) :- unfold(F,G).

unfold(F & G, S) :-
    unfold(F, H1),
    unfold(G, K1),
    (H1 = (X1 & X2), unfold(X1, DH1), unfold(X2, DK1), unfold(DH1 & DK1, DH3)),
    (K1 = (X3 v X4), unfold(X3, DH2), unfold(X4, DK2), unfold(DH2 v DK2, DK4)),
    S = DH3 & DK4.

unfold(F v G, S) :-
    unfold(F, H1),
    unfold(G, K1),
    (H1 = (X1 & X2), unfold(X1, DH1), unfold(X2, DK1), unfold(DH1 v DK1, DH3)),
    (K1 = (X3 v X4), unfold(X3, DH2), unfold(X4, DK2), unfold(DH2 v DK2, DK4)),
    S = DH3 v DK4.



%unfold(F v G, H v K) :- unfold(F, H), unfold(G, K).













%unfold(F, G) :- F =.. [Op| [Arg1, Arg2]], unfold(Arg1, G1), unfold(Arg2, G2), G =.. [Op, G1, G2].

%unfold(F, X1 v X2) :- F=.. [Op|[F1|F2]] , unfold(F1,X1), unfold(F2,X2).

% 1+1=.. L.
% L=[+,1,1]

%familia(leonor, felipe, juancarlos)
%F=[familia, leonor, felipe, juancarlos]

%F= a v b, F=.. [0p, F1, F2]