%Óscar Vilela Rodríguez. 
%Nombre del archivo: "practica1.pl"

% Ejercicio 1: sublist
% Definir, utilizando el predicado append, un nuevo predicado sublist(S,L) de tal modo que S sea una sublista de L.
% Por ejemplo, la llamada ?- sublist(S,[1,2,3]). debe generar las listas S=[], S=[1], S=[1,2], S=[1,2,3], S=[2], S=[2,3], S=[3].
% Nota: se admite generar la misma lista varias veces.

sublist(S, L) :- 
    append(_, X, L),    % Utilizamos append para dividir L en dos partes:
                        % una parte inicial que ignoramos y una parte final que contiene a S como prefijo.
                        % X es una lista que empieza con S.
 
    append(S, _, X).    % Utilizamos append de nuevo para obtener S como prefijo de X.
                        % Es decir, S es una sublista de L que empieza en algún punto y termina en el final de L.

%_______________________________________________________________________________________________________________________________________________________________________________

% Ejercicio 2: insert
% Definir un predicado insert(X,L1,L2) de modo que X sea insertado de todos los modos posibles en la lista L1 para producir la lista L2.
% Por ejemplo, la llamada ?- insert(5,[1,2,3],L) debe generar las listas L=[5,1,2,3], L=[1,5,2,3], L=[1,2,5,3], L=[1,2,3,5].

insert(X, L1, L2) :-
    append(Prefix, Suffix, L1),     % Se utiliza el predicado append para dividir la lista en dos partes.
                                    % En Prefix se almacena la lista que va antes del lugar donde se quiere insertar X y en Suffix el resto de la lista L1.

    append(Prefix, [X|Suffix], L2). % Se utiliza el predicado append para construir la lista L2.
                                    % En Prefix se almacena la lista que va antes del lugar donde se quiere insertar X y se agrega X.
                                    % En Suffix se almacena el resto de la lista L1.
       
%_______________________________________________________________________________________________________________________________________________________________________________

% Ejercicio 3: del
% Definir, utilizando el predicado append, un nuevo predicado del(X,L1,L2) de modo que X sea eliminado (arbitrariamente) de la lista L1 para producir la lista L2.
% Por ejemplo, la llamada ?- del(0,[1,0,2,0,3,0,4],L). debe generar las listas L=[1,2,0,3,0,4], L=[1,0,2,3,0,4], L=[1,0,2,0,3,4].

del(X, L1, L2) :- 
    append(Prefix, [X|Suffix], L1), % Se utiliza el predicado append para dividir la lista en dos partes.
                                    % En Prefix se almacena la lista que va antes del elemento X y en Suffix el resto de la lista L1.

    append(Prefix, Suffix, L2).     % Se utiliza el predicado append para construir la lista L2.
                                    % En Prefix se almacena la lista que va antes del elemento X y en Suffix se almacena el resto de L1.

%_______________________________________________________________________________________________________________________________________________________________________________

% Ejercicio 4: perm
% Usar los predicados anteriores para definir perm(L1,L2) de modo que la lista L2 es cualquier permutación arbitraria de L1.

perm([], []).        % Caso base: la lista vacía se convierte en la lista vacía.
perm(L1,[X|L2]) :- 
    del(X,L1,Resto), % Se utiliza el predicado del para eliminar el elemento X de la lista L1 y generar la lista Resto.

    perm(Resto,L2).  % Se utiliza el predicado perm recursivamente para generar todas las permutaciones de Resto en L2.

%_______________________________________________________________________________________________________________________________________________________________________________

% Ejercicio 5: flatten
% Definir el predicado flatten(L1,L2) que elimina las listas anidadas poniendo todas las constantes al mismo nivel en una única lista.

flatten([], []).                    % Caso base: la lista vacía se convierte en la lista vacía.                           
flatten(X, [X]) :- \+ is_list(X).   % Caso en el que X no es una lista. Se agrega X a la lista Y, que es el resultado de aplanar la lista vacía.
flatten([X|Xs], Ys) :-              % Caso en el que X es una lista: se aplanan sus elementos y se concatenan con la lista resultante de aplanar el resto de la lista.
    flatten(X, Y),                  % Se aplanan los elementos de la cabeza de la lista.

    flatten(Xs, Ys1),               % Se aplanan el resto de los elementos de la lista.

    append(Y, Ys1, Ys).             % Se concatenan los resultados de las dos aplanaciones para obtener la lista aplanada final.
