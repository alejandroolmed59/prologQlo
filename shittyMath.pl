%Casos triviales

d(X, C, 0):- atom(X), not(number(X)), number(C),!. %Derivada constante Ej. 10

d(X, X, 1):- atom(X), not(number(X)), !. %Derivada por si misma. Ej x

d(X, X^N, N * X^E):- atom(X), not(number(X)), number(N), is(E, -(N, 1)), !. %derivada de x^N. Ej.  x^10

d(X, log(X), 1/X):- atom(X), not(number(X)), !. %Log natural de x. Ej log(x)

d(X, A^X, A^X * log(A)):- atom(X), not(number(X)), number(A), !. %Constante elevada a la X. Ej. 10^x

d(X, C*X, C):- atom(X), not(number(X)), number(C), !. %Constante por X . Ej. 10x

%Casos complejos (recursivos)
d(X, C * Ux, C * DUx):- atom(X), not(number(X)), number(C), d(X, Ux, DUx), !. % La derivada de una constante por una función:

d(X, Ux + Vx, DUx + DVx):- atom(X), not(number(X)), d(X, Ux, DUx), d(X, Vx, DVx), !. %Derivada suma de dos funciones

d(X, Ux - Vx, DUx - DVx):- atom(X), not(number(X)), d(X, Ux, DUx), d(X, Vx, DVx), !. %Derivada resta de dos funciones

d(X, Ux * Vx, DUx * Vx + Ux * DVx):- atom(X), not(number(X)), d(X, Ux, DUx), d(X,Vx, DVx), !. %Derivada producto de dos funciones

d(X, Ux ^ Vx, Ux ^ Vx * DVx * log(Ux) + Vx * DUx /Ux):- atom(X), not(number(X)),d(X, Ux, DUx), d(X, Vx, DVx), !. %derivada funcion elevada a otra funcion

d(X, C^Ux, C^Ux*log(C)* DUx ):- atom(X), not(number(X)), number(C), d(X, Ux, DUx), !. %La derivada de una constante elevada a una función:

d(X, log(Ux), DUx/Ux):- atom(X), not(number(X)), d(X, Ux, DUx), !. %Logaritmo de una funcion

d(X, Ux/Vx, (DUx*Vx - DVx*Ux)/(Vx^2)):- atom(X), not(number(X)), d(X, Ux, DUx), d(X,Vx, DVx), !. %Derivada cociente de dos funciones

%INTEGRALES
i(X, C , C * X):- atom(X), not(number(X)), number(C), !.

i(X, X, 0.5 * X^2):- atom(X), not(number(X)), !.

i(X, X^N, (1/R) * X^(R)):- atom(X), not(number(X)), number(N), is(R, +(N,1)), !.