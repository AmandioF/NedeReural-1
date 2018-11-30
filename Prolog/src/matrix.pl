:- module(matrix, [dot/3, sigMatrix/2, derivativeSigMatrix/2, hadamardMatrix/3, subMatrix/3, multMatrix/3, addMatrix/3, addition/3, product/3, productByScalar/3]).
:- use_module(library(clpfd)).

% N eh o produto escalar das listas V1 e V2.
dot(V1, V2, N) :- maplist(product,V1,V2,P), sumlist(P,N).

% N3 eh o produto de N1 e N2
product(N1,N2,N3) :- N3 is N1*N2.

% M2 eh o produto da matriz M1 pela constante K
productByScalar(M1, K, M2) :- maplist(maplist(product(K)), M1, M2).

% Multiplicacao de matrizes com as matrizes M1 e M2
% onde M3 eh o produto.
multMatrix(M1, M2, M3) :- transpose(M2,MT), maplist(mmHelper(MT), M1, M3).
mmHelper(M2, I1, M3) :- maplist(dot(I1), M2, M3).

% M3 eh a soma de matrizes entre as matrizes M1 e M2
addMatrix(M1, M2, M3) :- maplist(maplist(addition), M1, M2, M3).
addition(X,Y,Z) :- Z is X+Y.

% M3 eh a matriz de hadammard das matrizes M1 e M2
hadamardMatrix(M1, M2, M3) :- maplist(maplist(mult), M1, M2, M3).
mult(X,Y,Z) :- Z is X*Y.

% M3 eh a matriz da subtracao entre M1 e M2
subMatrix(M1, M2, M3) :- maplist(maplist(subtraction), M1, M2, M3).
subtraction(X,Y,Z) :- Z is X - Y.

sig(Elem, Res) :- Res is 1 / (1 + exp(-Elem)).
sigMatrix(List, Res) :- maplist(maplist(sig), List, Res).

derivativeSig(Elem, Res) :- sig(Elem, S), Res is S * (1 - S).
derivativeSigMatrix(List, Res) :- maplist(maplist(derivativeSig), List, Res).