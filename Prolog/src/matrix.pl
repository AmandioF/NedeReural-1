:- module(matrix, [dot/3, hadamardMatrix/3, multMatrix/3, addMatrix/3, addition/3, product/3, productByScalar/3]).
:- use_module(library(clpfd)).

% N is the dot product of lists V1 and V2.
dot(V1, V2, N) :- maplist(product,V1,V2,P), sumlist(P,N).
product(N1,N2,N3) :- N3 is N1*N2.
productByScalar(M1, K, M2) :- maplist(maplist(product(K)), M1, M2).

% Matrix multiplication with matrices represented
% as lists of lists. M3 is the product of M1 and M2
multMatrix(M1, M2, M3) :- transpose(M2,MT), maplist(mmHelper(MT), M1, M3).
mmHelper(M2, I1, M3) :- maplist(dot(I1), M2, M3).


addMatrix(M1, M2, M3) :- maplist(maplist(addition), M1, M2, M3).
addition(X,Y,Z) :- Z is X+Y.

%% Needs testing
hadamardMatrix(M1, M2, M3) :- maplist(maplist(mult), M1, M2, M3).
mult(X,Y,Z) :- Z is X*Y.