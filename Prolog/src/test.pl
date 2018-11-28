:- initialization(main).
:- [execution, inputOutput, matrix].
:- use_module(library(clpfd)).

% main :- O1 is 2 / 10, O2 is 7 / 10, outputError([[O1], [O2]], [[0], [1]], [[0], [0]], Res), writeln(Res).

main :- 
    O1 is 2 / 10, O2 is 7 / 10,
    OActivation = [[1], [0]],
    ExpectedOutput = [[0], [1]],
    HActivation = [[1], [0]],
    OBias = [[0], [0]],
    OWeight = [[0, 0], [0, 0]],
    zeta(OWeight, HActivation, OBias, OZeta),
    outputError(OActivation, ExpectedOutput, OZeta, OError), 
    writeln(OError).


sig(Elem, Res) :- Res is 1 / 1 + exp(-Elem).
derivativeSig(Elem, Res) :- sig(Elem, S), Res is S * (1 - S).
derivativeSigMatrix(List, Res) :- maplist(maplist(derivativeSig), List, Res).

outputError(OActivation, ExpectedOutput, OZeta, Res) :- 
    derivativeSigMatrix(OZeta, ZetaDSig),
    subMatrix(OActivation, ExpectedOutput, Sub),
    hadamardMatrix(Sub, ZetaDSig, Res).

hiddenError(OWeight, OError, HZeta, Res) :-
    derivativeSigMatrix(HZeta, HZetaDSig),
% Maybe transpose won't work (?)
transpose(OWeight, OWeightTrans),
multMatrix(OWeightTrans, OError, MultRes),
hadamardMatrix(MultRes, HZetaDSig, Res).    

zeta(Weights, LastActivation, Bias, Res) :-
    % zeta = (weight * previousValues) + bias;
    multMatrix(Weights, LastActivation, Mult),
    addMatrix(Mult, Bias, Res).