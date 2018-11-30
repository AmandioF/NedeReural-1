:- module(execution, [execute/0, getNetwork/1, generateBasedOf/2, addNetworks/3, divideNetworks/3, feedforward/3]).
:- [matrix, inputOutput].
:- use_module(library(lists)).

% firstNet(InitNetwork) :- [InitHW, InitHB, InitHA, InitHZ, InitOW, InitOB, InitOA, InitOZ].



execute :-
    loadNetwork(InitNetwork, Network), 
    getImage(Image),
    feedforward(Image, Network, FinalNetwork).

    

% SEGUNDA LETRA MAIUSCULA: Matriz
% Segunda letra normal: Matriz coluna
%                                 HW     Hb    Ha    Hz    OW    Ob    Oa    Oz                  
getNetwork(Network) :- 
    getWeightsHidden(HW),
    getBiasesHidden(Hb),
    getWeightsOut(OW),
    getBiasesOut(Ob),
    Network = [HW, Hb, Hb, Hb, OW, Ob, Ob, Ob].

% NewNetwork eh uma rede de mesmo formato que Network
% porem com todo o conteudo composto de 0.0
generateBasedOf(Network, NewNetwork) :- nth0(0, Network, HiddenWeights),
                                        nth0(1, Network, HiddenBiases),
                                        nth0(2, Network, HiddenActivations),
                                        nth0(3, Network, HiddenZetaValues),
                                        nth0(4, Network, OutputWeights),
                                        nth0(5, Network, OutputBiases),
                                        nth0(6, Network, OutputActivations),
                                        nth0(7, Network, OutputZetaValues),
                                        maplist(replace(0.0), HiddenWeights, ZeroHWeights),
                                        maplist(replace(0.0), HiddenBiases, ZeroHBiases),
                                        maplist(replace(0.0), HiddenActivations, ZeroHActivations),
                                        maplist(replace(0.0), HiddenZetaValues, ZeroHZetaValues),
                                        maplist(replace(0.0), OutputWeights, ZeroOWeights),
                                        maplist(replace(0.0), OutputBiases, ZeroOBiases),
                                        maplist(replace(0.0), OutputActivations, ZeroOActivations),
                                        maplist(replace(0.0), OutputZetaValues, ZeroOZetaValues),
                                        NewNetwork = [ZeroHWeights, ZeroHBiases, ZeroHActivations, ZeroHZetaValues, 
                                                     ZeroOWeights, ZeroOBiases, ZeroOActivations, ZeroOZetaValues].

% A terceira lista eh a mesma que a segunda,
% porem onde cada um de seus valores eh NewValue.
replace(_, [], []).
replace(NewValue, [_|T], [NewValue|T]) :- replace(NewValue, T, T).

% NetworkC eh uma rede onde cada um de seus compontentes
% eh a soma dos respectivos componentes de NetworkA e NetworkB.
addNetworks(NetworkA, NetworkB, NetworkC) :- nth0(0, NetworkA, HiddenWeightsA),
                                             nth0(1, NetworkA, HiddenBiasesA),                                     
                                             nth0(2, Network, HiddenActivationsA),
                                             nth0(3, Network, HiddenZetaValuesA),
                                             nth0(4, NetworkA, OutputWeightsA),
                                             nth0(5, NetworkA, OutputBiasesA),
                                             nth0(6, Network, OutputActivationsA),
                                             nth0(7, Network, OutputZetaValuesA),
                                             nth0(0, NetworkB, HiddenWeightsB),
                                             nth0(1, NetworkB, HiddenBiasesB),
                                             nth0(2, Network, HiddenActivationsB),
                                             nth0(3, Network, HiddenZetaValuesB),
                                             nth0(4, NetworkB, OutputWeightsB),
                                             nth0(5, NetworkB, OutputBiasesB),
                                             nth0(6, Network, OutputActivationsB),
                                             nth0(7, Network, OutputZetaValuesB),
                                             addMatrix(HiddenWeightsA, HiddenWeightsB, HiddenWeightsC),
                                             addMatrix(HiddenBiasesA, HiddenBiasesB, HiddenBiasesC),
                                             addMatrix(HiddenActivationsA, HiddenActivationsB, HiddenActivationsC),
                                             addMatrix(HiddenZetaValuesA, HiddenZetaValuesB, HiddenZetaValuesC),
                                             addMatrix(OutputWeightsA, OutputWeightsB, OutputWeightsC),
                                             addMatrix(OutputBiasesA, OutputBiasesB, OutputBiasesC),
                                             addMatrix(OutputActivationsA, OutputActivationsB, OutputActivationsC),
                                             addMatrix(OutputZetaValuesA, OutputZetaValuesB, OutputZetaValuesC),
                                             NetworkC = [HiddenWeightsC, HiddenBiasesC, HiddenActivationsC, HiddenZetaValuesC, 
                                                        OutputWeightsC, OutputBiasesC, OutputActivationsC, OutputZetaValuesC].

% NetworkB eh uma rede onde cada um de seus componentes
% eh equivalente aos respectivos componentes de NetworkA porem
% divididos por Constant.
divideNetworks(NetworkA, Constant, NetworkB) :- nth0(0, NetworkA, HiddenWeightsA),
                                                nth0(1, NetworkA, HiddenBiasesA),                                     
                                                nth0(2, Network, HiddenActivationsA),
                                                nth0(3, Network, HiddenZetaValuesA),
                                                nth0(4, NetworkA, OutputWeightsA),
                                                nth0(5, NetworkA, OutputBiasesA),
                                                nth0(6, Network, OutputActivationsA),
                                                nth0(7, Network, OutputZetaValuesA),
                                                Multiplier is 1 / Constant,
                                                productByScalar(HiddenWeightsA, Multiplier, HiddenWeightsB),
                                                productByScalar(HiddenBiasesA, Multiplier, HiddenBiasesB),
                                                productByScalar(HiddenActivationsA, Multiplier, HiddenActivationsB),
                                                productByScalar(HiddenZetaValuesA, Multiplier, HiddenZetaValuesB),
                                                productByScalar(OutputWeightsA, Multiplier, OutputWeightsB),
                                                productByScalar(OutputBiasesA, Multiplier, OutputBiasesB),
                                                productByScalar(OutputActivationsA, Multiplier, OutputActivationsB),
                                                productByScalar(OutputZetaValuesA, Multiplier, OutputZetaValuesB),
                                                NetworkB = [HiddenWeightsB, HiddenBiasesB, HiddenActivationsB, HiddenZetaValuesB,
                                                            OutputWeightsB, OutputBiasesB, OutputActivationsB, OutputZetaValuesB].

% Ativacao camada-a-camada da rede, onde ExecutedNetwork
% eh a rede Network com valores de ativacao e zeta alterados
% de acordo com os calculos utilizando Image.
feedforward(Image, Network, ExecutedNetwork) :- nth0(0, Network, HiddenWeights),
                                                nth0(1, Network, HiddenBiases),
                                                multMatrix(HiddenWeights, Image, ProductMatrix1),
                                                addMatrix(ProductMatrix1, HiddenBiases, HiddenZetaValues),
                                                sigMatrix(HiddenZetaValues, HiddenActivations),
                                                nth0(4, Network, OutputWeights),
                                                nth0(5, Network, OutputBiases),
                                                multMatrix(OutputWeights, HiddenActivations, ProductMatrix2),
                                                addMatrix(ProductMatrix2, OutputBiases, OutputZetaValues),
                                                sigMatrix(OutputZetaValues, OutputActivations),
                                                ExecutedNetwork = [HiddenWeights, HiddenBiases, HiddenActivations, HiddenZetaValues,
                                                OutputWeights, OutputBiases, OutputActivations, OutputZetaValues]. 
