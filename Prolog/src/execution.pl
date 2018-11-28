:- module(execution, [execute/0, getNetwork/1, generateBasedOf/2, addNetworks/3, divideNetworks/3]).
:- [matrix].
:- use_module(library(lists)).


execute.

% CAPTION: Matrix
% normal: column matrix
%                                 HW     Hb    Ha    Hz    OW    Ob    Oa    Oz                  
getNetwork(Network) :- Network = [[[]], [[]], [[]], [[]], [[]], [[]], [[]], [[]]].

generateBasedOf(Network, NewNetwork) :- nth0(0, Network, HiddenWeights),
                                        nth0(1, Network, HiddenBiases),
                                        nth0(2, Network, HiddenActivations),
                                        nth0(3, Network, HiddenZetaValues),
                                        nth0(4, Network, OutputWeights),
                                        nth0(5, Network, OutputBiases),
                                        nth0(6, Network, OutputActivations),
                                        nth0(7, Network, OutputZetaValues),
                                        maplist(replace(0), HiddenWeights, ZeroHWeights),
                                        maplist(replace(0), HiddenBiases, ZeroHBiases),
                                        maplist(replace(0), HiddenActivations, ZeroHActivations),
                                        maplist(replace(0), HiddenZetaValues, ZeroHZetaValues),
                                        maplist(replace(0), OutputWeights, ZeroOWeights),
                                        maplist(replace(0), OutputBiases, ZeroOBiases),
                                        maplist(replace(0), OutputActivations, ZeroOActivations),
                                        maplist(replace(0), OutputZetaValues, ZeroOZetaValues),
                                        NewNetwork = [ZeroHWeights, ZeroHBiases, ZeroHActivations, ZeroHZetaValues, 
                                                     ZeroOWeights, ZeroOBiases, ZeroOActivations, ZeroOZetaValues].

replace(_, [], []).
replace(NewValue, [_|T], [NewValue|T]) :- replace(NewValue, T, T).

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
                                    