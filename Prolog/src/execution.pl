:- module(execution, [execute/0, getNetwork/1, generateBasedOf/2, addNetworks/3, divideNetworks/3]).
:- [src/matrix].
:- use_module(library(lists)).


execute.

getNetwork(Network) :- Network = [[[]], [], [[]], []].

generateBasedOf(Network, NewNetwork) :- nth0(0, Network, HiddenWeights),
                                        nth0(1, Network, HiddenBiases),
                                        nth0(2, Network, OutputWeights),
                                        nth0(3, Network, OutputBiases),
                                        maplist(replace(0), HiddenWeights, ZeroHWeights),
                                        replace(0, HiddenBiases, ZeroHBiases),
                                        maplist(replace(0), OutputWeights, ZeroOWeights),
                                        replace(0, OutputBiases, ZeroOBiases),
                                        NewNetwork = [ZeroHWeights, ZeroHBiases, ZeroOWeights, ZeroOBiases].

replace(_, [], []).
replace(NewValue, [_|T], [NewValue|T]) :- replace(NewValue, T, T).

addNetworks(NetworkA, NetworkB, NetworkC) :- nth0(0, NetworkA, HiddenWeightsA),
                                             nth0(1, NetworkA, HiddenBiasesA),
                                             nth0(2, NetworkA, OutputWeightsA),
                                             nth0(3, NetworkA, OutputBiasesA),
                                             nth0(0, NetworkB, HiddenWeightsB),
                                             nth0(1, NetworkB, HiddenBiasesB),
                                             nth0(2, NetworkB, OutputWeightsB),
                                             nth0(3, NetworkB, OutputBiasesB),
                                             addMatrix(HiddenWeightsA, HiddenWeightsB, HiddenWeightsC),
                                             maplist(addition, HiddenBiasesA, HiddenBiasesB, HiddenBiasesC),
                                             addMatrix(OutputWeightsA, OutputWeightsB, OutputWeightsC),
                                             maplist(addition, OutputBiasesA, OutputBiasesB, OutputBiasesC),
                                             NetworkC = [HiddenWeightsC, HiddenBiasesC, OutputWeightsC, OutputBiasesC].

divideNetworks(NetworkA, Constant, NetworkB) :- nth0(0, NetworkA, HiddenWeightsA),
                                               nth0(1, NetworkA, HiddenBiasesA),
                                               nth0(2, NetworkA, OutputWeightsA),
                                               nth0(3, NetworkA, OutputBiasesA),
                                               Multiplier is 1 / Constant,
                                               productByScalar(HiddenWeightsA, Multiplier, HiddenWeightsB),
                                               maplist(product(Multiplier), HiddenBiasesA, HiddenBiasesB),
                                               productByScalar(OutputWeightsA, Multiplier, OutputWeightsB),
                                               maplist(product(Multiplier), OutputBiasesA, OutputBiasesB),
                                               NetworkB = [HiddenWeightsB, HiddenBiasesB, OutputWeightsB, OutputBiasesB].
                                    