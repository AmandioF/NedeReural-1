:- module(training, [train/1]).
:- [execution, inputOutput, matrix].
:- use_module(library(random)).
:- use_module(library(clpfd)).

train(Amount) :- getTrainings(TrainingSet),
                 getTests(TestSet),
                 getNetwork(Network),
                 manageTrainingEpoch(Amount, TrainingSet, TestSet, Network).

manageTrainingEpoch(0, _, _, _).
manageTrainingEpoch(Amount, TrainingSet, TestSet, Network) :- 
                                    trainingEpoch(TrainingSet, Network, NewNetwork),
                                    testEpoch(TestSet, NewNetwork, CorrectAmount),
                                    length(TestSet, TotalAmount),
                                    printEpoch(CorrectAmount, Amount, TotalAmount),
                                    save(NewNetwork),
                                    NewAmount is Amount - 1,
                                    manageTrainingEpoch(NewAmount, TrainingSet, TestSet, NewNetwork).

trainingEpoch(TrainingSet, Network, NewNetwork) :-
                            length(TrainingSet, TrainingSize),
                            % faz o shuffle no training set
                            random_permutation(TrainingSet, ShuffledTrainingSet),
                            MinibatchAmount is 20,
                            MinibatchSize is TrainingSize // 20,
                            % cria uma matriz, onde cada linha é uma parte do training set
                            chunksOf(ShuffledTrainingSet, MinibatchAmount, Minibatches),
                            manageMinibatch(MinibatchAmount, 0, Network, Minibatches, NewNetwork).

manageMinibatch(Amount, Counter, Network, Minibatches, NewNetwork) :- 
                            nth0(Counter, Minibatches, Minibatch),
                            minibatchEvaluation(Minibatch, Amount, Network, Changes),
                            addNetworks(Network, Changes, NetworkA),
                            NewCounter is Counter + 1,
                            manageMinibatch(Amount, NewCounter, Network, Minibatches, NetworkB),
                            addNetworks(NetworkA, NetworkB, NewNetwork).

manageMinibatch(Amount, Counter, Network, _, NewNetwork) :- 
                                                    Index is Counter + 1, 
                                                    Amount =:= Index, 
                                                    generateBasedOf(Network, NewNetwork).

minibatchEvaluation(Minibatch, Amount, Network, AverageDesiredChanges) :- 
                                                            manageSample(Minibatch, Amount, Network, SumedDesiredChanges),
                                                            divideNetworks(SumedDesiredChanges, Amount, AverageDesiredChanges).

manageSample(_, Counter, NetworkModel, NetworkZero) :- 
                                                    Counter > 0, 
                                                    generateBasedOf(NetworkModel, NetworkZero).

manageSample(Minibatch, Counter, NetworkModel, Changes) :-
                                                    nth0(Counter, Minibatch, Sample),
                                                    nth0(1, Sample, RepresentedInt),
                                                    nth0(2, Sample, Image),
                                                    feedforward(Image, NetworkModel, Network),
                                                    expectedOutput(RepresentedInt, ExpectedOutput),
                                                    backpropagation(Network, Image, ExpectedOutput, NewNetwork),
                                                    generateBasedOf(SumChangNetworkModeles, NetworkModel),
                                                    addNetworks(SumChanges, NewNetwork, ChangesA),
                                                    manageSample(Minibatch, Counter, NetworkModel, ChangesB),
                                                    addNetworks(ChangesA, ChangesB, Changes).

expectedOutput(0, [[1.0],[0.0],[0.0],[0.0],[0.0],[0.0],[0.0],[0.0],[0.0],[0.0]]).
expectedOutput(1, [[0.0],[1.0],[0.0],[0.0],[0.0],[0.0],[0.0],[0.0],[0.0],[0.0]]).
expectedOutput(2, [[0.0],[0.0],[1.0],[0.0],[0.0],[0.0],[0.0],[0.0],[0.0],[0.0]]).
expectedOutput(3, [[0.0],[0.0],[0.0],[1.0],[0.0],[0.0],[0.0],[0.0],[0.0],[0.0]]).
expectedOutput(4, [[0.0],[0.0],[0.0],[0.0],[1.0],[0.0],[0.0],[0.0],[0.0],[0.0]]).
expectedOutput(5, [[0.0],[0.0],[0.0],[0.0],[0.0],[1.0],[0.0],[0.0],[0.0],[0.0]]).
expectedOutput(6, [[0.0],[0.0],[0.0],[0.0],[0.0],[0.0],[1.0],[0.0],[0.0],[0.0]]).
expectedOutput(7, [[0.0],[0.0],[0.0],[0.0],[0.0],[0.0],[0.0],[1.0],[0.0],[0.0]]).
expectedOutput(8, [[0.0],[0.0],[0.0],[0.0],[0.0],[0.0],[0.0],[0.0],[1.0],[0.0]]).
expectedOutput(9, [[0.0],[0.0],[0.0],[0.0],[0.0],[0.0],[0.0],[0.0],[0.0],[1.0]]).

backpropagation(Network, Image, ExpectedOutput, DesiredChanges) :-

                                                    nth0(3, Network, HZeta),
                                                    nth0(7, Network, OZeta),

                                                    nth0(6, Network, OActivation),
                                                    nth0(2, Network, HActivation),

                                                    nth0(2, Network, OWeight),

                                                    outputError(OActivation, ExpectedOutput, OZeta, OError),
                                                    hiddenError(OWeight, OError, HZeta, HError),

                                                    % Isso deve responder um NewNetwork zerado
                                                    generateBasedOf(Network, NewNetwork),

                                                    nth0(2, NewNetwork, HActivationsChange),
                                                    nth0(3, NewNetwork, HZetaValuesChange),
                                                    nth0(6, NewNetwork, OActivationsChange),
                                                    nth0(7, NewNetwork, OZetaValuesChange),

                                                    transpose(HActivation, HActivationTrans),
                                                    multMatrix(OError, HActivationTrans, OWeightsChangeModified),

                                                    transpose(Image, ImageTrans),
                                                    multMatrix(HError, ImageTrans, HWeightsChangeModified),

                                                    DesiredChanges = [HWeightsChangeModified, HError, HActivationsChange, HZetaValuesChange, 
                                                    OWeightsChangeModified, OError, OActivationsChange, OZetaValuesChange].


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


    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
chunksOf(List, T, [Start|Rest]) :-
    append(Start, Remainder, List),
    length(Start, T),
    chunksOf(Remainder, T, Rest).

