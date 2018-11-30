% :- module(training, [train/1, sigMatrix/2]).
:- [execution, inputOutput, matrix].
:- use_module(library(random)).
:- use_module(library(clpfd)).

% Treina a rede com Amount sendo a quantidade de epocas
% de treino.
train(Amount) :- getTrainings(TrainingSet),
                 getTests(TestSet),
                 getNetwork(Network),
                 manageTrainingEpoch(Amount, TrainingSet, TestSet, Network).

manageTrainingEpoch(0, _, _, Network) :- save(Network).
manageTrainingEpoch(Amount, TrainingSet, TestSet, Network) :- 
                                    trainingEpoch(TrainingSet, Network, NewNetwork),
                                    testEpoch(TestSet, NewNetwork, CorrectAmount),
                                    length(TestSet, TotalAmount),
                                    printEpoch(CorrectAmount, Amount, TotalAmount),
                                    NewAmount is Amount - 1,
                                    manageTrainingEpoch(NewAmount, TrainingSet, TestSet, NewNetwork).

trainingEpoch(TrainingSet, Network, NewNetwork) :-
                            length(TrainingSet, TrainingSize),
                            random_permutation(TrainingSet, ShuffledTrainingSet),
                            MinibatchAmount is 20,
                            MinibatchSize is TrainingSize // MinibatchAmount,
                            chunksOf(ShuffledTrainingSet, MinibatchSize, Minibatches),
                            manageMinibatch(MinibatchAmount, 0, Network, Minibatches, NewNetwork).

manageMinibatch(Amount, Counter, Network, _, NewNetwork) :- 
                            Index is Counter + 1, 
                            Amount =:= Index, 
                            generateBasedOf(Network, NewNetwork).

manageMinibatch(Amount, Counter, Network, Minibatches, NewNetwork) :- 
                            nth0(Counter, Minibatches, Minibatch),
                            minibatchEvaluation(Minibatch, Amount, Network, Changes),
                            addNetworks(Network, Changes, NetworkA),
                            NewCounter is Counter + 1,
                            manageMinibatch(Amount, NewCounter, Network, Minibatches, NetworkB),
                            addNetworks(NetworkA, NetworkB, NewNetwork).

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
                                                    generateBasedOf(NetworkModel, SumChanges),
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

% Efetua uma epoca de testes com TestSet
testEpoch(TestSet, Network, CorrectAmount) :- length(TestSet, TestSize), 
                                                 manageTestEpoch(TestSet, Network, TestSize, CorrectAmount).

manageTestEpoch(_, _, 0, 0).

% Executa cada teste de TestSet na rede Network, checando se foram bem sucedidos,
% onde CorrectAmount eh a quantidade de testes bem sucedidos.
manageTestEpoch(TestSet, Network, Counter, CorrectAmount) :- nth0(Counter, TestSet, Test),
                                                             nth0(0, Test, RepresentedInt),
                                                             nth0(1, Test, Image),
                                                             feedforward(Image, Network, NewNetwork),
                                                             nth0(6, NewNetwork, OutputActivations),
                                                             testOutput(OutputActivations, RepresentedInt, Result1),
                                                             NewCounter is Counter - 1,
                                                             manageTestEpoch(TestSet, Network, NewCounter, Result2),
                                                             CorrectAmount is Result1 + Result2.

% Verifica se o teste foi bem sucedido. Se sim, Result eh 1,
% senao, Result eh 0.
testOutput(OutputActivations, RepresentedInt, Result) :- expectedOutput(RepresentedInt, OutputActivations),
                                                         Result = 1.

testOutput(_, _, 0).

% Quebra List em partes de tamanho T,
% onde cada parte eh uma linha da matrix
% resultante.
chunksOf(_, _, []).

chunksOf(List, T, [Start|Rest]) :-
    append(Start, Remainder, List),
    length(Start, T),
    chunksOf(Remainder, T, Rest).

% Salva as partes relevantes da rede Network em arquivos.
save(Network) :- nth0(0, Network, HiddenWeights),
                 nth0(1, Network, HiddenBiases),
                 nth0(4, Network, OutputWeights),
                 nth0(5, Network, OutputBiases),
                 writeData(HiddenBiases, OutputBiases, HiddenWeights, OutputWeights).
