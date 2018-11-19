:- module(training, [train/1]).
:- [execution, inputOutput, matrix].
:- use_module(library(random)).

train(Amount) :- getTraining(TrainingSet),
                 getTest(TestSet),
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
                            % faz o shuffle no training seet
                            random_permutation(TrainingSet, ShuffledTrainingSet),
                            MinibatchAmount is 20,
                            MinibatchSize is TrainingSize // 20,
                            % cria uma matriz, onde cada linha é uma parte do training set
                            chunksOf(ShuffledTrainingSet, MinibatchAmount, Minibatches),
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
                                                    buildExpectedOutput(RepresentedInt, ExpectedOutput),
                                                    backpropagation(Network, Image, ExpectedOutput, DesiredChanges),
                                                    generateBasedOf(SumChanges, NetworkModel),
                                                    addNetworks(SumChanges, DesiredChanges, ChangesA),
                                                    manageSample(Minibatch, Counter, NetworkModel, ChangesB),
                                                    addNetworks(ChangesA, ChangesB, Changes).

buildExpectedOutput(RepresentedInt, ExpectedOutput) :-
                                                BasicOutput = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
                                                select(RepresentedInt, BasicOutput, RepresentedInt, ExpectedOutput).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
chunksOf(List, T, [Start|Rest]) :-
    append(Start, Remainder, List),
    length(Start, T),
    chunksOf(Remainder, T, Rest).

