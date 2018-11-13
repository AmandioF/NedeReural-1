:- module(training, [train/1]).
:- [src/execution, src/inputOutput].
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
                            MinibatchSize is TrainingSet // 20,
                            % cria uma matriz, onde cada linha é uma parte do training set
                            chunks_of(ShuffledTrainingSet, MinibatchAmount, Minibatches),
                            manageMinibatch(MinibatchAmount, 0, Network, Minibatches, NewNetwork).

manageMinibatch(Amount, Counter, Network, Minibatches, NewNetwork) :- NewNetwork = [].




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
chunks_of(List, T, [Start|Rest]) :-
    append(Start, Remainder, List),
    length(Start, T),
    chunks_of(Remainder, T, Rest).

