:- module(training, [train/1]).
:- [src/execution, src/inputOutput].

train(Amount) :- getTraining(TrainingSet),
                 getTest(TestSet),
                 getNetwork(Network),
                 manageTrainingEpoch(Amount, TrainingSet, TestSet, Network).

manageTrainingEpoch(0, _, _, _).
manageTrainingEpoch(Amount, TrainingSet, TestSet, Network) :- 
                                    trainingEpoch(TrainingSet, Network, newNetwork),
                                    testEpoch(TestSet, NewNetwork, CorrectAmount),
                                    length(TestSet, TotalAmount),
                                    printEpoch(CorrectAmount, Amount, TotalAmount),
                                    save(NewNetwork),
                                    NewAmount is Amount - 1,
                                    manageTrainingEpoch(NewAmount, TrainingSet, TestSet, NewNetwork).

trainingEpoch(TrainingSet, Network, NewNetwork) :-
                            length(TrainingSet, TrainingSize),
                            % shuffle TrainingSet
                            MinibatchAmount is 20,
                            MinibatchSize is TrainingSet // 20,
                            % take chunks of MinibatchSize from TrainingSet
                            Minibatches = [],
                            manageMinibatch(MinibatchAmount, 0, Network, Minibatches, NewNetwork).

manageMinibatch(Amount, Counter, Network, Minibatches, NewNetwork) :- NewNetwork = [].