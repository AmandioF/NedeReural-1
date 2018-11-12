:- module(inputOutput, [aaa/0, getTest/1, getTraining/1, printEpoch/3]).

aaa.

getTest(TestSet) :- TestSet = [].

getTraining(TrainingSet) :- TrainingSet = [].

printEpoch(CorrectAmount, Amount, TotalAmount) :- write("Epoch #"),
                                                  write(Amount),
                                                  write(" - "),
                                                  write(CorrectAmount),
                                                  write(" / "),
                                                  writeln(TotalAmount).
save(Network).