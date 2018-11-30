:- [execution, training].
:- initialization(main).

run(Option) :- (Option == "train", read(EpochAmount), train(EpochAmount));
               (Option == "exec", execute);
               (Option == "debug", writeln("Not bad")).

main :- read(Option), run(Option).