:- [src/execution, src/training].
:- initialization(main).

main :- read(Option), run(Option).

run(Option) :- (Option == "train", read(EpochAmount), train(EpochAmount));
               (Option == "exec", execute).