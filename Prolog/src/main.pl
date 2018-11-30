:- [execution, training].
:- initialization(main).

run(Option) :- (Option == "train", read(EpochAmount), train(EpochAmount));
               (Option == "exec", execute);
               (Option == "debug", writeln("Not bad")).

main :- current_prolog_flag(argv, Args), nth0(0, Args, Argument1), atom_string(Argument1, Option), run(Option).