:- module(inputOutput, [aaa/0, getTest/1, getTraining/1, printEpoch/3]).

aaa.

%% ----------------- Parte com funçoes de Escrita ----------------------------

% -- Escreve os dados da biases e do output
% nos seus respectivos arquivos
writeData(BH, BO, WH, WO):-
    writeFile("biasesHidden.txt", BH),
    writeFile("weightsHidden.txt", WH),

    writeFile("biasesOut.txt", BO),
    writeFile("weightsOut.txt", WO).

% Escreve no arquivo
writeFile(File, Matrix):-
    matrix_to_list(Matrix, Arr),

    clear(File),
    open(File, append, Stream),
    processW(Stream, Arr), nl,
    close(Stream).

% Cria uma lista com os caracteres presentes no Arquivo
processW(_, []).
processW(Stream, [Line|Rest]) :-
    write(Stream, Line), nl,
    processW(Stream, Rest).

% Transforma as listas da matriz em string
matrix_to_list([], []).
matrix_to_list([H|T], [C|R]):- 
    atomic_list_concat(H, " ", C),
    matrix_to_list(T, R).

clear(File):- open(File, write, Stream), write(Stream, ""), close(Stream).
    
% ------------------ Parte com funçoes de Leitura -------------------------

% Retorna a matriz de Treino
getTrainings(Train):- readTraining("trainings.txt", Train).

% Retorna a matriz de Testes
getTests(Train):- readTraining("tests.txt", Train).

% -- Retorna dados da Hidden
% Retorna a matriz de Treino
getBiasesHidden(Train):- readFile("biasesHidden.txt", Train).
% Retorna a matriz de Treino
getWeightsHidden(Train):- readFile("weightsHidden.txt", Train).

% -- Retorna dados do output
% Retorna a matriz de Treino
getBiasesOut(Train):- readFile("biasesOut.txt", Train).
% Retorna a matriz de Treino
getWeightsOut(Train):- readFile("biasesOut.txt", Train).

% Lê o arquivo e transforma-o em uma matriz de numeros
readFile(File, Arr):-
    open(File, read, Stream),

    read_line_to_codes(Stream, Line),

    % Setar a quantidade de linhas a ser lida
    processR(Line, Stream, F, 0, 600),
    arr_to_matrix(F, Arr),
    close(Stream).

% Lê um arquivo em forma de Treino
readTraining(File, Arr):-
    open(File, read, Stream),

    read_line_to_codes(Stream, Line),

    % Setar a quantidade de linhas a ser lida
    processR(Line, Stream, F, 0, 600),
    arr_to_training(F, Arr),
    close(Stream).

% Transforma as strings ,presentes na lista, em listas
arr_to_matrix([], []).
arr_to_matrix([H|T], [C|R]) :- string_to_arr(H, C), arr_to_matrix(T, R).

% Transforma as strings ,presentes na lista, em listas
arr_to_training([], []).
arr_to_training([H|T], [C|R]) :- 
    string_to_arr(H, [Label|Mt]),
    C = [Label, Mt],
    arr_to_training(T, R).

% Cria uma lista com os caracteres presentes no Arquivo
processR(end_of_file, _, [], _, _).
processR(_, _, [], Limit, Limit).
processR(Line, Stream, [Line|Ans], Cnt, Limit) :-
    read_line_to_codes(Stream, Line1),
    Bef is Cnt + 1,
    processR(Line1, Stream, Ans, Bef, Limit).

% Transforma todos os caracteres em Atomos
arr_to_atom([], []).
arr_to_atom([H|T], [C|B]):- string_to_atom(H, C), arr_to_atom(T, B).

% Transforma todos os Atomos da lista em numeros
arr_to_number([], []).
arr_to_number([H|T], [C|B]):- atom_number(H, C), arr_to_number(T, B).

% Transforma uma string em uma lista de numeros
string_to_arr(Str, Arr):-
    split_string(Str, " ", " ", Aux1),
    
    arr_to_atom(Aux1 , Aux2),
    arr_to_number(Aux2, Arr).

printEpoch(CorrectAmount, Amount, TotalAmount) :- write("Epoch #"),
                                                  write(Amount),
                                                  write(" - "),
                                                  write(CorrectAmount),
                                                  write(" / "),
                                                  writeln(TotalAmount).
save(Network).