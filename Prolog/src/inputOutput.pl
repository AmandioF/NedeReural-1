:- module(inputOutput, [aaa/0, getTest/1, getTraining/1, printEpoch/3]).

aaa.

% ----------------- Parte com funçoes de Escrita ----------------------------
% Escreve no arquivo
writeFile(File, Matrix):-
    % Transforma a matriz em string
    matrix_to_list(Matrix, Arr),
    atomic_list_concat(Arr, '\n', Str),
    
    % Escreve a string no arquivo
    open(File, write, Stream),
    write(Stream, Str), nl,
    close(Stream).

% Transforma as listas da matriz em string
matrix_to_list([], []).
matrix_to_list([H|T], [C|R]):- 
    atomic_list_concat(H, " ", C),
    matrix_to_list(T, R).
    
% ------------------ Parte com funçoes de Leitura -------------------------

% Lê o arquivo e transforma-o em uma matriz de numeros
readFile(File, Arr):-
    open(File, read, Stream),

    get_char(Stream, Char1),

    % Transforma o Arquivo em matriz
    process(Char1, Stream, F),
    atomic_list_concat(F, Juntando),
    split_string(Juntando, "\n", "\n", Separando),
    arr_to_matrix(Separando, Arr),
    close(Stream).

% Transforma as strings ,presentes na lista, em listas
arr_to_matrix([], []).
arr_to_matrix([H|T], [C|R]) :- string_to_arr(H, C), arr_to_matrix(T, R).

% Cria uma lista com os caracteres presentes no Arquivo
process(end_of_file, _, []).
process(Char, Stream, [Char|Ans]) :-
    get_char(Stream, Char1), 
    process(Char1, Stream, Ans).

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

getTest(TestSet) :- TestSet = [].

getTraining(TrainingSet) :- TrainingSet = [].

printEpoch(CorrectAmount, Amount, TotalAmount) :- write("Epoch #"),
                                                  write(Amount),
                                                  write(" - "),
                                                  write(CorrectAmount),
                                                  write(" / "),
                                                  writeln(TotalAmount).
save(Network).