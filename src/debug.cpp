#include "debug.h"

void DEBUG_MATRIX(Narray a){
    for (int i = 0; i < a.row; i++) {
        for (int j = 0; j < a.colunm; j++) {
            std::cout << a.values[i][j] << " ";
        } std::cout << std::endl;
    }
}

void DEBUG_TEST(){
    std::cout << "D" << std::endl;
}