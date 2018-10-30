#pragma once

#include "doriNum.h"

// Header da Rede

// Estrutura que representa uma camada de neuronios da rede neural
struct Layer {

    // Numero de neuronios da camada atual
    unsigned int numNeuronsThis;

    // Numero de neuronios da camada anterior
    unsigned int numNeuronsPrevious;

    // Matriz de pesos; w[i][j] representa o peso da aresta
    // que conecta o j-esimo neuronio da camada anterior
    // com o i-esimo neuronio da camada atual
    Narray weight;

    // Biases da camada
    Narray bias;
    
    // Valores de ativacao dos neuronios das camadas
    // No caso da camada de output cada i-esimo elemento
    // representa a resposta da rede para o i-esimo numero possivel
    Narray value;

    // Util para backpropagation: representa os valores
    // de ativacao dos neuronios antes da funcao sigmoid
    Narray zeta;

    // Construtor da camada
    Layer(unsigned int _numNeuronsThis, unsigned int _numNeuronsPrevious);

    //Construtor da camada vazia
    Layer();
    
    //preenche os valores de weight com os valores presentes em actual
    void fillWeight(Narray &actual);

    //preenche os valores de Bias com os valores presentes em actual
    void fillBias(Narray &actual);

    //preenche os valores de Value com os valores presentes em actual
    void fillValue(Narray &actual);

    // Funcao de ativacao dos neuronios da camada
    Narray activate(Narray previousValues);

};
