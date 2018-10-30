#pragma once

#include "network.h"
#include "inputreader.h"
#include "output.h"
#include "binaryReader.h"

// Funcao que executa a rede neural
// apenas para fornecer uma resposta
std::string execute();


// Funcao que executa a rede neural no
// modo de treinamento
void train();

void save(InputReader);