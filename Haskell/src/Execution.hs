-- Modulo destinado a todas as funcoes relativas a execucao da Rede.

module Execution
(execute,
 feedforward) where

import InputOutput
import Types
import Numeric.LinearAlgebra.HMatrix

type Image = [Double]
type Sample = (Int, Image)

-- execute, por retornar IO, pode interagir chamando outras IO Actions como
-- leitura dos weights, biases e input da Rede, por exemplo.
execute :: IO String -- mero esqueleto da funcao de execucao
execute = do 
        network <- readIn
        let image = [1.0 .. 784.0]
            executedNetwork = feedforward image network
        do definitiveAnswer executedNetwork            

-- recebe a imagem, a network e computa os calculos,
-- retornando a nova data com os valores de ativacao
-- e zeta do hidden e output alterados.
feedforward :: Image -> Data -> Data 
feedforward image network = let zH = add ((wHidden network) #> (fromList image)) (bHidden network)
                                aH = fromList $ sigV (toList zH)
                                zO = add ((wOutput network) #> aH) (bOutput network)
                                aO = fromList $ sigV (toList zO) 
                            in Data (wHidden network) (bHidden network) aH zH (wOutput network) (bOutput network) aO zO