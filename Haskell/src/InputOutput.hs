-- Modulo destinado a leitura e escrita de arquivos.

module InputOutput
(getTraining,
 getTest,
 readIn,
 writeIn,
 isEmptyFile,
 definitiveAnswer,
 printEpoch
 ) where

import Data.List
import Data.Char
import Data.Maybe
import System.Directory
import System.Random
import Types
import Text.Printf
import Numeric.LinearAlgebra.HMatrix

type Image = [Double]
type Sample = (Int, Image)

-- Transforma uma String em uma lista
strToArr::String->[Double]
strToArr line = [(read x::Double)| x<-(words line)]
    
-- Transforma uma String em uma matrix
strToMatrix::String->[[Double]]
strToMatrix mt = [(strToArr x)| x<-(lines mt)]

-- Transforma uma lista em uma String
arrToStr::[Double]->String
arrToStr [] = ""
arrToStr (h:t) = (show h::String) ++ " " ++ (arrToStr t)

-- Transforma uma matriz em uma String
matrixToStr::[[Double]]->String
matrixToStr [] = ""
matrixToStr (h:t) = (arrToStr h) ++ "\n" ++ (matrixToStr t)

-- Dado o seu tamanho, gera uma lista 
-- com valores randomicos
listRandom::Int->IO [Double]
listRandom 0 = return []
listRandom sz = do 
    head <- randomIO::IO Double
    tail <- listRandom (sz-1)
    return (head:tail)

-- Dado o seu tamanho, gera uma matrix
-- com valores randomicos
matrixRandom::Int->Int->IO [[Double]]
matrixRandom 0 y = return []
matrixRandom x y = do
    head <- listRandom y
    tail <- matrixRandom (x-1) y
    return (head:tail)

-- Dado o seu caminho, le o arquivo
-- e o transforma em uma lista
getArchiveL::String->Int->IO [Double]
getArchiveL path sz = do
    emp <- isEmptyFile path
    if not emp then 
        do 
            archive <- (readFile path)
            return $ strToArr archive
    else 
        do
            ret <- listRandom sz
            return ret

-- Dado o seu caminho, le o arquivo
-- e o transforma em uma matriz
getArchiveM::String->Int->Int->IO [[Double]]
getArchiveM path x y = do
    emp <- isEmptyFile path
    if not emp then 
        do 
            archive <- readFile path
            return $ strToMatrix archive
    else 
        do
            ret <- matrixRandom x y
            return ret

-- Le os vetores e matrizes presentes nos arquivos para montar e retornar um Data
readIn::IO Data
readIn = do
    wH <- getArchiveM "Data/Weight_hidden.txt" 30 784
    bH <- getArchiveL "Data/Biases_hidden.txt" 30
    aH <- getArchiveL "Data/Activation_hidden.txt" 30
    zH <- getArchiveL "Data/Zeta_hidden.txt" 30

    wO <- getArchiveM "Data/Weight_Output.txt" 10 30
    bO <- getArchiveL "Data/Biases_Output.txt" 10
    aO <- getArchiveL "Data/Activation_Output.txt" 10
    zO <- getArchiveL "Data/Zeta_Output.txt" 10

    return $ (Data (fromLists wH) (fromList bH) (fromList aH) (fromList zH) (fromLists wO) (fromList bO) (fromList aO) (fromList zO))

-- Escreve o Data nos arquivos
writeIn::Data->IO() 
writeIn elem = do
    writeFile "NedeReural_Haskell/Data/Weight_hidden.txt" (matrixToStr (toLists $ wHidden elem))
    writeFile "NedeReural_Haskell/Data/Biases_hidden.txt" (arrToStr (toList $ bHidden elem))
    writeFile "NedeReural_Haskell/Data/Activation_hidden.txt" (arrToStr (toList $ aHidden elem))
    writeFile "NedeReural_Haskell/Data/Zeta_hidden.txt" (arrToStr (toList $ zetaHidden elem))

    writeFile "NedeReural_Haskell/Data/Weight_Output.txt" (matrixToStr (toLists $ wOutput elem))
    writeFile "NedeReural_Haskell/Data/Biases_Output.txt" (arrToStr (toList $ bOutput elem))
    writeFile "NedeReural_Haskell/Data/Activation_Output.txt" (arrToStr (toList $ aOutput elem))
    writeFile "NedeReural_Haskell/Data/Zeta_Output.txt" (arrToStr (toList $ zetaOutput elem))

-- Verifica se o arquivo esta vazio
isEmptyFile::String->IO Bool
isEmptyFile path = do
    elem <- readFile path

    if (length elem) == 0 then return True
    else return False

-- Pega o numero que define a resposta do teste/treino
pickNumber::FilePath->Int
pickNumber "" = -1
pickNumber (h:t)
                | h == '-' = digitToInt $ t!!0
                | otherwise = (pickNumber t)

-- Constroi uma imagem
makeImage::FilePath->IO Image
makeImage path = do
    elem <- readFile path
    let ret = (strToArr elem)
    return ret

-- Constroi um Sample
makeSample::FilePath->String->IO Sample
makeSample archive path = do
    let number = (pickNumber archive)
    mt <- (makeImage $ path++archive)
    return (number, mt)

-- Cria uma lista de Sample dado os arquivos e o local
listSample::[FilePath]->String->IO [Sample]
listSample [] path = return []
listSample (h:t) path = do
                head <- makeSample h path
                tail <- listSample t path
                return $ head:tail

-- Retorna o TestSet da rede
getTest::IO [Sample]
getTest = do
    let caminho = "NedeReural_Haskell/Tests" -- Diretorio de testes
    elems <- (getDirectoryContents caminho) -- Colocar diretorio
    ret <- listSample (filter (/= ".") $ filter (/= "..") elems) caminho 
    return ret

-- Retorna o TraninigSet da rede
getTraining::IO [Sample]
getTraining = do
    let caminho = "NedeReural_Haskell/Training" -- Diretorio de treino
    elems <- (getDirectoryContents caminho) -- Colocar diretorio
    ret <- listSample (filter (/= ".") $ filter (/= "..") elems) caminho 
    return ret

-- Imprime na tela a taxa de acerto de uma epoca de teste
printEpoch :: Int -> Int -> Int -> IO()
printEpoch epochIndex correctCnt total = putStrLn $ "EPOCH #" ++ (show epochIndex) ++ " - " ++ (show correctCnt) ++ " / " ++ (show total)

-- Recebe a lista de ativacao como parametro
-- Retorna uma tupla: o melhor sigmoid e seu respectivo indice
getBestSigmoid :: [Double] -> (Double, Int)
getBestSigmoid activationValues = let first = maximum (activationValues)
                                      second = fromJust $ (elemIndex (maximum (activationValues)) (activationValues))
                                  in (first, second)
                                  
-- Recebe a tupla (sigmoid, indice) 
-- Retorna a formatacao em string que representa
-- o numero e a probabilidade de ser esse numero
toPercentage :: (Double, Int) -> [Double] -> String
toPercentage sigmoid activationValues = 
    "[" ++ (printf "%d" (snd sigmoid + 1)) ++ " - " ++ (printf "%.2f" ((fst sigmoid) * 100 / sum (activationValues))) ++ "%]" 

-- Computa a resposta definitiva a partir do
-- array de valores de ativacao
definitiveAnswer :: Data -> IO String
definitiveAnswer activationValues = return ("Resposta definitiva: " ++ toPercentage (getBestSigmoid (toList $ aOutput activationValues)) (toList $ aOutput activationValues))