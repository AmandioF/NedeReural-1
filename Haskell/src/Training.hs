-- Modulo destinado a todas as funcoes relativas ao treino da Rede.

module Training 
(train) where

import Execution
import InputOutput
import Types
import Data.List.Split
import System.Random
import System.Random.Shuffle
import Numeric.LinearAlgebra.HMatrix
import Numeric.LinearAlgebra.Data

type Image = [Double]
type Sample = (Int, Image)

-- Funcao de treino que executa uma
-- dada quantidade de epocas de treino
train :: Int -> IO String
train epochAmount = do 
                    trainingSet <- getTraining
                    testSet <- getTest
                    network <- readIn
                    manageTrainingEpoch epochAmount trainingSet testSet network

-- 
manageTrainingEpoch :: Int -> [Sample] -> [Sample] -> Data -> IO String
manageTrainingEpoch 0 _ _ _ = return ""
manageTrainingEpoch epochAmount trainingSet testSet network = do
                                                newNetwork <- trainingEpoch trainingSet network
                                                let 
                                                    correctCnt = testEpoch testSet newNetwork
                                                    totalAmount = length testSet
                                                do  return $ printEpoch epochAmount correctCnt totalAmount
                                                    writeIn newNetwork
                                                    manageTrainingEpoch (epochAmount - 1) trainingSet testSet newNetwork

trainingEpoch :: [Sample] -> Data -> IO Data
trainingEpoch trainingSet network = do
                                standardGenerator <- getStdGen
                                let trainingSize = length trainingSet
                                    shuffledTrainingSet = shuffle' trainingSet trainingSize standardGenerator
                                    minibatchAmount = 20 -- adequar quantidade
                                    minibatchSize = trainingSize `div` 20 -- adequar quantidade
                                    minibatches = chunksOf minibatchSize shuffledTrainingSet
                                    newNetwork = manageMinibatch minibatchAmount 0 network minibatches
                                return newNetwork

manageMinibatch :: Int -> Int -> Data -> [[Sample]] -> Data
manageMinibatch amount counter network minibatches = if amount /= (counter + 1) 
                                                        then  
                                                          let changes = minibatchEvaluation (minibatches !! counter) amount network
                                                          in plus (plus network changes) (manageMinibatch amount (counter + 1) network minibatches)
                                                        else
                                                          generateBasedOf network 

minibatchEvaluation :: [Sample] -> Int -> Data -> Data
minibatchEvaluation minibatch amount network = let sumedDesiredChanges = manageSample minibatch amount network
                                                   averageDesiredChanges = divide sumedDesiredChanges (fromIntegral amount)
                                               in averageDesiredChanges

manageSample :: [Sample] -> Int -> Data -> Data
manageSample minibatch counter networkModel = if counter > 0
                                    then
                                        let representedInt = fst $ minibatch !! counter
                                            image = snd $ minibatch !! counter
                                            network = feedforward image networkModel
                                            expectedOutput = buildExpectedOutput representedInt
                                            desiredChanges = backpropagation network image expectedOutput
                                            sumChanges = generateBasedOf networkModel
                                        in plus (plus sumChanges desiredChanges) (manageSample minibatch counter networkModel)
                                    else
                                        generateBasedOf networkModel

buildExpectedOutput :: Int -> [Double]
buildExpectedOutput representedValue = let indexes = [0.0 .. 9.0]
                                       in [if x == (fromIntegral representedValue) then x else 0.0 | x <- indexes]

-- Recebe as informacoes da rede neural, o resultado 
-- esperado e retorna um Data com as modificacoes necessarias
-- na rede
-- N = network, E = expected list, I = image values
backpropagation :: Data -> Image -> [Double] -> Data
backpropagation n i e
    | isEmpty n = error "Backpropagation error: Data is empty"
    | length e /= 10 = error "Backpropagation error: expectedOutput list is invalid"
    | otherwise = let outputError = computeOutputError (aOutput n) (fromList e) (zetaOutput n)
                      hiddenError = computeHiddenError (wOutput n) outputError (zetaHidden n)
                      outputDesiredChanges = computeOutputDesiredChanges outputError (aHidden n)
                      hiddenDesiredChanges = computeHiddenDesiredChanges hiddenError i
                      wH = fst hiddenDesiredChanges
                      bH = snd hiddenDesiredChanges
                      wO = fst outputDesiredChanges
                      bO = snd outputDesiredChanges
                      newNetwork = (Data wH bH (aHidden n) (zetaHidden n) wO bO (aOutput n) (zetaOutput n))  
                  in newNetwork

-- Gera o vetor de erro da camada output, recebe
-- as ativacoes do output atual, as ativacoes esperadas 
-- e a lista zeta do output
computeOutputError :: Vector Double -> Vector Double -> Vector Double -> Vector Double
computeOutputError a e z = fromList $ hadamardV (toList (add a (scale (-1) e))) (sigV' $ toList z)

computeHiddenError :: Matrix R -> Vector Double -> Vector Double -> Vector Double
computeHiddenError ow oe hz = fromList $ hadamardV (toList $ (tr' ow) #> oe) (sigV' (toList hz)) 

computeOutputDesiredChanges :: Vector Double -> Vector Double -> (Matrix R, Vector Double)
computeOutputDesiredChanges oe ah = let owDesired = oe `outer` ah
                                        ohDesired = oe
                                    in (owDesired, ohDesired)

computeHiddenDesiredChanges :: Vector Double -> Image -> (Matrix R, Vector Double)
computeHiddenDesiredChanges he image = let hwDesired = he `outer` (fromList image)
                                           hbDesired = he
                                       in (hwDesired, hbDesired)

testEpoch :: [Sample] -> Data -> Int
testEpoch testSet network = manageEpoch testSet network (length testSet)

manageEpoch :: [Sample] -> Data -> Int -> Int
manageEpoch testSet network counter = if counter > 0
                                        then
                                            let newNetwork = feedforward (snd $ testSet !! counter) network
                                            in if (toList $ aOutput newNetwork) == buildExpectedOutput (fst $ testSet !! counter) 
                                                then
                                                    1 + manageEpoch testSet network (counter - 1)
                                                else
                                                    0 + manageEpoch testSet network (counter - 1)
                                        else
                                            0
                                            
