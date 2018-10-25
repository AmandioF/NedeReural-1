-- Modulo destinado a todas as funcoes relativas ao treino da Rede.

module Training 
(train) where

import Execution
import InputOutput
import Types
import Data.List.Split

type Image = [Float]
type Sample = (Int, Image)

train :: Int -> IO String
train epochAmount = manageTrainingEpoch epochAmount trainingSet testSet network
                -- save network
                where   trainingSet = readTrainingSet
                        network = initialize
                        testSet = readTestSet

manageTrainingEpoch :: Int -> [Sample] -> [Sample] -> Data -> IO String
manageTrainingEpoch 0 _ _ _ = return ""
manageTrainingEpoch epochAmount trainingSets testSet network = do
                                                let newNetwork = trainingEpoch trainingSets network
                                                    correctCnt = testEpoch testSet newNetwork
                                                    totalAmount = length testSet
                                                do  return $ printEpoch epochAmount correctCnt totalAmount
                                                    manageTrainingEpoch (epochAmount - 1) trainingSets testSet newNetwork

trainingEpoch :: [Sample] -> Data -> Data
trainingEpoch trainingSet network = let minibatchAmount = 20 -- adequar quantidade
                                        minibatchSize = (length trainingSet) `div` 20 -- adequar quantidade
                                        -- dar shuffle
                                        minibatches = chunksOf minibatchSize trainingSet
                                        newNetwork = manageMinibatch minibatchAmount 0 network minibatches
                                    in newNetwork

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
                                        let network = feedforward (snd $ minibatch !! counter) networkModel
                                            expectedOutput = buildExpectedOutput (fst $ minibatch !! counter)
                                            desiredChanges = backpropagation network expectedOutput
                                            sumChanges = generateBasedOf networkModel
                                        in plus (plus sumChanges desiredChanges) (manageSample minibatch counter networkModel)
                                    else
                                        generateBasedOf networkModel

buildExpectedOutput :: Int -> [Float]
buildExpectedOutput representedValue = let indexes = [0.0 .. 9.0]
                                       in [if x == (fromIntegral representedValue) then x else 0.0 | x <- indexes]

-- Recebe as informacoes da rede neural, o resultado 
-- esperado e retorna um Data com as modificacoes necessarias
-- na rede
backpropagation :: Data -> [Float] -> Data
backpropagation network expectedOutput = Data [[]] [] [] [] [[]] [] [] []

--     | isEmpty network = error "Data is empty"
--     | expected < 0 || expected > 9 = error "Invalid expected number"
--     | otherwise = generateBasedOf network -- MUDAR, SÓ PRA RODAR
                 -- TODO


testEpoch :: [Sample] -> Data -> Int
testEpoch testSet network = manageEpoch testSet network (length testSet)

manageEpoch :: [Sample] -> Data -> Int -> Int
manageEpoch testSet network counter = if counter > 0
                                        then
                                            let newNetwork = feedforward (snd $ testSet !! counter) network
                                            in if (aOutput newNetwork) == buildExpectedOutput (fst $ testSet !! counter) 
                                                then
                                                    1 + manageEpoch testSet network (counter - 1)
                                                else
                                                    0 + manageEpoch testSet network (counter - 1)
                                        else
                                            0
                                            