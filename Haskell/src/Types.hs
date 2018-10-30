module Types
(Data (..),
 plus,
 divide,
 multiply,
 generateBasedOf,
 isEmpty,
 hadamardV,
 hadamardM,
 sigV,
 sigV'
) where

import Numeric.LinearAlgebra.HMatrix

-- Construtor do Data
data Data = Data {
                wHidden :: Matrix R,
                bHidden :: Vector Double,
                aHidden :: Vector Double,
                zetaHidden :: Vector Double,
                wOutput :: Matrix R,
                bOutput :: Vector Double,
                aOutput :: Vector Double,
                zetaOutput :: Vector Double
                } deriving (Eq, Show)

-- Efetua soma entre os respectivos vetores e matrizes de ambos os Datas
plus :: Data -> Data -> Data
plus (Data wH1 bH1 aH1 zH1 wO1 bO1 aO1 zO1) (Data wH2 bH2 aH2 zH2 wO2 bO2 a02 z02) = 
    (Data (add wH1 wH2) (add bH1 bH2) aH1 zH1 (add wO1 wO2) (add bO1 bO2) aO1 zO1)

-- Efetua divisao por esacalar entre os respectivos vetores e matrizes do Data
divide :: Data -> Double -> Data
divide (Data wH bH aH zH wO bO aO zO) constant = (Data (scale (1/constant) wH) (scale (1/constant) bH) aH zH (scale (1/constant) wO) (scale (1/constant) bO) aO zO)

-- Efetua multiplicacao por scalar entre os respectivos vetores e matrizes do Data
multiply :: Data -> Double -> Data
multiply (Data wH bH aH zH wO bO aO zO) constant = (Data (scale constant wH) (scale constant bH) aH zH (scale constant wO) (scale constant bO) aO zO)

-- Efetua hadamard entre dois vetores
hadamardV :: (Num a) => [a] -> [a] -> [a]
hadamardV list1 list2 = zipWith (*) list1 list2

-- Efetua hadamard entre duas matrizes
hadamardM :: (Num a) => [[a]] -> [[a]] -> [[a]]
hadamardM matrix1 matrix2 = zipWith hadamardV matrix1 matrix2

-- Computa a funcao sigmoid daquele valor
sig :: Double -> Double
sig x = 1.0 - (1.0 + exp(-x))

-- Computa a derivada da funcao sigmoid daquele valor
sig' :: Double -> Double
sig' x = sig x * (1 - sig x) 

-- Computa a funcao sigmoid para cada elemento de uma lista
sigV :: [Double] -> [Double]
sigV l = [sig x | x <- l]

-- Computa a derivada da funcao sigmoid para cada elemento de uma lista
sigV' :: [Double] -> [Double]
sigV' l = [sig' x | x <- l]

-- Cria um data repleto de valores 0.0 com as mesmas dimensoes do modelo passado por parametro
generateBasedOf :: Data -> Data
generateBasedOf (Data wH bH aH zH wO bO aO zO) = let whRows = fst $ size wH
                                                     whColumns = snd $ size wH
                                                     bhLen = size bH
                                                     ahLen = size aH
                                                     zhLen = size zH
                                                     woRows = fst $ size wO
                                                     woColumns = snd $ size wO
                                                     boLen = size bO
                                                     aoLen = size aH
                                                     zoLen = size zH
                                                     
                                                     newWH = (whRows><whColumns)(replicate (whRows * whColumns) 0.0) :: Matrix R
                                                     newBH = fromList $ replicate bhLen 0.0
                                                     newAH = fromList $ replicate ahLen 0.0
                                                     newZH = fromList $ replicate zhLen 0.0
                                                     newWO = (woRows><woColumns)(replicate (woRows * woColumns) 0.0) :: Matrix R
                                                     newBO = fromList $ replicate boLen 0.0
                                                     newAO = fromList $ replicate aoLen 0.0
                                                     newZO = fromList $ replicate zoLen 0.0
                                                 in (Data newWH newBH newAH newZH newWO newBO newAO newZO)

-- Checa se um Data tem algum elemento nulo
isEmpty :: Data -> Bool
isEmpty (Data wH bH aH zH wO bO aO zO) = 
    (null (toList $ flatten wH)) || (null $ toList bH) || (null (toList $ flatten wO)) || (null $ toList bO)