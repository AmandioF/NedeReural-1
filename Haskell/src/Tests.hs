import Test.QuickCheck
import Execution
import InputOutput
import Types
import Data.List.Split
import System.Random
import System.Random.Shuffle
import Numeric.LinearAlgebra.HMatrix
import Numeric.LinearAlgebra.Data
import Training
import Execution


-- Exemplo de como usar:
--
-- $ ghci Tests.hs
--  > import Test.QuickCheck
--  > quickCheck prop_reverseReverse
 
prop_reverseReverse :: [Int] -> Bool
prop_reverseReverse xs = reverse (reverse xs) == xs
