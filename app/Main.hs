{-#LANGUAGE ScopedTypeVariables#-}

module Main where

import Lib
import Misc
import Numeric.LinearAlgebra
import Data.Foldable
import Data.Maybe
import Data.Default.Class
import Optimise

printMas :: Show a => [a] -> IO ()
printMas [] = return ()
printMas (x:xs) = do
  print x
  printMas xs

insrt :: [a] -> Int -> a -> [a]
insrt m i e = (take i m) ++ [e] ++ (drop (i+1) m)

swap :: forall a. [a] -> Int -> Int -> [a]
swap x i j = insrt (insrt x i (x!!j)) j (x!!i) 


main :: IO ()
main = do
  d <- foldlM 
        (\acc fp -> do
          el <- matrixFromCsv fp
          return $ el:acc
        ) [] [  "data/data1.csv"
              , "data/data2.csv"
              , "data/data3.csv"
              , "data/data4.csv"
              , "data/data5.csv"
              ]
  i_noisy <- matrixFromCsv "data/I_noisy.csv"
  i_orig <- matrixFromCsv "data/I_orig.csv"
  r <- matrixFromCsv "data/residual.csv"
  let dr = catMaybes $ (\a -> case a of 
                          Left _ -> Nothing
                          Right v -> Just v) <$> d
  let lc = linComb dr [1..]
  let c = zipMatrixWith (+) (matrix 3 [1.0..9.0]) (matrix 3 [1.0..9.0])


  let p = [[0,0],[1.2,0],[0.0,0.8]]
  let s = Simplex p foo [] 1 0.5 2 (10**(-10)) (([],0),([],0),([],0)) []
  let res = validate 100 $ initSimplex s
  print $ best res
--  let Right i = i_orig
--  let Right r' = r
--  fminsearch def (costAbs dr r' i) [0,0,0,0,0] >>= print
  
  
  print $ sqp [0]

foo :: [Double] -> Double
foo xs@(x:y:[]) = 10 - x**2 - 4*x + y**2 - y - x*y

linComb :: [Matrix Double] -> [Double] -> Matrix Double
linComb d alpha = foldl (+) (scalar 0) $ zipWith (*) alpha' d
  where alpha' = scalar <$> alpha

costFunct :: (Double -> Double -> Double)   --calc_cost. e.g. abs diff, square etc
            -> [Matrix Double]              --Array of diff images
            -> Matrix Double                --Residual image
            -> Matrix Double                --Original image
            -> [Double]                     --Alpha
            -> Double                       --cost
costFunct f d r o a = (dot) (vector [1.0,1.0..]) $ flatten $ zipMatrixWith f o $ linComb d a + r

costAbs = costFunct (\a b -> abs (a - b))
costSqr = costFunct (\a b -> (a - b)**2)
costZeroes = costFunct (\a b -> if a == b then 0 else 1)
