{-#LANGUAGE ScopedTypeVariables#-}

module Main where

import Lib
import Misc
import Numeric.LinearAlgebra as NA
import Data.Foldable
import Data.Maybe
import Data.Default.Class
import Numeric.GSL.Minimization
import Optimise
import Data.Packed.Repa
import Data.Array.Repa (computeUnboxedP)
import Data.Array.Repa.IO.BMP
import Data.Word
import qualified Data.Array.Repa.Operators.Mapping as RM

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

  let Right r' = r
  let Right i = i_orig
  let a0 = [0,0,0,0,0]

  let (s,p) = minimize NMSimplex2 1E-2 50 ((\_ -> 10) <$> a0) (costAbs dr r' i) a0
  q <- computeUnboxedP $ RM.map dtg $ matrixToRepa i
  print "ready to write"
  writeImageToBMP "data/img.bmp" $ q

dtg :: Double -> (Word8,Word8,Word8)
dtg d = (w,w,w)
  where w = fromInteger $ round $ d * 255

costFunc :: [Matrix Double]     --Array of diff images
            -> Matrix Double    --Residual image
            -> Matrix Double    --Original image
            -> [Double]         --Alpha
            -> Double           --cost
costAbs :: [Matrix Double] -> Matrix Double -> Matrix Double -> [Double] -> Double
costAbs d r o a = sum $ NA.toList $ flatten $ cmap (abs) $ o - ad + r 
  where
    ad = foldl (+) (scalar 0.0) $ zipWith (\a d -> scalar a * d) a d