{-#LANGUAGE ScopedTypeVariables#-}

module Misc(
    matrixFromCsv
  , zipMatrixWith
  ) where

import Data.Csv
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import Numeric.LinearAlgebra.Data as LA
import Control.Monad (join)

matrixFromCsv :: String -> IO (Either String (Matrix Double))
matrixFromCsv fpath = do
  raw <- BS.readFile fpath
  let (dec :: Either String (V.Vector [Double])) = decode NoHeader raw 
  return $ func <$> dec

func :: V.Vector [Double] -> Matrix Double
func v = (i><j) (join $ V.toList v)
  where 
    j = V.length v
    i = length $ V.head v

zipMatrixWith ::  (Double -> Double -> Double) 
                -> Matrix Double 
                -> Matrix Double 
                -> Matrix Double
zipMatrixWith f a b = fromLists $ zipWith (\x y -> zipWith f x y) a' b'
  where
    a' = toLists a
    b' = toLists b