{-#LANGUAGE ScopedTypeVariables, RecordWildCards#-}

module Optimise where

import Control.Monad.Random
import Data.List
import Data.Ord
import Data.Default.Class
import Debug.Trace

type Point = [Double]
type GBU = ((Point,Double),(Point,Double),(Point,Double))

--| Scalar to point multiplication; Non-commutative
spm :: Double -> Point -> Point
spm s p = (s*) <$> p

--| Point by scalar division; Handy for computing central point
psd :: Point -> Double -> Point
psd p s = (\e -> e / s) <$> p

--| Point-point summation
pps :: Point -> Point -> Point
pps = zipWith (+)

--| dot-product of two points
dot' :: Point -> Point -> Double
dot' a b = sum $ zipWith (*) a b

--| dot-product with self
sqp :: Point -> Double
sqp x = x `dot'` x

--| Main data type
data Simplex = Simplex{
  sPoints   :: [Point]            -- Vertices of simplex
, sFunc     :: Point -> Double    -- Function to optimise
, sResults  :: [Double]           -- Values of sFunc at simplex's vertices 
, sAlpha    :: Double             -- alpha parameter for reflection
, sBetha    :: Double             -- betha -- for contraction
, sGamma    :: Double             -- gamma for extention
, sError    :: Double             -- error value for validation
, sGBU      :: GBU                -- The good the bad and the ugly 
                                  -- vertices with respective sFunc values  
, sCentral  :: Point              -- central point
}

-- | get the best vertice (the Good)
best :: Simplex -> (Point,Double)
best    (Simplex{..})  = (\(a,_,_) -> a) sGBU

-- | get the second best vertice (the Bad)
sndBest :: Simplex -> (Point,Double)
sndBest (Simplex{..})  = (\(_,a,_) -> a) sGBU

-- | get the worst vertice (the Ugly)
worst :: Simplex -> (Point,Double)
worst   (Simplex{..})  = (\(_,_,a) -> a) sGBU


-- | init simplex: calc the sFunc, GBU and central point
initSimplex :: Simplex -> Simplex
initSimplex s@(Simplex {..}) = s{
      sPoints = sPoints'
    , sResults = sResults'
    , sGBU = gbu
    , sCentral = xc
  } 
  where
    xfx = sortOn (\(_,a) -> Down a) $ (\x -> (x,sFunc x)) <$> sPoints
    gbu = (head xfx, xfx!!1, last xfx)
    (sPoints',sResults') = unzip xfx
    xc = centralPoint $ tail sPoints'

--| reflect the point in respect to the central one
reflect :: Double -> Point -> Point -> Point
reflect a c x = ((1 + a) `spm` c) `pps` ((-a) `spm` x)

--| extend the point in respect to the central one
extend :: Double -> Point -> Point -> Point
extend g c x = ((1 - g) `spm` c) `pps` (g `spm` x) 

--| contract the point in respect to the central one
contract :: Double -> Point -> Point -> Point
contract b c x = (b `spm` x) `pps` ((1 - b) `spm` c)

--| calculate the central point (average)
centralPoint :: [Point] -> Point
centralPoint p = (foldl pps [0,0..] p) `psd` (fromIntegral $ length p)

--| shrink the whole simplex
shrink :: [Point] -> Point -> [Point]
shrink ps c = (\p -> (p `pps` c) `psd` 2.0) <$> ps

data FminOpts = FminOpts{
  oAlpha :: Double  -- alpha   
, oBetha :: Double  -- betha
, oGamma :: Double  -- gamma
, oError :: Double  -- error value
, oScale :: Double  -- the bigger the scale - the bigger the initial simplex
, oMaxn  :: Int     -- maximum number of steps
}

instance Default FminOpts where
  def = FminOpts 1.0 0.5 2 0.00001 1000 10000

fminsearch :: FminOpts                  --| minimisation options
              -> ([Double] -> Double)   --| function to minimize
              -> [Double]               --| initial point
              -> IO ((Point,Double))    --| (best point, best value)
fminsearch FminOpts{..} f p0 = do
  p <- getSimplexPoints oScale p0
  print p
  let s = Simplex p f [] oAlpha oBetha oGamma oError (([],0),([],0),([],0)) []
  let res = validate oMaxn $ initSimplex s
  return $ best res

--| Place the x among g,b,u. e.g: g < x < b < u => 1
placeAt :: Double -> GBU -> Int
placeAt x (g,b,u) = trace ("x:"
    ++ show x
    ++ "\tgbu=" ++ show (g,b,u)) $ foldl (\a e -> if (x < e) then a else a + 1) 0 y'
  where y' = snd <$> [g,b,u]

--| generate n+1 simplex vertices from n-dimension 
getSimplexPoints :: Double -> [Double] -> IO [[Double]]
getSimplexPoints scale p = do
  (x::[Double])  <- getRandoms
  let res = foldl (\acc e -> (addAt e p):acc) [] $ zip [0 .. length p - 1] $ (*scale) <$> x
  return $ p:res
  where
    addAt :: (Int,Double) -> [Double] -> [Double]
    addAt (i,d) a = 
      if i < 0 || i > length a 
        then a 
        else take i a ++ [d'] ++ drop (i+1) a
      where d' = d + a!!i 

--| Run one step of the search 
stepSearch :: Simplex -> Simplex
stepSearch s0@(Simplex{..}) = 
  case cs of
    0 -> trace "case0:" $ if (fe < fr) 
          then s{
              sGBU = (g,b,(xe,fe))
            , sPoints = (init sPoints) ++ [xe]
            , sResults = (init sResults) ++ [fe]
          }
          else s{
              sGBU = (g,b,(xr,fr))
            , sPoints = (init sPoints) ++ [xr]
            , sResults = (init sResults) ++ [fr]
          }
    1 -> trace "case1:" $ s{
        sGBU = (g,b,(xr,fr))
      , sPoints = (init sPoints) ++ [xr]
      , sResults = (init sResults) ++ [fr]
      }
    2 -> trace "case2:" $ squezze s{
            sGBU = (g,b,(xr,fr))
          , sPoints = (init sPoints) ++ [xr]
          , sResults = (init sResults) ++ [fr]
          }
    _ -> trace "case3:" $ squezze s
  where
    s   = initSimplex s0
    (g,b,u) = sGBU
    xr  = reflect sAlpha sCentral $ fst $ worst s
    fr  = sFunc xr
    xe  = extend sGamma sCentral xr
    fe  = sFunc xe
    cs  = placeAt fr sGBU

--| 
squezze :: Simplex -> Simplex
squezze s@(Simplex{..}) = 
  if fs < (snd u)
    then s{
          sGBU = (g,b,(xs,fs))
        , sPoints = (init sPoints) ++ [xs]
        , sResults = (init sResults) ++ [fs]
      }
    else s{
        sPoints = (head sPoints):(shrink (tail sPoints) (head sPoints))
      }
  where 
    (g,b,u) = sGBU
    xs = contract sBetha sCentral $ fst u
    fs = sFunc xs

--| covariance
cov ::[Point] -> Double
cov x = trace ("ex=" ++ show ex ++ "\t ex2=" ++ show ex2)  ex2 - ex
  where
    ex  = sqp $ centralPoint x 
    ex2 = (sum $ sqp <$> x) / (fromIntegral $ length x)  

validate :: Int -> Simplex -> Simplex
validate 0 s = s
validate n s@(Simplex{..}) = trace ("n=" ++ show n) $ 
  if sqrt (cov sPoints) < sError
    then s
    else validate (n - 1) $ stepSearch s