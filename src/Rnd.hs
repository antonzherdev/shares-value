module Rnd (Distribution(random), N, normalize, denormalize, Rnd, (..<), (<..>), (|*|), runRnd, simulate, minMeanMaxN, mkN,
  simulateN, simulateMmm, MeanMinMax, scanM, withConfidence, mkMeanMinMax) where


import Data.Random.Normal
import Control.Monad.State
import System.Random
import Data.List
import Numeric

type Rnd = State StdGen

class Distribution a where
  random :: a -> Rnd Double

data N = N Double Double deriving (Show)

data MeanMinMax = MeanMinMax Double Double Double

instance Show MeanMinMax where
    show (MeanMinMax mean min max) = showD 2 mean ++ " (" ++ showD 2 min ++ " .. " ++ showD 2 max ++ ")"

instance Distribution N where
   random n = fmap (denormalize n) $ state normal

(MeanMinMax mean min max) |*| v = MeanMinMax (mean*v) (min*v) (max*v)

normalize :: N -> Double -> Double
normalize (N mean std) v = (v - mean)/std

denormalize :: N -> Double -> Double
denormalize (N mean std) v = v*std + mean

(..<) :: Double -> Double -> N
min ..< max = N mean std where
  mean = (max + min)/2
  std = (max - min)/4

minMeanMaxN :: MeanMinMax -> N
minMeanMaxN (MeanMinMax mean min max) = N mean1 std where
  mean1 = (max + 4*mean + min)/6
  std = (max - min)/4

(<..>) :: Double -> (Double, Double) -> N
mean <..> (min, max) = minMeanMaxN $ MeanMinMax mean min max

mkN :: [Double] -> N
mkN [] = N 0 1
mkN xs = minMeanMaxN $ mkMeanMinMax $ withConfidence 0.997 xs

withConfidence :: Double -> [Double] -> [Double]
withConfidence confidence xs = if d == 0 then xs else take (len - 2*d) $ drop d $ sort xs
  where
    len = length xs
    d = floor $ (1 - confidence)/2 * (fromIntegral len)

mkMeanMinMax :: [Double] -> MeanMinMax
mkMeanMinMax xs = MeanMinMax (sum xs / fromIntegral (length xs) ) (minimum xs) (maximum xs)

simulate :: Int -> Rnd a -> [a]
simulate 0 r = []
simulate times r = (runRnd times r) : (simulate (times - 1) r )

simulateN :: Int -> Rnd Double -> N
simulateN times r = mkN $ simulate times r

simulateMmm :: Int -> Double -> Rnd Double -> MeanMinMax
simulateMmm times confidence r = mkMeanMinMax $ withConfidence confidence $ simulate times r

runRnd :: Int -> Rnd a -> a
runRnd seed r = evalState r (mkStdGen seed)


scanM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m [a]
scanM f q [] = return [q]
scanM f q (x:xs) =
  do
    q2 <- f q x
    qs <- scanM f q2 xs
    return (q:qs)

showD numOfDecimals floatNum = showFFloat (Just numOfDecimals) floatNum ""