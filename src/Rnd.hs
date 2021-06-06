module Rnd (Distribution(random), N, normalize, denormalize, Rnd, minMax95, minMax99, meanMinMax95, meanMinMax99,
  (|*|), runRnd, simulate, mkN95,
  simulateN, simulateMmm, MeanMinMax, scanM, withConfidence, mkMeanMinMax) where


import Data.Random.Normal
import Control.Monad.State
import System.Random
import qualified Data.List as List
import Numeric

type Rnd = State StdGen

class Distribution a where
  random :: a -> Rnd Double

data N = N Double Double deriving (Show)

data MeanMinMax = MeanMinMax Double Double Double

instance Show MeanMinMax where
    show (MeanMinMax mean pMin pMax) = showD 2 mean ++ " (" ++ showD 2 pMin ++ " .. " ++ showD 2 pMax ++ ")"

instance Distribution N where
   random n = denormalize n <$> state normal

(|*|) :: MeanMinMax -> Double -> MeanMinMax
(MeanMinMax mean pMin pMax) |*| v = MeanMinMax (mean*v) (pMin*v) (pMax*v)

normalize :: N -> Double -> Double
normalize (N mean std) v = (v - mean)/std

denormalize :: N -> Double -> Double
denormalize (N mean std) v = v*std + mean

minMax95 :: Double -> Double -> N
minMax95 pMin pMax = N mean std where
  mean = (pMax + pMin)/2
  std = (pMax - pMin)/4

minMax99 :: Double -> Double -> N
minMax99 pMin pMax = N mean std where
  mean = (pMax + pMin)/2
  std = (pMax - pMin)/6

meanMinMax95N :: MeanMinMax -> N
meanMinMax95N (MeanMinMax mean pMin pMax) = N mean1 std where
  mean1 = (pMax + 4*mean + pMin)/6
  std = (pMax - pMin)/4

meanMinMax95 :: Double -> Double -> Double -> N
meanMinMax95 mean pMin pMax = meanMinMax95N $ MeanMinMax mean pMin pMax

meanMinMax99N :: MeanMinMax -> N
meanMinMax99N (MeanMinMax mean pMin pMax) = N mean1 std where
  mean1 = (pMax + 4*mean + pMin)/6
  std = (pMax - pMin)/6

meanMinMax99 :: Double -> Double -> Double -> N
meanMinMax99 mean pMin pMax = meanMinMax99N $ MeanMinMax mean pMin pMax

mkN95 :: [Double] -> N
mkN95 [] = N 0 1
mkN95 xs = meanMinMax95N $ mkMeanMinMax $ withConfidence 0.997 xs

withConfidence :: Double -> [Double] -> [Double]
withConfidence confidence xs = if d == 0 then xs else take (len - 2*d) $ drop d $ List.sort xs
  where
    len = length xs
    d = floor $ (1 - confidence)/2 * fromIntegral len

mkMeanMinMax :: [Double] -> MeanMinMax
mkMeanMinMax xs = MeanMinMax (sum xs / fromIntegral (length xs) ) (minimum xs) (maximum xs)

simulate :: Int -> Rnd a -> [a]
simulate 0 _ = []
simulate times r = runRnd times r : simulate (times - 1) r

simulateN :: Int -> Rnd Double -> N
simulateN times r = mkN95 $ simulate times r

simulateMmm :: Int -> Double -> Rnd Double -> MeanMinMax
simulateMmm times confidence r = mkMeanMinMax $ withConfidence confidence $ simulate times r

runRnd :: Int -> Rnd a -> a
runRnd seed r = evalState r (mkStdGen seed)


scanM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m [a]
scanM _ q [] = return [q]
scanM f q (x:xs) =
  do
    q2 <- f q x
    qs <- scanM f q2 xs
    return (q:qs)

showD :: RealFloat a => Int -> a -> String
showD numOfDecimals floatNum = showFFloat (Just numOfDecimals) floatNum ""