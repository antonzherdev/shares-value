module Rnd (Distr, Rnd, minMax95, minMax99, meanMinMax95, meanMinMax99,
  (|*|), runRnd, simulate, mkN95, distrN, distrAsymN,
  simulateN, simulateMmm, MeanMinMax, scanM, withConfidence, mkMeanMinMax, showD) where


import Data.Random.Normal
import Control.Monad.State
import System.Random
import qualified Data.List as List
import Numeric

type Rnd = State StdGen
type Distr = Rnd Double

distrN :: Double -> Double -> Distr
distrN mean std = (+) mean . (*) std <$> state normal

distrAsymN :: Double -> Double -> Double -> Distr
distrAsymN mean lowStd highStd = (+) mean . mulStd <$> state normal
  where 
    mulStd :: Double -> Double
    mulStd v
      | v <= 0 = v*lowStd
      | otherwise = v*highStd

data MeanMinMax = MeanMinMax Double Double Double

instance Show MeanMinMax where
    show (MeanMinMax mean pMin pMax) = showD 2 mean ++ " (" ++ showD 2 pMin ++ " .. " ++ showD 2 pMax ++ ")"

(|*|) :: MeanMinMax -> Double -> MeanMinMax
(MeanMinMax mean pMin pMax) |*| v = MeanMinMax (mean*v) (pMin*v) (pMax*v)

minMax95 :: Double -> Double -> Distr
minMax95 pMin pMax = distrN mean std where
  mean = (pMax + pMin)/2
  std = (pMax - pMin)/4

minMax99 :: Double -> Double -> Distr
minMax99 pMin pMax = distrN mean std where
  mean = (pMax + pMin)/2
  std = (pMax - pMin)/6

meanMinMax95N :: MeanMinMax -> Distr
meanMinMax95N (MeanMinMax mean pMin pMax) = distrAsymN mean lowStd highStd
  where
    lowStd = (mean - pMin)/2
    highStd = (pMax - mean)/2

meanMinMax95 :: Double -> Double -> Double -> Distr
meanMinMax95 mean pMin pMax = meanMinMax95N $ MeanMinMax mean pMin pMax

meanMinMax99N :: MeanMinMax -> Distr
meanMinMax99N (MeanMinMax mean pMin pMax) = distrN mean1 std where
  mean1 = (pMax + 4*mean + pMin)/6
  std = (pMax - pMin)/6

meanMinMax99 :: Double -> Double -> Double -> Distr
meanMinMax99 mean pMin pMax = meanMinMax99N $ MeanMinMax mean pMin pMax

mkN95 :: [Double] -> Distr
mkN95 [] = distrN 0 1
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

simulateN :: Int -> Rnd Double -> Distr
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