module Rnd (Distr, Rnd, minMax95, minMax99, meanMinMax95, meanMinMax99,
  (|*|), (|+|), runRnd, simulate, mkN95, distrN, distrAsymN,
  simulateN, simulateMmm, MeanMinMax(..), scanM, withConfidence, mkMeanMinMax, showD, mmmProbability95AtLeast) where

import Statistics.Distribution.Normal
import Statistics.Distribution
import Data.Random.Normal
import Control.Monad.State
import System.Random
import qualified Data.List as List
import Numeric

type Rnd = State StdGen
type Distr = Rnd Double

distrN :: Double -> Double -> Distr
distrN m std = (+) m . (*) std <$> state normal

distrAsymN :: Double -> Double -> Double -> Distr
distrAsymN m lowStd highStd = (+) m . mulStd <$> state normal
  where
    mulStd :: Double -> Double
    mulStd v
      | v <= 0 = v*lowStd
      | otherwise = v*highStd

data MeanMinMax = MeanMinMax {
  mmmMean :: Double,
  mmmMin :: Double,
  mmmMax :: Double
}

instance Show MeanMinMax where
    show (MeanMinMax m pMin pMax) = showD 2 m ++ " (" ++ showD 2 pMin ++ " .. " ++ showD 2 pMax ++ ")"

instance Num MeanMinMax where
  (MeanMinMax a c e) + (MeanMinMax b d f) = MeanMinMax (a + b) (c + d) (e + f)
  (MeanMinMax a c e) - (MeanMinMax b d f) = MeanMinMax (a - b) (c - d) (e - f)
  (MeanMinMax a c e) * (MeanMinMax b d f) = MeanMinMax (a * b) (c * d) (e * f)
  negate (MeanMinMax a b c) = MeanMinMax (-a) (-b) (-c)
  abs (MeanMinMax a b c) = MeanMinMax (abs a) (abs b) (abs c)
  signum (MeanMinMax a b c) = MeanMinMax (signum a) (signum b) (signum c)
  fromInteger x =
    let d = fromIntegral x
    in MeanMinMax d d d

instance Fractional MeanMinMax where
  (MeanMinMax a c e) / (MeanMinMax b d f) = MeanMinMax (a / b) (c / d) (e / f)
  fromRational x = 
    let d = fromRational x 
    in MeanMinMax d d d  

mmmProbability95AtLeast :: MeanMinMax -> Double -> Double
mmmProbability95AtLeast (MeanMinMax m a z) x
  | x == m = 0.5
  | x < m = complCumulative (normalDistr m ((m - a)/2)) x
  | otherwise = complCumulative (normalDistr m ((z - m)/2)) x

(|*|) :: MeanMinMax -> Double -> MeanMinMax
(MeanMinMax m pMin pMax) |*| v = MeanMinMax (m*v) (pMin*v) (pMax*v)

(|+|) :: MeanMinMax -> Double -> MeanMinMax
(MeanMinMax m pMin pMax) |+| v = MeanMinMax (m+v) (pMin+v) (pMax+v)

minMax95 :: Double -> Double -> Distr
minMax95 pMin pMax = distrN m std where
  m = (pMax + pMin)/2
  std = (pMax - pMin)/4

minMax99 :: Double -> Double -> Distr
minMax99 pMin pMax = distrN m std where
  m = (pMax + pMin)/2
  std = (pMax - pMin)/6

meanMinMax95N :: MeanMinMax -> Distr
meanMinMax95N (MeanMinMax m pMin pMax) = distrAsymN m lowStd highStd
  where
    lowStd = (m - pMin)/2
    highStd = (pMax - m)/2

meanMinMax95 :: Double -> Double -> Double -> Distr
meanMinMax95 m pMin pMax = meanMinMax95N $ MeanMinMax m pMin pMax

meanMinMax99N :: MeanMinMax -> Distr
meanMinMax99N (MeanMinMax m pMin pMax) = distrN mean1 std where
  mean1 = (pMax + 4*m + pMin)/6
  std = (pMax - pMin)/6

meanMinMax99 :: Double -> Double -> Double -> Distr
meanMinMax99 m pMin pMax = meanMinMax99N $ MeanMinMax m pMin pMax

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