module Shares (intrinsicValue, test, calcEarnings, RevEarn(RevEarn), earnings, revenue) where

import Rnd
import Data.Foldable


intrinsicValue :: Double -> [Double] -> Double
intrinsicValue p earnings = es / ( (1 + p) ^ years - 1)
  where
    es = sum earnings
    years = length earnings
    

data RevEarn = RevEarn Double Double

revenue (RevEarn r _) = r
earnings (RevEarn _ e) = e

revEarn :: Double -> (N, N) -> Rnd RevEarn
revEarn revenue0 (grow, margin) =
  do
    g <- random grow
    m <- random margin
    let revenue = revenue0 * (1 + g)
    return $ RevEarn revenue (revenue*m)

calcEarnings :: Double -> [(N, N)] -> (Int, Rnd [RevEarn])
calcEarnings revenue0 years = (length years, fmap tail $ scanM f (RevEarn revenue0 0) years)
  where
    f (RevEarn r _) i = revEarn r i

test = (1 ..< 4, simulateMmm 100000 0.682 $ random (1 ..< 4))