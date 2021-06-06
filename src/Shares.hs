module Shares (intrinsicValue, calcEarnings, stockSimulation, expectedInterest) where

import Rnd
import SharesModel
import qualified Data.List as List
--import Debug.Trace(traceShowId)

intrinsicValue :: FinParam -> Stock -> [Double] -> Double
--intrinsicValue p earnings = (es + last earnings/p) / ( (1 + p) ^ years)
intrinsicValue FinParam{gdpGrowth=gg, marketPeRatio=pe} Stock{stockEarnings = e0} es =
  (e0 + earningsGrowthOverInflation) * pe -- +  / ( (1 + 1/pe) ^ years)
  where
    years = length es
    cpiEarnings = ((e0 * (1 + gg) + e0 * (1 + gg) ^ years)/2) * fromIntegral years
    earningsGrowthOverInflation = sum es - cpiEarnings

expectedInterest :: FinParam -> Double
expectedInterest FinParam{gdpGrowth=gg, marketPeRatio=pe} = gg + (1 + gg)/pe

revEarn :: Double -> StockYear -> Rnd RevEarn
revEarn revenue0 (StockYear _ grow mrg) =
  do
    g <- random grow
    m <- random mrg
    let revenue = revenue0 * (1 + g)
    return $ RevEarn revenue (revenue*m)

calcEarnings :: Double -> [StockYear] -> Rnd [RevEarn]
calcEarnings revenue0 years = tail <$> scanM f (RevEarn revenue0 0) years
  where
    f (RevEarn r _) i = revEarn r i

calcStock :: FinParam -> Stock -> Rnd ([RevEarn], Double)
calcStock param stock@Stock{stockCash = cash, stockFuture = fut, stockRevenue = revenue} = do
  es <- calcEarnings revenue fut
  let ev = cash + intrinsicValue param stock (map reEarnings es)
  return (es, ev)


simulations :: FinParam -> Stock -> [([RevEarn], Double)]
simulations param stock = simulate 1 $ calcStock param stock


stockSimulation :: Double -> FinParam -> Stock -> ([(MeanMinMax, MeanMinMax)], MeanMinMax)
stockSimulation confidence param stock = (map calcMean years, mk $ map snd sims)
  where
    sims = simulations param stock
    years = List.transpose $ map fst sims
    mk = mkMeanMinMax . withConfidence confidence
    calcMean rs = (mk $ map reRevenue rs, mk $ map reEarnings rs)




