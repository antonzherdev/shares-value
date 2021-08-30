module Shares (intrinsicValue, calcEarnings, stockSimulation, expectedInterest) where

import Rnd
import SharesModel
import qualified Data.List as List
--import Debug.Trace(traceShowId)

intrinsicValue :: FinParam -> Stock -> [Double] -> Double -> Double
--intrinsicValue p earnings = (es + last earnings/p) / ( (1 + p) ^ years)
intrinsicValue FinParam{gdpGrowth=gdp, marketPeRatio=pe} _ es adj  =
  (en*pe + sum es + adj) / int
  where
    n = length es
    en = last es
    int = (1 + gdp)^n + ((1 + gdp) + (1 + gdp)^n)* fromIntegral n /(2*pe)

expectedInterest :: FinParam -> Double
expectedInterest FinParam{gdpGrowth=gdp, marketPeRatio=pe} = (1 + gdp)*(1 + 1/pe) - 1

calcEarnings :: Double -> [FutureYear] -> Rnd [RevEarn]
calcEarnings revenue0 years = tail <$> scanM f (RevEarn revenue0 0 0) years
  where
    f r i = i r

calcStock :: FinParam -> Stock -> Rnd ([RevEarn], Double)
calcStock param stock@Stock{stockFuture = fut, stockData = d} = do
  es <- calcEarnings (stockRevenue d) (futureYears fut)
  adj <- futureAssetsAdjustment fut
  let ev = stockCurrentAssets d - stockCurrentLiability d + adj + intrinsicValue param stock (map reEarnings es) adj
  return (es, ev)


simulations :: FinParam -> Stock -> [([RevEarn], Double)]
simulations param stock = simulate 10000 $ calcStock param stock


stockSimulation :: Double -> FinParam -> Stock -> ([(MeanMinMax, MeanMinMax, MeanMinMax)], MeanMinMax)
stockSimulation confidence param stock = (map calcMean years, mk $ map snd sims)
  where
    sims = simulations param stock
    years = List.transpose $ map fst sims
    mk = mkMeanMinMax . withConfidence confidence
    calcMean rs = (mk $ map reRevenue rs, mk $ map reEarnings rs, mk $ map reMargin rs)




