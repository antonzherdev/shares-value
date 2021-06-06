module Shares (intrinsicValue, calcEarnings, stockSimulation) where

import Rnd
import SharesModel
import qualified Data.List as List

intrinsicValue :: Double -> [Double] -> Double
intrinsicValue p earnings = (es + last earnings/p) / ( (1 + p) ^ years)
  where
    es = sum earnings
    years = length earnings


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
calcStock FinParam{riskFreeRate = g, marketPeRatio = peRatio} Stock{stockCash = cash, stockFuture = fut, stockRevenue = revenue} = do
  es <- calcEarnings revenue fut
  let ev = cash + intrinsicValue (1/peRatio + g) (map reEarnings es)
  return (es, ev)


simulations :: FinParam -> Stock -> [([RevEarn], Double)]
simulations param stock = simulate 10000 $ calcStock param stock


stockSimulation :: Double -> FinParam -> Stock -> ([(MeanMinMax, MeanMinMax)], MeanMinMax)
stockSimulation confidence param stock = (map calcMean years, mk $ map snd sims)
  where
    sims = simulations param stock
    years = List.transpose $ map fst sims
    mk = mkMeanMinMax . withConfidence confidence
    calcMean rs = (mk $ map reRevenue rs, mk $ map reEarnings rs)




