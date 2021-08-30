module Lib
    ( someFunc
    ) where

import Shares
import Stocks
import SharesModel
import Rnd
import Data.Foldable
import System.Environment
import Data.Map ((!))

--riskFreeRate = 0.021


someFunc :: IO ()
someFunc = getArgs >>= run

run :: [String] -> IO ()
run ["--refresh", token, symbol] = updateStockCsv token $ StockId "NZSE" symbol
run ["--refresh", token, market, symbol] = updateStockCsv token $ StockId market symbol
run ["--refresh-all", token] = forM_ allStockIds (updateStockCsv token)  
run [symbol] = stock (StockId "NZSE" symbol) >>= procStock
run [market, symbol] = stock (StockId market symbol) >>= procStock
run o = error $ "invalid params " ++ show o


procStock :: Stock -> IO ()
procStock s =  do
  putStrLn "# Now"
  putStrLn $ "Revenue  = " ++ unwords (showD 2 . pastYearRevenue <$> stockPast d)
  putStrLn $ "Growth   = " ++ show (pastRevenueGrowth d |*| 100) ++ " [" ++ unwords (showD 2 . (100 *) <$> pastRevenueGrowths d) ++ "]"
  putStrLn $ "Earnings = " ++ unwords (showD 2 . pastYearEarnings <$> stockPast d)
  putStrLn $ "Margin   = " ++ show (pastMargin d |*| 100) ++ " [" ++ unwords (showD 2 . (100 *) . pastYearMargin  <$> stockPast d) ++ "]"
  _ <- foldlM printYear (futureStartYear (stockFuture s)) $ zip prevRevs yss
  putStrLn "---------------------------------------------------------"
  putStrLn $ stockIdSymbol sId ++ ": " ++ stockName d
  putStrLn $ "Intrinsic cap  [$M] = " ++ show (snd sm)
  putStrLn $ "Current debt   [$M] = " ++ show (stockDebt d)
  putStrLn $ "Current equity [$M] = " ++ show (stockEquity d)
  putStrLn $ "Equity cap     [$M] = " ++ show debtCap
  putStrLn $ "Current cap    [$M] = " ++ showD 2 capMln
  putStrLn $ "Intrinsic price [$] = " ++ show (debtCap |*| (1/sharesCountMln))
  putStrLn $ "Current price   [$] = " ++ showD 2 (capMln/sharesCountMln)
  putStrLn $ "Expected return [%] = " ++ show (((undervalue |*| 0.2) |+| targetInterest) |*| 100) ++ " | " ++ showD 2 (targetInterest*100) 
  putStrLn $ "Undervalued     [%] = " ++ show (undervalue |*| 100)
  putStrLn $ "Undervalue prob [%] = " ++ showD 2 (mmmProbability95AtLeast debtCap capMln * 100)
  
  return ()
--  putStrLn $ simulateMmm 10000 0.95 $ intrinsicValue 0.021 $ earnings
  where
    sId = stockId d
    d = stockData s
    sharesCountMln = stockShareCountMln d
    capMln = stockCapMln d
    finParam = finParams ! stockIdMarket sId
    targetInterest = expectedInterest finParam
    undervalue = (debtCap |+| (-capMln)) / debtCap
    equityPercent = stockEquity d / (stockEquity d + stockDebt d)
    debtCap = snd sm |*| equityPercent
    sm = stockSimulation 0.95 finParam s
    yss :: [(MeanMinMax, MeanMinMax, MeanMinMax)]
    yss = fst sm
    ppRs = (\(r, _, _ ) -> r) <$> yss
    prevRevs = mkMeanMinMax [stockRevenue d] : ppRs
    printYear :: Int -> (MeanMinMax, (MeanMinMax, MeanMinMax, MeanMinMax)) -> IO Int
    printYear y (pr, (rev, earn, mrg)) =
      do
        putStrLn $ "# " ++ show y
        putStrLn $ "Revenue  = " ++ show rev
        let growth = mkMeanMinMax [mmmMean rev/mmmMean pr, mmmMin rev/mmmMin pr, mmmMax rev/mmmMax pr]
        putStrLn $ "Growth   = " ++ show ((growth |+| (-1)) |*| 100)
        putStrLn $ "Earnings = " ++ show earn
        putStrLn $ "Margin   = " ++ show (mrg |*| 100)
        return $ y + 1

--someFunc = putStrLn $ simulateMmm 10000 0.95 $ intrinsicValue 0.021 $ (10, random (2000 ..< 3000))

--someFunc = print $ intrinsicValue 0.021 10 2706





