module Lib
    ( someFunc
    ) where

import Shares
import Stocks
import SharesModel
import Rnd
import Data.Foldable
import System.Environment

--riskFreeRate = 0.021


someFunc :: IO ()
someFunc = do
  args <- getArgs
  s <- stock $ head args 
  procStock s


procStock :: Stock -> IO ()
procStock s@Stock{stockShareCountMln = sharesCountMln, stockCapMln = capMln} =  do
  putStrLn $ "Market interest = " ++ showD 4 (100*expectedInterest finParam) ++ "%"
  putStrLn "---------------------------------------------------------"
  putStrLn "# Now"
  putStrLn $ "Revenue  = " ++ showD 2 (stockRevenue s)
  putStrLn $ "Earnings = " ++ showD 2 (stockEarnings s)
  putStrLn $ "Margin   = " ++ showD 2 (stockEarnings s / stockRevenue s * 100)
  _ <- foldlM printYear 2021 (fst sm)
  putStrLn "---------------------------------------------------------"
  putStrLn $ stockSymbol s ++ ": " ++ stockName s 
  putStrLn $ "Intrinsic cap  [$M] = " ++ show (snd sm)
  putStrLn $ "Current debt   [$M] = " ++ show (stockDebt s)
  putStrLn $ "Current equity [$M] = " ++ show (stockEquity s)
  putStrLn $ "Equity cap     [$M] = " ++ show debtCap
  putStrLn $ "Current cap    [$M] = " ++ showD 2 capMln
  putStrLn $ "Intrinsic price [$] = " ++ show (debtCap |*| (1/sharesCountMln))
  putStrLn $ "Current price   [$] = " ++ showD 2 (capMln/sharesCountMln)
  
  return ()
--  putStrLn $ simulateMmm 10000 0.95 $ intrinsicValue 0.021 $ earnings
  where
    equityPercent = stockEquity s / (stockEquity s + stockDebt s)
    debtCap = snd sm |*| equityPercent
    sm = stockSimulation 0.95 finParam s
    printYear :: Int -> (MeanMinMax, MeanMinMax, MeanMinMax) -> IO Int
    printYear y (rev, earn, mrg) =
      do
        putStrLn $ "# " ++ show y
        putStrLn $ "Revenue  = " ++ show rev
        putStrLn $ "Earnings = " ++ show earn
        putStrLn $ "Margin   = " ++ show (mrg |*| 100)
        return $ y + 1

--someFunc = putStrLn $ simulateMmm 10000 0.95 $ intrinsicValue 0.021 $ (10, random (2000 ..< 3000))

--someFunc = print $ intrinsicValue 0.021 10 2706





