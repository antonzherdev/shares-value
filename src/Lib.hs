module Lib
    ( someFunc
    ) where

import SharesModel
import Shares
import Rnd
import Data.Foldable

--riskFreeRate = 0.021

finParam :: FinParam
finParam = FinParam {
      riskFreeRate = 0.021
    , marketPeRatio = 21.7
  }

a2milk :: Stock
a2milk = Stock {
      stockCapMln = 4513
    , stockShareCountMln = 743.41
    , stockCash = 774
    , stockRevenue = 1600
    , stockFuture = [
       {-2021-}   StockYear ((-0.5) ..< (-0.22)) (0.0 ..< 0.12)
       {-2022-} , StockYear (0.20 <..> (0.0, 0.60)) (0.05 ..< 0.20)
       {-2023-} , StockYear (0.10 ..< 0.25) (0.10 ..< 0.25)
       {-2024-} , StockYear (0.00 ..< 0.25) (0.10 ..< 0.25)
       {-2025-} , StockYear ((-0.10) ..< 0.20) (0.10 ..< 0.25)
      ]
  }

someFunc :: IO ()
someFunc = procStock a2milk

procStock :: Stock -> IO ()
procStock stock@Stock{stockShareCountMln = sharesCountMln, stockCapMln = capMln} =  do
  putStrLn $ "Share price    = " ++ show (snd sm |*| (1/sharesCountMln))
  putStrLn $ "Current price  = " ++ show (capMln/sharesCountMln)
  putStrLn $ "Capitalisation = " ++ show (snd sm)
  _ <- foldlM printYear 2021 (fst sm)
  return ()
--  putStrLn $ simulateMmm 10000 0.95 $ intrinsicValue 0.021 $ earnings
  where
    sm = stockSimulation 0.95 finParam stock
    printYear :: Int -> (MeanMinMax, MeanMinMax) -> IO Int
    printYear year (rev, earn) =
      do
        putStrLn $ "# " ++ show year
        putStrLn $ "Revenue  = " ++ show rev
        putStrLn $ "Earnings = " ++ show earn
--        putStrLn $ "Margin   = " ++ show (rev
        return $ year + 1

--someFunc = putStrLn $ simulateMmm 10000 0.95 $ intrinsicValue 0.021 $ (10, random (2000 ..< 3000))

--someFunc = print $ intrinsicValue 0.021 10 2706





