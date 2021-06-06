module Lib
    ( someFunc
    ) where

import Shares
import Stocks
import SharesModel
import Rnd
import Data.Foldable

--riskFreeRate = 0.021


someFunc :: IO ()
someFunc = procStock $ stock "foley"
--someFunc = print $ expectedInterest finParam

procStock :: Stock -> IO ()
procStock s@Stock{stockShareCountMln = sharesCountMln, stockCapMln = capMln} =  do
  putStrLn $ "Market interest = " ++ showD 4 (100*expectedInterest finParam) ++ "%"
  putStrLn "---------------------------------------------------------"
  _ <- foldlM printYear 2021 (fst sm)
  putStrLn "---------------------------------------------------------"
  putStrLn $ "Share price    = " ++ show (snd sm |*| (1/sharesCountMln))
  putStrLn $ "Current price  = " ++ showD 2 (capMln/sharesCountMln)
  putStrLn $ "Capitalisation = " ++ show (snd sm)
  putStrLn $ "Current cap    = " ++ showD 2 capMln
  
  return ()
--  putStrLn $ simulateMmm 10000 0.95 $ intrinsicValue 0.021 $ earnings
  where
    sm = stockSimulation 0.95 finParam s
    printYear :: Int -> (MeanMinMax, MeanMinMax) -> IO Int
    printYear y (rev, earn) =
      do
        putStrLn $ "# " ++ show y
        putStrLn $ "Revenue  = " ++ show rev
        putStrLn $ "Earnings = " ++ show earn
--        putStrLn $ "Margin   = " ++ show (rev
        return $ y + 1

--someFunc = putStrLn $ simulateMmm 10000 0.95 $ intrinsicValue 0.021 $ (10, random (2000 ..< 3000))

--someFunc = print $ intrinsicValue 0.021 10 2706





