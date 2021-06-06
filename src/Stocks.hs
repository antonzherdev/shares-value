module Stocks(finParam, stock, stocks) where

import SharesModel
import Rnd
import Data.Map (Map, (!))
import qualified Data.Map as Map
  
finParam :: FinParam
finParam = FinParam {
      riskFreeRate = 0.021
    , marketPeRatio = 21.7
  }

stock :: String -> Stock
stock = (!) stocks  
 
stocks :: Map String Stock
stocks = Map.fromList $ map (\s -> (stockName s, s)) [
    Stock {
        stockName = "stable"
      , stockCapMln = 1000 * 0.1 * (1 + riskFreeRate finParam) / (1 / marketPeRatio finParam + riskFreeRate finParam)
      , stockShareCountMln = 1000
      , stockCash = 0
      , stockRevenue = 1000
      , stockFuture = [
           2021 `revGrowth` sre `margin` smr
         , 2022 `revGrowth` sre `margin` smr
         , 2025 `revGrowth` sre `margin` smr
         , 2023 `revGrowth` sre `margin` smr
         , 2024 `revGrowth` sre `margin` smr
        ]
    } 
    , Stock {
        stockName = "a2milk"
      , stockCapMln = 4513
      , stockShareCountMln = 743.41
      , stockCash = 774
      , stockRevenue = 1600
      , stockFuture = [
           2021 `revGrowth` ((-0.50) `minMax95` (-0.22)) `margin` (0.0 `minMax95` 0.12)
         , 2022 `revGrowth` meanMinMax95 0.20 0.0 0.60   `margin` (0.05 `minMax95` 0.20)
         , 2023 `revGrowth` (  0.10  `minMax95`   0.25)  `margin` (0.10 `minMax95` 0.25)
         , 2024 `revGrowth` (  0.00  `minMax95`   0.25)  `margin` (0.10 `minMax95` 0.25)
         , 2025 `revGrowth` ((-0.10) `minMax95`   0.25)  `margin` (0.10 `minMax95` 0.25)
        ]
    } 
  ] where 
    sre = N (riskFreeRate finParam) 0.0
    smr = N 0.1 0.0
    
