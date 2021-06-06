module Stocks(finParam, stock, stocks, defaultStock) where

import SharesModel
import Rnd
import Data.Map (Map, (!))
import qualified Data.Map as Map

finParam :: FinParam
finParam = FinParam {
      gdpGrowth = 0.028
    , marketPeRatio = 21.7
  }

stock :: String -> Stock
stock = (!) stocks

defaultStock :: String
defaultStock = "fphealth"

stocks :: Map String Stock
stocks = Map.fromList $ map (\s -> (stockName s, s)) [
      stableStock
    , Stock {
        stockName = "a2milk"
      , stockCapMln = 4513
      , stockShareCountMln = 743.41
      , stockCash = 774
      , stockRevenue = 1600
      , stockEarnings = 320
      , stockFuture = [
           2021 `revGrowth` ((-0.50) `minMax95` (-0.22)) `margin` (0.0 `minMax95` 0.12)
         , 2022 `revGrowth` meanMinMax95 0.20 0.0 0.60   `margin` (0.05 `minMax95` 0.20)
         , 2023 `revGrowth` (  0.10  `minMax95`   0.25)  `margin` (0.10 `minMax95` 0.25)
         , 2024 `revGrowth` (  0.00  `minMax95`   0.25)  `margin` (0.10 `minMax95` 0.25)
         , 2025 `revGrowth` ((-0.10) `minMax95`   0.25)  `margin` (0.10 `minMax95` 0.25)
        ]
    }, Stock {
        stockName = "foley"
      , stockCapMln = 113
      , stockShareCountMln = 65.74
      , stockCash = 113 - 48
      , stockRevenue = 50.23
      , stockEarnings = 6.76
      , stockFuture = [
           2021 `revGrowth` ((-0.12) `minMax95` (-0.05)) `margin` (0.03 `minMax95` 0.10)
         , 2022 `revGrowth` (  0.05  `minMax95`   0.15)  `margin` (0.05 `minMax95` 0.14)
         , 2023 `revGrowth` (  0.00  `minMax95`   0.15)  `margin` (0.05 `minMax95` 0.14)
         , 2024 `revGrowth` ((-0.10)  `minMax95`  0.15)  `margin` (0.05 `minMax95` 0.14)
         , 2025 `revGrowth` ((-0.10) `minMax95`   0.15)  `margin` (0.05 `minMax95` 0.14)
        ]
    }, Stock {
         stockName = "fphealth" {- Fisher & Paykel health -}
       , stockCapMln = 16714
       , stockShareCountMln = 576.34
       , stockCash = 882 - 76
       , stockRevenue = 1971.2
       , stockEarnings = 524.2
       , stockFuture = [
            2021 `revGrowth` ((-0.50) `minMax95` (-0.10)) `margin` (0.15 `minMax95` 0.30)
          , 2022 `revGrowth` (  0.05  `minMax95`   0.25)  `margin` (0.15 `minMax95` 0.30)
          , 2023 `revGrowth` (  0.00  `minMax95`   0.25)  `margin` (0.15 `minMax95` 0.30)
          , 2024 `revGrowth` (  0.00  `minMax95`   0.25)  `margin` (0.15 `minMax95` 0.30)
          , 2025 `revGrowth` (  0.00  `minMax95`   0.25)  `margin` (0.15 `minMax95` 0.30)
         ]
     }
  ]

-- stock price should be equal to pe ratio if stock growth with inflation rate
stableStock :: Stock
stableStock = Stock {
      stockName = "stable"
    , stockCapMln = earn * marketPeRatio finParam
    , stockShareCountMln = 1000
    , stockCash = 0
    , stockRevenue = rev
    , stockEarnings = rev*mar
    , stockFuture = [
         2021 `revGrowth` sre `margin` smr
       , 2022 `revGrowth` sre `margin` smr
       , 2025 `revGrowth` sre `margin` smr
       , 2023 `revGrowth` sre `margin` smr
       , 2024 `revGrowth` sre `margin` smr
      ]
  } where
    mar = 0.1
    rev = 1000
    earn = rev * mar
    sre = N (gdpGrowth finParam) 0.0
    smr = N mar 0.0