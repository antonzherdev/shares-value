module Stocks(finParam, stock, stocks) where

import SharesModel
import Rnd
import qualified Data.Map as Map
import Data.Map (Map, (!))

finParam :: FinParam
finParam = FinParam {
      gdpGrowth = 0.028
    , marketPeRatio = 21.7
  }

stocks :: Map String (IO Stock)
stocks = Map.fromList [
      (stockSymbol stableStock, return stableStock)
    , loadStock ("NZSE", "ATM", "A2 Milk") [
         2021 `revGrowth` ((-0.50) `minMax95` (-0.22)) `margin` (0.0 `minMax95` 0.12)
       , 2022 `revGrowth` meanMinMax95 0.20 0.0 0.60   `margin` (0.05 `minMax95` 0.20)
       , 2023 `revGrowth` (  0.10  `minMax95`   0.25)  `margin` (0.10 `minMax95` 0.25)
       , 2024 `revGrowth` (  0.00  `minMax95`   0.25)  `margin` (0.10 `minMax95` 0.25)
       , 2025 `revGrowth` ((-0.10) `minMax95`   0.25)  `margin` (0.10 `minMax95` 0.25)
      ]
    , loadStock ("NZSE", "FWL", "Foley Wines") [
         2021 `revGrowth` ((-0.12) `minMax95` (-0.05)) `margin` (0.03 `minMax95` 0.10)
       , 2022 `revGrowth` (  0.05  `minMax95`   0.15)  `margin` (0.05 `minMax95` 0.14)
       , 2023 `revGrowth` (  0.00  `minMax95`   0.15)  `margin` (0.05 `minMax95` 0.14)
       , 2024 `revGrowth` ((-0.10)  `minMax95`  0.15)  `margin` (0.05 `minMax95` 0.14)
       , 2025 `revGrowth` ((-0.10) `minMax95`   0.15)  `margin` (0.05 `minMax95` 0.14)
      ]
    , loadStock ("NZSE", "FPH", "Fisher & Paykel health") [
        2021 `revGrowth` ((-0.50) `minMax95` (-0.10)) `margin` (0.15 `minMax95` 0.30)
      , 2022 `revGrowth` (  0.05  `minMax95`   0.25)  `margin` (0.15 `minMax95` 0.30)
      , 2023 `revGrowth` (  0.00  `minMax95`   0.25)  `margin` (0.15 `minMax95` 0.30)
      , 2024 `revGrowth` (  0.00  `minMax95`   0.25)  `margin` (0.15 `minMax95` 0.30)
      , 2025 `revGrowth` (  0.00  `minMax95`   0.25)  `margin` (0.15 `minMax95` 0.30)
     ]
    , loadStock ("NZSE", "AIR", "Air New Zealand") [
       2021 `revGrowth` ((-0.10) `minMax95`   0.10) `margin` ((-0.15) `minMax95` (-0.10))
     , 2022 `revGrowth` meanMinMax95 0.70 0.20 0.80  `margin` (0.00 `minMax95` 0.07)
     , 2023 `revGrowth` (  0.10  `minMax95`   0.30)  `margin` (0.04 `minMax95` 0.10)
     , 2024 `revGrowth` ((-0.02) `minMax95`   0.08)  `margin` (0.04 `minMax95` 0.10)
     , 2025 `revGrowth` ((-0.02) `minMax95`   0.08)  `margin` (0.04 `minMax95` 0.10)
    ]
  ]

stock :: String -> IO Stock
stock = (!) stocks


-- stock price should be equal to pe ratio if stock growth with inflation rate
stableStock :: Stock
stableStock = Stock {
      stockMarket = "test", stockSymbol = "stable", stockName = "stable"
    , stockCapMln = earn * marketPeRatio finParam
    , stockShareCountMln = 1000
    , stockCurrentAssets = 0, stockCurrentLiability = 0, stockLongTermLiability = 0
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
    sre = distrN (gdpGrowth finParam) 0.0
    smr = distrN mar 0.0