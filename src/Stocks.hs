module Stocks(finParams, stock, stocks, allStockIds) where

import SharesModel
import Rnd
import qualified Data.Map as Map
import Data.Map (Map, (!))

finParams :: Map String FinParam
finParams = Map.fromList $ (\p -> (paramMarket p, p)) <$> [
    FinParam {
        paramMarket = "NZSE"
      , gdpGrowth = 0.028
      , marketPeRatio = 21.7
    },
    FinParam {
        paramMarket = "ASX"
      , gdpGrowth = 0.022
      , marketPeRatio = 20.9
    },
    FinParam {
        paramMarket = "test"
      , gdpGrowth = 0.028
      , marketPeRatio = 21.7
    }
  ]

stocks :: Map StockId (IO Stock)
stocks = Map.fromList [
      (stockId (stockData stableStock), return stableStock)
    , makeStock ("NZSE", "ATM", "A2 Milk") $ constFuture [
         2021 `revGrowth` ((-0.50) `minMax95` (-0.22)) `margin` (0.0 `minMax95` 0.12)
       , 2022 `revGrowth` meanMinMax95 0.20 0.0 0.60   `margin` (0.05 `minMax95` 0.20)
       , 2023 `revGrowth` (  0.10  `minMax95`   0.25)  `margin` (0.10 `minMax95` 0.25)
       , 2024 `revGrowth` (  0.00  `minMax95`   0.25)  `margin` (0.10 `minMax95` 0.25)
       , 2025 `revGrowth` ((-0.10) `minMax95`   0.25)  `margin` (0.10 `minMax95` 0.25)
      ]
    , makeStock ("NZSE", "FWL", "Foley Wines") $ constFuture [
         2021 `revGrowth` ((-0.10) `minMax95`   0.05)  `margin` (0.07 `minMax95` 0.15)
       , 2022 `revGrowth` (  0.05  `minMax95`   0.15)  `margin` (0.05 `minMax95` 0.15)
       , 2023 `revGrowth` (  0.00  `minMax95`   0.15)  `margin` (0.05 `minMax95` 0.15)
       , 2024 `revGrowth` ((-0.10)  `minMax95`  0.15)  `margin` (0.05 `minMax95` 0.15)
       , 2025 `revGrowth` ((-0.10) `minMax95`   0.15)  `margin` (0.05 `minMax95` 0.15)
      ]
    , makeStock ("ASX", "TWE", "Treasury Wine Estates") $ constFuture [
       2021 `revGrowth` ((-0.10) `minMax95`   0.05)  `margin` (0.07 `minMax95` 0.15)
     , 2022 `revGrowth` (  0.05  `minMax95`   0.15)  `margin` (0.05 `minMax95` 0.15)
     , 2023 `revGrowth` (  0.00  `minMax95`   0.15)  `margin` (0.05 `minMax95` 0.15)
     , 2024 `revGrowth` ((-0.10)  `minMax95`  0.15)  `margin` (0.05 `minMax95` 0.15)
     , 2025 `revGrowth` ((-0.10) `minMax95`   0.15)  `margin` (0.05 `minMax95` 0.15)
    ]
    , makeStock ("NZSE", "FPH", "Fisher & Paykel health") $ constFuture [
        2021 `revGrowth` ((-0.50) `minMax95` (-0.10)) `margin` (0.15 `minMax95` 0.30)
      , 2022 `revGrowth` (  0.05  `minMax95`   0.25)  `margin` (0.15 `minMax95` 0.30)
      , 2023 `revGrowth` (  0.00  `minMax95`   0.25)  `margin` (0.15 `minMax95` 0.30)
      , 2024 `revGrowth` (  0.00  `minMax95`   0.25)  `margin` (0.15 `minMax95` 0.30)
      , 2025 `revGrowth` (  0.00  `minMax95`   0.25)  `margin` (0.15 `minMax95` 0.30)
     ]
    , makeStock ("NZSE", "AIR", "Air New Zealand") $ constFuture [
       2021 `revGrowth` ((-0.10) `minMax95`   0.10) `margin` ((-0.15) `minMax95` (-0.10))
     , 2022 `revGrowth` meanMinMax95 0.70 0.20 0.80  `margin` (0.00 `minMax95` 0.07)
     , 2023 `revGrowth` (  0.10  `minMax95`   0.30)  `margin` (0.04 `minMax95` 0.10)
     , 2024 `revGrowth` ((-0.02) `minMax95`   0.08)  `margin` (0.04 `minMax95` 0.10)
     , 2025 `revGrowth` ((-0.02) `minMax95`   0.08)  `margin` (0.04 `minMax95` 0.10)
    ]
    , makeStock ("NZSE", "KMD", "Kathmandu") $ constFuture [
        2021 `revGrowth` (0.07    `minMax95`   0.15) `margin`  (0.03 `minMax95` 0.07)
      , 2022 `revGrowth` (0.05    `minMax95`   0.17)  `margin` (0.03 `minMax95` 0.10)
      , 2023 `revGrowth` (0.00    `minMax95`   0.20)  `margin` (0.03 `minMax95` 0.12)
      , 2024 `revGrowth` (0.00    `minMax95`   0.20)  `margin` (0.03 `minMax95` 0.12)
      , 2025 `revGrowth` (0.00    `minMax95`   0.20)  `margin` (0.03 `minMax95` 0.12)
    ]
    , makeStock ("ASX", "HVN", "Harvey Norman") $ projFuture [2021 .. 2025]
    , makeStock ("NZSE", "HLG", "Hallenstein Glasson Holdings") $ constFuture [
        2021 `revGrowth` (0.00    `minMax95`   0.17)  `margin` meanMinMax95 0.095 0.06 0.11
      , 2022 `revGrowth` (0.00    `minMax95`   0.17)  `margin` meanMinMax95 0.095 0.06 0.11
      , 2023 `revGrowth` (0.00    `minMax95`   0.17)  `margin` meanMinMax95 0.095 0.06 0.11
      , 2024 `revGrowth` (0.00    `minMax95`   0.17)  `margin` meanMinMax95 0.095 0.06 0.11
      , 2025 `revGrowth` (0.00    `minMax95`   0.17)  `margin` meanMinMax95 0.095 0.06 0.11
    ]
    , makeStock ("NZSE", "MFT", "Mainfreight") $ projFuture [2021 .. 2025]
    , makeStock ("NZSE", "SPK", "Spark") $ projFuture [2021 .. 2025]
    , makeStock ("NZSE", "FBU", "Fletcher Buildings") $ projFuture [2021 .. 2025]
    , makeStock ("NZSE", "MEL", "Meredian Energy") $ constFuture [
       2021 `revGrowth` ((-0.20) `minMax95` 0.22) `margin`  (0.03 `minMax95` 0.12)
     , 2022 `revGrowth` ((-0.20) `minMax95` 0.22) `margin`  (0.03 `minMax95` 0.12)
     , 2023 `revGrowth` ((-0.20) `minMax95` 0.22) `margin`  (0.03 `minMax95` 0.12)
     , 2024 `revGrowth` ((-0.33) `minMax95` 0.09) `margin`  (0.03 `minMax95` 0.12)
     , 2025 `revGrowth` ((-0.20) `minMax95` 0.22) `margin`  (0.03 `minMax95` 0.12)
    ]
    , makeStock ("ASX", "IAG", "Insurance Australia Group") $ constFuture [
       2021 `revGrowth` ((-0.20) `minMax95` 0.07) `margin`  ((-0.05) `minMax95` 0.0)
     , 2022 `revGrowth` ((-0.20) `minMax95` 0.07) `margin`  (0.06 `minMax95` 0.125)
     , 2023 `revGrowth` ((-0.20) `minMax95` 0.07) `margin`  (0.06 `minMax95` 0.125)
     , 2024 `revGrowth` ((-0.33) `minMax95` 0.07) `margin`  (0.06 `minMax95` 0.125)
     , 2025 `revGrowth` ((-0.20) `minMax95` 0.07) `margin`  (0.06 `minMax95` 0.125)
    ]
  ]

allStockIds :: [StockId]
allStockIds = filter (\s -> stockIdMarket s /= "test") $ Map.keys stocks

stock :: StockId -> IO Stock
stock = (!) stocks


-- stock price should be equal to pe ratio if stock growth with inflation rate
stableStock :: Stock
stableStock = Stock d [
       2021 `revGrowth` sre `margin` smr
     , 2022 `revGrowth` sre `margin` smr
     , 2025 `revGrowth` sre `margin` smr
     , 2023 `revGrowth` sre `margin` smr
     , 2024 `revGrowth` sre `margin` smr
    ]
   where
    d = StockData {
        stockId = StockId "test" "stable", stockName = "stable"
      , stockCapMln = earn * marketPeRatio finParam
      , stockShareCountMln = 1000
      , stockCurrentAssets = 0, stockCurrentLiability = 0, stockDebt = 0, stockEquity = 1000
      , stockPast = [PastYear rev earn]
    }
    finParam = finParams ! "test"
    mar = 0.1
    rev = 1000.0
    earn = rev * mar
    sre = distrN (gdpGrowth finParam) 0.0
    smr = distrN mar 0.0