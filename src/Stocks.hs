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
    , makeStock ("NZSE", "ATM", "A2 Milk") $ semiProjFuture (0.3, 0.0) [2021 .. 2031] [
         revGrowth ((-0.50) `minMax95` (-0.22)) `margin` (0.0 `minMax95` 0.12)
       , revGrowth (meanMinMax95 0.20 0.0 0.60) `margin` (0.05 `minMax95` 0.20)
       , revGrowth (  0.10  `minMax95`   0.25)  `margin` (0.10 `minMax95` 0.25)
      ]
    , makeStock ("NZSE", "FWL", "Foley Wines") $ semiProjFuture (0.3, 0.0) [2021 .. 2031] [
         revGrowth ((-0.10) `minMax95`   0.05)  `margin` (0.06 `minMax95` 0.15)
      ] 
    , makeStock ("ASX", "TWE", "Treasury Wine Estates") $ semiProjFuture (0.3, 0.0) [2021 .. 2031] [
       revGrowth ((-0.10) `minMax95`   0.05)  `margin` (0.07 `minMax95` 0.15)
    ]
    , makeStock ("NZSE", "FPH", "Fisher & Paykel health") $ semiProjFuture (0.3, 0.0) [2021 .. 2031] [
        revGrowth ((-0.50) `minMax95` (-0.10)) `margin` (0.15 `minMax95` 0.30)
      , revGrowth (  0.05  `minMax95`   0.25)  `margin` (0.15 `minMax95` 0.30)
     ]
    , makeStock ("NZSE", "AIR", "Air New Zealand") $ semiProjFuture (0.3, 0.0) [2021 .. 2031] [
    {- from Dec 2018 -} 
    {- to   Dec 2019 = $5867 -}
    
    {- from Dec 2019 -}  
    {- to   Dec 2020 = $3055 -}
    
    {- from Dec 2020 -}  {- through Jun 2021 $2400 - $2600-}
       revenue (2400 `minMax95` 3000) `fixedExpenses` meanMinMax95 950 600 1600 `margin` (0.1 `minMax95` 0.2)
    {- to   Dec 2021 -}
    
    {- from Dec 2021   40% - 50% of $5800 -}
     , revenue (3200 `minMax95` 4350) `fixedExpenses` meanMinMax95 1000 600 2000 `margin` (0.15 `minMax95` 0.22)
    {- to   Dec 2022   70% - 100% of $5800 -}
    
    {- from Dec 2022   70% - 110% of $5800 -}
     , revenue (4600 `minMax95` 6400) `fixedExpenses` meanMinMax95 1000 600 2000 `margin` (0.17 `minMax95` 0.25)
    {- to   Dec 2023   90% - 110% of $5800 -}
     , revGrowth (meanMinMax95 0.03 (-0.02) 0.3) `margin` (0.04 `minMax95` 0.10)                
    ] . dropLastYear
    , makeStock ("NZSE", "KMD", "Kathmandu") $ semiProjFuture (0.3, 0.0) [2021 .. 2031] [
        revGrowth (0.07 `minMax95` 0.15) `margin` (0.03 `minMax95` 0.07)
      , revGrowth (0.05 `minMax95` 0.17) `margin` (0.03 `minMax95` 0.10)
    ]
    , makeStock ("ASX", "HVN", "Harvey Norman") $ projFuture (0.3, 0.0) [2021 .. 2031]
    , makeStock ("NZSE", "HLG", "Hallenstein Glasson Holdings") $ projFuture (0.3, 0.0) [2021 .. 2031]
    , makeStock ("NZSE", "MFT", "Mainfreight") $ projFuture (0.3, 0.0) [2021 .. 2031]
    , makeStock ("NZSE", "SPK", "Spark") $ projFuture (0.3, 0.0) [2021 .. 2031]
    , makeStock ("NZSE", "FBU", "Fletcher Buildings") $ projFuture (0.3, 0.0) [2021 .. 2031]
    , makeStock ("NZSE", "MEL", "Meredian Energy") $ semiProjFuture (0.3, 0.0) [2021 .. 2031] [
       revGrowth ((-0.20) `minMax95` 0.22) `margin`  (0.03 `minMax95` 0.12)
     , revGrowth ((-0.20) `minMax95` 0.22) `margin`  (0.03 `minMax95` 0.12)
     , revGrowth ((-0.20) `minMax95` 0.22) `margin`  (0.03 `minMax95` 0.12)
     , revGrowth ((-0.33) `minMax95` 0.09) `margin`  (0.03 `minMax95` 0.12)
     , revGrowth ((-0.20) `minMax95` 0.22) `margin`  (0.03 `minMax95` 0.12)
    ]
    , makeStock ("ASX", "IAG", "Insurance Australia Group") $ semiProjFuture (0.3, 0.0) [2021 .. 2031] [
       revGrowth ((-0.20) `minMax95` 0.07) `margin`  ((-0.05) `minMax95` 0.0)
     ] 
  ]

dropLastYear :: StockData -> StockData
dropLastYear s = s {stockPast = tail (stockPast s)}

projFuture :: (Double, Double) -> [Int] -> StockData -> StockFuture
projFuture (gDecay, mDecay) years d =
  (\(_, g, m) -> revGrowth (meanMinMax95N g) `margin` meanMinMax95N m) <$>
    yss 
--    trace (show yss) yss

  where
    yss = go years (pastRevenueGrowth d, pastMargin d)
    gg = gdpGrowth $ finParams ! (stockIdMarket . stockId) d

    go :: [Int] -> (MeanMinMax, MeanMinMax) -> [(Int, MeanMinMax, MeanMinMax)]
    go  [] _ = []
    go (y:ys) (g, m) = (y, g', m') : go ys (g', m')
      where
        gd = gDecay * (mmmMean g - gg)
        g' = 
--          trace (show y ++ "=" ++ show gd ++ " <<< " ++ show g) $ 
          MeanMinMax (mmmMean g - gd) (mmmMin g - gd) (mmmMax g - gd)
        md = mDecay * mmmMean m
        m' = MeanMinMax (mmmMean m - md) (mmmMin m - md) (mmmMax m - md)

semiProjFuture :: (Double, Double) -> [Int] -> StockFuture -> StockData -> StockFuture
semiProjFuture dd years fut d = fut ++ drop (length fut) prj
  where
    prj = projFuture dd years d


allStockIds :: [StockId]
allStockIds = filter (\s -> stockIdMarket s /= "test") $ Map.keys stocks

stock :: StockId -> IO Stock
stock = (!) stocks


-- stock price should be equal to pe ratio if stock growth with inflation rate
stableStock :: Stock
stableStock = Stock d [
       revGrowth sre `margin` smr
     , revGrowth sre `margin` smr
     , revGrowth sre `margin` smr
     , revGrowth sre `margin` smr
     , revGrowth sre `margin` smr
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