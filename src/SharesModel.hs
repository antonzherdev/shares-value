module SharesModel (FinParam(..), Stock(..), StockYear(..), RevEarn(..), revGrowth, margin) where

import Rnd

data FinParam = FinParam {
    riskFreeRate :: Double,
    marketPeRatio :: Double
  } deriving (Show)

data Stock = Stock {
    stockName :: String,
    stockCash :: Double,
    stockRevenue :: Double,
    stockCapMln :: Double,
    stockShareCountMln :: Double,
    stockFuture :: [StockYear]
  } deriving (Show)

data StockYear = StockYear {
    year :: Int,
    yearRevenueGrowth :: N,
    yearMargin :: N
  } deriving (Show)

revGrowth :: Int -> N -> N -> StockYear
revGrowth = StockYear

margin :: (N -> StockYear) -> N -> StockYear
margin f = f

data RevEarn = RevEarn {
    reRevenue :: Double,
    reEarnings :: Double
  } deriving (Show)