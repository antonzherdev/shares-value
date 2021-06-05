module SharesModel (FinParam(..), Stock(..), StockYear(..), RevEarn(..)) where

import Rnd

data FinParam = FinParam {
    riskFreeRate :: Double,
    marketPeRatio :: Double
  } deriving (Show)

data Stock = Stock {
    stockCash :: Double,
    stockRevenue :: Double,
    stockCapMln :: Double,
    stockShareCountMln :: Double,
    stockFuture :: [StockYear]
  } deriving (Show)

data StockYear = StockYear {
    yearRevenueGrowth :: N,
    yearMargin :: N
  } deriving (Show)

data RevEarn = RevEarn {
    reRevenue :: Double,
    reEarnings :: Double
  } deriving (Show)