module SharesModel (
  FinParam(..), Stock(..), StockYear(..), RevEarn(..),
  revGrowth, margin, stockBalanceOffset, loadStock, reMargin) where

import Data.List.Split
import Rnd
import qualified Data.Map as Map
import Data.Map ((!?))
import Data.Maybe

data FinParam = FinParam {
    gdpGrowth :: Double,
    marketPeRatio :: Double
  } deriving (Show)

data Stock = Stock {
    stockMarket :: String,
    stockSymbol :: String,
    stockName :: String,
    stockCurrentAssets :: Double,
    stockCurrentLiability :: Double,
    stockLongTermLiability :: Double,
    stockRevenue :: Double,
    stockEarnings :: Double,
    stockCapMln :: Double,
    stockShareCountMln :: Double,
    stockFuture :: [StockYear]
  } deriving (Show)

stockBalanceOffset :: Stock -> Double
stockBalanceOffset s = stockCurrentAssets s - stockCurrentLiability s - stockLongTermLiability s

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
reMargin :: RevEarn -> Double
reMargin (RevEarn r e) = e/r


loadStock :: (String, String, String) -> [StockYear] -> (String, IO Stock)
loadStock (market, symbol, name) future = (symbol, processFile <$> readFile ("shares/" ++  market ++ "_" ++ symbol ++ ".csv"))
  where
    processFile :: String -> Stock
    processFile text = Stock {
        stockMarket = market,
        stockSymbol = symbol,
        stockName = name,
        stockCurrentAssets = d "health_current_assets",
        stockCurrentLiability = d "health_total_current_liab",
        stockLongTermLiability = d "health_long_term_liab",
        stockRevenue = d "past_revenue",
        stockEarnings = d "past_ebt_excluding" * (1.0 - d "value_intrinsic_value_tax_rate"),
        stockCapMln = d "market_cap_listing" / 1000000,
        stockShareCountMln = d "market_cap_shares_outstanding" / 1000000,
        stockFuture = future
      }
      where
        parts = Map.fromList $ map (\ss -> (head ss, tail ss)) $ splitOn "," <$> lines text
        d :: String -> Double
        d nm = read $ head $ fromMaybe (error $ "Cannot find " ++ nm) $ parts !? nm