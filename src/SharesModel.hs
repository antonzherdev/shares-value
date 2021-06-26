module SharesModel (
  FinParam(..), Stock(..), StockYear(..), RevEarn(..),
  revGrowth, margin, loadStock, reMargin, updateStockCsv) where

import Data.List.Split
import Rnd
import qualified Data.Map as Map
import Data.Map ((!?))
import Data.Maybe
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit.Combinators     (sinkFile)
import Network.HTTP.Conduit         (parseRequest, requestHeaders)
import Network.HTTP.Simple          (httpSink)
import Network.HTTP.Types.Header    (hAuthorization)
import qualified Data.ByteString.Char8 as C

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
    stockDebt :: Double,
    stockEquity :: Double,
    stockRevenue :: Double,
    stockEarnings :: Double,
    stockCapMln :: Double,
    stockShareCountMln :: Double,
    stockFuture :: [StockYear]
  } 


data StockYear  = StockYear {
    year :: Int,
    yearRevenueGrowth :: Distr,
    yearMargin :: Distr
  } 

revGrowth :: Int -> Distr -> Distr -> StockYear
revGrowth = StockYear

margin :: (Distr -> StockYear) -> Distr -> StockYear
margin f = f

data RevEarn = RevEarn {
    reRevenue :: Double,
    reEarnings :: Double
  } deriving (Show)
reMargin :: RevEarn -> Double
reMargin (RevEarn r e) = e/r


loadStock :: (String, String, String) -> [StockYear] -> (String, IO Stock)
loadStock (market, symbol, name) future = (symbol, processFile <$> readFile (stockFileName (market, symbol)))
  where
    processFile :: String -> Stock
    processFile text = Stock {
        stockMarket = market,
        stockSymbol = symbol,
        stockName = name,
        stockCurrentAssets = d "health_current_assets",
        stockCurrentLiability = d "health_total_current_liab",
        stockDebt = d "health_total_debt",
        stockEquity = d "health_total_equity",
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
        
--        authorization: Bearer e526640dcd26f56e7df5d3459579e622619d5a44
stockFileName :: (String, String) -> String
stockFileName (market, symbol) = "shares/" ++  market ++ "_" ++ symbol ++ ".csv"

stockCsvUrl :: (String, String) -> String
stockCsvUrl (m, s) = "https://api.simplywall.st/api/company/download/csv/" ++ m ++ ":" ++ s

updateStockCsv :: String -> (String, String) -> IO ()
updateStockCsv token stock = do
  req0 <- parseRequest $ stockCsvUrl stock
  let req = req0 {requestHeaders= requestHeaders req0 ++ [(hAuthorization, C.pack $ "Bearer " ++ token)]}
  runResourceT $ httpSink req $ \_ -> sinkFile (stockFileName stock)
