module SharesModel (
  FinParam(..), Stock(..), StockId(..), StockYear(..), RevEarn(..),
  revGrowth, margin, loadStock, reMargin, updateStockCsv, stockId) where

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
    paramMarket :: String,
    gdpGrowth :: Double,
    marketPeRatio :: Double
  } deriving (Show)

data StockId = StockId {
    stockIdMarket :: String,
    stockIdSymbol :: String
  } deriving (Show, Eq)

instance Ord StockId where
  compare (StockId lm ls) (StockId rm rs) 
    | lm == rm = compare ls rs
    | otherwise = compare lm rm

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
stockId :: Stock -> StockId
stockId s = StockId (stockMarket s) (stockSymbol s)

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


loadStock :: (String, String, String) -> [StockYear] -> (StockId, IO Stock)
loadStock (market, symbol, name) future = (sId, processFile <$> readFile (stockFileName sId))
  where
    sId = StockId market symbol
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
        
stockFileName :: StockId -> String
stockFileName (StockId market symbol) = "shares/" ++  market ++ "_" ++ symbol ++ ".csv"

stockCsvUrl :: StockId -> String
stockCsvUrl (StockId m s) = "https://api.simplywall.st/api/company/download/csv/" ++ m ++ ":" ++ s

updateStockCsv :: String -> StockId -> IO ()
updateStockCsv token stock = do
  req0 <- parseRequest $ stockCsvUrl stock
  let req = req0 {requestHeaders= requestHeaders req0 ++ [(hAuthorization, C.pack $ "Bearer " ++ token)]}
  runResourceT $ httpSink req $ \_ -> sinkFile (stockFileName stock)
