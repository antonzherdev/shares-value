module SharesModel (
  FinParam(..), StockData(..), Stock(..), StockFuture(..), StockId(..), FutureYear, RevEarn(..), PastYear(..),
  revGrowth, margin, loadStockData, makeStock, reMargin, updateStockCsv, stockRevenue, pastMargin,
  pastRevenueGrowths, pastRevenueGrowth, revenue, fixedExpenses,
  stockEarnings, pastYearMargin) where

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

data PastYear = PastYear {
  pastYearRevenue :: Double,
  pastYearEarnings :: Double
}

pastYearMargin :: PastYear -> Double
pastYearMargin d = pastYearEarnings d / pastYearRevenue d

data StockData = StockData {
    stockId :: StockId,
    stockName :: String,
    stockCurrentAssets :: Double,
    stockCurrentLiability :: Double,
    stockDebt :: Double,
    stockEquity :: Double,
    stockPast :: [PastYear],
    stockCapMln :: Double,
    stockShareCountMln :: Double
  }

pastMargin :: StockData -> MeanMinMax
pastMargin d = mkMeanMinMax $ pastYearMargin <$> stockPast d

pastRevenueGrowth :: StockData -> MeanMinMax
pastRevenueGrowth = mkMeanMinMax . pastRevenueGrowths

pastRevenueGrowths :: StockData -> [Double]
pastRevenueGrowths d =
  let revs = pastYearRevenue <$> stockPast d in
  (\ (a, b) -> (a - b)/b ) <$> zip revs (tail revs)


stockRevenue :: StockData -> Double
stockRevenue = pastYearRevenue . head . stockPast

stockEarnings :: StockData -> Double
stockEarnings = pastYearEarnings . head . stockPast

data StockFuture = StockFuture {
  futureYears :: [FutureYear],
  futureAssetsAdjustment :: Distr
}
type FutureYear = RevEarn -> Rnd RevEarn

data Stock = Stock {
  stockData :: StockData,
  stockFuture :: StockFuture
}

revGrowth :: Distr -> FutureYear
revGrowth grow (RevEarn r0 _ _) = grow >>= (\g -> return $ RevEarn (r0 * (1 + g)) 0 0)

revenue :: Distr -> FutureYear
revenue rev _ = rev >>= (\r -> return $ RevEarn r 0 0)

margin :: FutureYear -> Distr -> FutureYear
margin prev mgn re =
  do
     p <- prev re
     m <- mgn
     return p {reEarnings=reRevenue p * m - reFixedExpenses p}

fixedExpenses :: FutureYear -> Distr -> FutureYear
fixedExpenses prev fe re =
  do
     p <- prev re
     e <- fe
     return p {reFixedExpenses=e}

data RevEarn = RevEarn {
    reRevenue :: Double,
    reEarnings :: Double,
    reFixedExpenses :: Double
  } deriving (Show)
reMargin :: RevEarn -> Double
reMargin (RevEarn r e _) = e/r


makeStock :: (String, String, String) -> (StockData -> StockFuture) -> (StockId, IO Stock)
makeStock (market, symbol, name) makeFuture =
  let sId = StockId market symbol in
  (sId, loadStockData sId name >>= (\d -> return $ Stock d (makeFuture d)))


loadStockData :: StockId -> String -> IO StockData
loadStockData sId name = processFile <$> readFile (stockFileName sId)
  where
    processFile :: String -> StockData
    processFile text = StockData {
        stockId = sId,
        stockName = name,
        stockCurrentAssets = d "health_current_assets",
        stockCurrentLiability = d "health_total_current_liab",
        stockDebt = d "health_total_debt",
        stockEquity = d "health_total_equity",
        stockPast = uncurry PastYear <$> zip (dd "past_revenue_ltm_history") (dd "past_net_income_ltm_history"),
        stockCapMln = d "market_cap_listing" / 1000000,
        stockShareCountMln = d "market_cap_shares_outstanding" / 1000000
      }
      where
        parts = Map.fromList $ map (\ss -> (head ss, tail ss)) $ splitOn "," <$> lines text
        part :: String -> [String]
        part nm = fromMaybe (error $ "Cannot find " ++ nm) $ parts !? nm
        d :: String -> Double
        d = read . head . part
        dd :: String -> [Double]
        dd nm = fmap read $ filter (/= "") $ part nm
        
stockFileName :: StockId -> String
stockFileName (StockId market symbol) = "shares/" ++  market ++ "_" ++ symbol ++ ".csv"

stockCsvUrl :: StockId -> String
stockCsvUrl (StockId m s) = "https://api.simplywall.st/api/company/download/csv/" ++ m ++ ":" ++ s

updateStockCsv :: String -> StockId -> IO ()
updateStockCsv token stock = do
  req0 <- parseRequest $ stockCsvUrl stock
  let req = req0 {requestHeaders= requestHeaders req0 ++ [(hAuthorization, C.pack $ "Bearer " ++ token)]}
  runResourceT $ httpSink req $ \_ -> sinkFile (stockFileName stock)
