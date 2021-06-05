module Lib
    ( someFunc
    ) where
import Shares
import Rnd
import Data.List
import Data.Foldable

--riskFreeRate = 0.021
peRatio = 21.7


capMln = 4513
sharesCountMln = 743.41
cash = 774
revenue0 = 1600
future = [
   {-2021-}   ((-0.5) ..< (-0.22), 0.0 ..< 0.12)
   {-2022-} , (0.20 <..> (0.0, 0.60), 0.05 ..< 0.20)
   {-2023-} , (0.10 ..< 0.25, 0.10 ..< 0.25)
   {-2024-} , (0.00 ..< 0.25, 0.10 ..< 0.25)
   {-2025-} , ((-0.10) ..< 0.20, 0.10 ..< 0.25)
  ]

someFunc :: IO ()
--someFunc = print $ test
someFunc =  do
  putStrLn $ "Share price    = " ++ show (snd simulation |*| (1/sharesCountMln))
  putStrLn $ "Current price  = " ++ show (capMln/sharesCountMln)
  putStrLn $ "Capitalisation = " ++ show (snd simulation)
  foldlM printYear 2021 (fst simulation)
  return ()
--  putStrLn $ simulateMmm 10000 0.95 $ intrinsicValue 0.021 $ earnings
  where
    printYear :: Int -> (MeanMinMax, MeanMinMax) -> IO Int
    printYear year (rev, earn) =
      do
        putStrLn $ "# " ++ show year
        putStrLn $ "Revenue  = " ++ show rev
        putStrLn $ "Earnings = " ++ show earn
        putStrLn $ "Margin   = " ++ show (rev
        return $ year + 1

--someFunc = putStrLn $ simulateMmm 10000 0.95 $ intrinsicValue 0.021 $ (10, random (2000 ..< 3000))

--someFunc = print $ intrinsicValue 0.021 10 2706



simulation :: ([(MeanMinMax, MeanMinMax)], MeanMinMax)
simulation = (map calcMean years, mk $ map snd simulations)
  where
    years = transpose $ map fst simulations
    mk = mkMeanMinMax . withConfidence 0.95
    calcMean rs = (mk $ map revenue rs, mk $ map earnings rs)


simulations :: [([RevEarn], Double)]
simulations = simulate 10000 calc

calc :: Rnd ([RevEarn], Double)
calc = do
  es <- snd simEarnings
  let ev = cash + (intrinsicValue (1/peRatio) (map earnings es))
  return $ (es, ev)

simEarnings :: (Int, Rnd [RevEarn])
simEarnings = calcEarnings revenue0 future

