module Main where

import Control.DeepSeq (NFData)
import Control.Monad (replicateM)
import Control.Parallel.Strategies (parListChunk, rdeepseq, withStrategy)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Csv
  ( FromRecord (parseRecord),
    HasHeader (NoHeader),
    decode,
    parseField,
  )
import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import System.IO (IOMode (ReadMode), withFile)
import System.Random (randomRIO)

type Ticker = BS.ByteString

type DailyReturn = Float

type SharpeRatio = Float

type Weight = Float

type Price = Float

data Stock = Stock
  { t :: !Ticker,
    drs :: ![DailyReturn]
  }
  deriving (Show)

data Portfolio = Portfolio
  { sr :: SharpeRatio,
    ws :: [Weight],
    assets :: [Ticker]
  }
  deriving (Show, Generic, NFData)

daysInYear :: Float
daysInYear = 252

numTrials :: Int
numTrials = 100

numSelectedAssets :: Int
numSelectedAssets = 25

riskFreeRate :: Float
riskFreeRate = 0.025

maxWeight :: Float
maxWeight = 0.2

filePath :: String
filePath = "data/dow_jones_close_prices_aug_dec_2024.csv"

chunkSize :: Int
chunkSize = 1000

computeCombinations :: Int -> [a] -> [[a]]
computeCombinations k xs = go k xs []
  where
    go 0 _ acc = [acc]
    go _ [] _ = []
    go n (x : ys) acc = go (n - 1) ys (x : acc) ++ go n ys acc

computeReturns :: [Price] -> [DailyReturn]
computeReturns vec =
  zipWith (\p1 p0 -> (p1 - p0) / p0) (tail vec) (init vec)

instance FromRecord Stock where
  parseRecord v = do
    prices <- mapM (parseField . (v V.!)) [1 .. V.length v - 1]
    let ticker = v V.! 0
        dailyReturns = computeReturns prices
    pure $ Stock ticker dailyReturns

readStockData :: IO (Either String (V.Vector Stock))
readStockData = do
  withFile filePath ReadMode $ \handle -> do
    csvData <- BS.hGetContents handle
    return $ decode NoHeader (BL.fromStrict csvData)

mean :: [Float] -> Float
mean xs = if null xs then 0 else sum xs / fromIntegral (length xs)

covariance :: [Float] -> [Float] -> Float
covariance xs ys
  | null xs || null ys || length xs == 1 = 0
  | otherwise =
      let mx = mean xs
          my = mean ys
          n = fromIntegral (length xs)
       in sum (zipWith (\x y -> (x - mx) * (y - my)) xs ys) / (n - 1)

computeAnnualizedReturn :: [[DailyReturn]] -> [Weight] -> Float
computeAnnualizedReturn dailyReturns weights =
  let portfolioReturns = map (weightedReturn weights) (transpose dailyReturns)
      expectedDailyReturn = mean portfolioReturns
   in expectedDailyReturn * daysInYear
  where
    weightedReturn :: [Weight] -> [DailyReturn] -> Float
    weightedReturn wList rs = sum (zipWith (*) wList rs)

    transpose :: [[a]] -> [[a]]
    transpose [] = []
    transpose xs = if any null xs then [] else map head xs : transpose (map tail xs)

computePortfolioStdDev :: [[DailyReturn]] -> [Weight] -> Float
computePortfolioStdDev dailyReturns weights =
  let n = length dailyReturns
      covMatrix = [[covariance (dailyReturns !! i) (dailyReturns !! j) | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]
      portfolioVariance = sum [weights !! i * sum [covMatrix !! i !! j * weights !! j | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]
   in if portfolioVariance <= 0 then 0 else sqrt portfolioVariance * sqrt daysInYear

computeSharpe :: [[DailyReturn]] -> [Weight] -> SharpeRatio
computeSharpe dailyReturns weights =
  let annualizedReturn = computeAnnualizedReturn dailyReturns weights
      portfolioStdDev = computePortfolioStdDev dailyReturns weights
      sharpe =
        if portfolioStdDev <= 0
          then 0
          else (annualizedReturn - riskFreeRate) / portfolioStdDev
   in sharpe

generateWeights :: Int -> IO [Weight]
generateWeights n = do
  tryWeights
  where
    tryWeights = do
      raw <- replicateM n (randomRIO (0, 1))
      let total = max (sum raw) 1e-10
          weights = map (/ total) raw
      if any (> maxWeight) weights
        then tryWeights
        else pure weights

generateAllWeightSets :: Int -> Int -> IO [[Weight]]
generateAllWeightSets trials n = replicateM trials (generateWeights n)

findBestPortfolio :: V.Vector Stock -> [Int] -> [[Weight]] -> Portfolio
findBestPortfolio stockVec indices weightSets =
  let selectedStocks = map (stockVec V.!) indices
      tickers = map t selectedStocks
      dailyReturns = map drs selectedStocks
      portfolios = [Portfolio (computeSharpe dailyReturns weights) weights tickers | weights <- weightSets]
   in maximumBy (comparing sr) portfolios

main :: IO ()
main = do
  result <- readStockData
  case result of
    Left err -> putStrLn $ "Error parsing CSV: " ++ err
    Right records -> do
      let combinations = computeCombinations numSelectedAssets [0 .. V.length records - 1]
      putStrLn $ "Generating " ++ show (length combinations) ++ " combinations..."
      weightSetsList <- replicateM (length combinations) (generateAllWeightSets numTrials numSelectedAssets)

      let portfolios =
            withStrategy (parListChunk chunkSize rdeepseq) $
              zipWith (findBestPortfolio records) combinations weightSetsList

      putStrLn $ "\nGenerated " ++ show (length portfolios) ++ " portfolios."
      let bestPortfolio = maximumBy (comparing sr) portfolios

      putStrLn $ "\nBest portfolio: " ++ show bestPortfolio
      putStrLn $ "Sum of weights: " ++ show (sum $ ws bestPortfolio)
