module Main where

import Control.Monad (replicateM)
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
  deriving (Show)

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

readStockData :: FilePath -> IO (Either String (V.Vector Stock))
readStockData filePath = do
  withFile filePath ReadMode $ \handle -> do
    csvData <- BS.hGetContents handle
    return $ decode NoHeader (BL.fromStrict csvData)

-- WIP
computeSharpe :: [[DailyReturn]] -> [Weight] -> SharpeRatio
computeSharpe _ _ = 0

generateWeights :: Int -> IO [Float]
generateWeights n
  | n <= 0 = pure []
  | otherwise = tryWeights
  where
    tryWeights = do
      raw <- replicateM n (randomRIO (0, 1))
      let total = max (sum raw) 1e-10
          weights = map (/ total) raw
      if any (> 0.2) weights
        then tryWeights
        else pure weights

findBestPortfolioForEachCombination :: V.Vector Stock -> [Int] -> IO Portfolio
findBestPortfolioForEachCombination stockVec indices = do
  let selectedStocks = map (stockVec V.!) indices :: [Stock]
      tickers = map t selectedStocks :: [Ticker]
      dailyReturns = map drs selectedStocks :: [[DailyReturn]]
      numTrials = 100 :: Int
  portfolios <-
    mapM
      ( \_ -> do
          weights <- generateWeights (length indices)
          let sharpeRatio = computeSharpe dailyReturns weights
          pure $ Portfolio sharpeRatio weights tickers
      )
      [1 .. numTrials]
  pure $ maximumBy (comparing sr) portfolios

main :: IO ()
main = do
  let filePath = "data/dow_jones_close_prices_aug_dec_2024.csv"
  result <- readStockData filePath
  case result of
    Left err -> putStrLn $ "Error parsing CSV: " ++ err
    Right records -> do
      let numSelectedAssets = 25 :: Int
          combinations = computeCombinations numSelectedAssets [0 .. V.length records - 1]

      portfolios <- mapM (findBestPortfolioForEachCombination records) combinations
      let bestPortfolio = maximumBy (comparing sr) portfolios

      putStrLn $ "Best portfolio: " ++ show bestPortfolio
