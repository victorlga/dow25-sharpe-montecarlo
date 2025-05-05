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

data Stock = Stock
  { ticker :: !Ticker,
    dailyReturns :: ![Float]
  }
  deriving (Show)

data Portfolio = Portfolio
  { sharpeRatio :: Float,
    weights :: [Float],
    assets :: [Ticker]
  }
  deriving (Show)

computeCombinations :: Int -> [a] -> [[a]]
computeCombinations k xs = go k xs []
  where
    go 0 _ acc = [acc]
    go _ [] _ = []
    go n (x : ys) acc = go (n - 1) ys (x : acc) ++ go n ys acc

computeReturns :: [Float] -> [Float]
computeReturns vec =
  zipWith (\p1 p0 -> (p1 - p0) / p0) (tail vec) (init vec)

instance FromRecord Stock where
  parseRecord v = do
    prices <- mapM (parseField . (v V.!)) [1 .. V.length v - 1]
    let t = v V.! 0
        rs = computeReturns prices
    pure $ Stock t rs

readStockData :: FilePath -> IO (Either String (V.Vector Stock))
readStockData filePath = do
  withFile filePath ReadMode $ \handle -> do
    csvData <- BS.hGetContents handle
    return $ decode NoHeader (BL.fromStrict csvData)

computeSharpe :: [[Float]] -> [Float] -> Float
computeSharpe stockReturns stockWeights = sum (zipWith (*) (map sum stockReturns) stockWeights)

generateWeights :: Int -> IO [Float]
generateWeights n
  | n <= 0 = pure []
  | otherwise = tryWeights
  where
    tryWeights = do
      raw <- replicateM n (randomRIO (0, 1))
      let total = max (sum raw) 1e-10
          ws = map (/ total) raw
      if any (> 0.2) ws
        then tryWeights
        else pure ws

findBestPortfolioForEachCombination :: V.Vector Stock -> [Int] -> IO Portfolio
findBestPortfolioForEachCombination stockVec indices = do
  let stocks = map (stockVec V.!) indices :: [Stock]
      ts = map ticker stocks :: [Ticker]
      drs = map dailyReturns stocks :: [[Float]]
      numTrials = 1000 :: Int
  portfolios <-
    mapM
      ( \_ -> do
          ws <- generateWeights (length indices)
          let sr = computeSharpe drs ws
          pure $ Portfolio sr ws ts
      )
      [1 .. numTrials]
  pure $ maximumBy (comparing sharpeRatio) portfolios

main :: IO ()
main = do
  let filePath = "data/dow_jones_close_prices_aug_dec_2024.csv"
  result <- readStockData filePath
  case result of
    Left err -> putStrLn $ "Error parsing CSV: " ++ err
    Right records -> do
      let totalAssets = V.length records
          numSelectedAssets = 25 :: Int
          indices = [0 .. totalAssets - 1]
          combinations = computeCombinations numSelectedAssets indices
          total = length combinations

      putStrLn $
        "Total combinations of "
          ++ show numSelectedAssets
          ++ " assets from "
          ++ show totalAssets
          ++ " assets: "
          ++ show total

      portfolios <- mapM (findBestPortfolioForEachCombination records) combinations
      let bestPortfolio = maximumBy (comparing sharpeRatio) portfolios

      putStrLn $ "Best portfolio: " ++ show bestPortfolio
