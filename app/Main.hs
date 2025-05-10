module Main where

import Control.Monad (foldM, replicateM)
import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Csv
  ( FromRecord (parseRecord),
    HasHeader (NoHeader),
    decode,
    parseField,
  )
import Data.List (transpose)
import Data.List.Split (chunksOf)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.IO (IOMode (ReadMode), withFile)
import System.Random (randomRIO)
import Control.Parallel.Strategies (withStrategy, parList, rdeepseq)
import GHC.Conc (getNumCapabilities)
import GHC.Generics (Generic)

type Ticker = BS.ByteString

type DailyReturn = Float

type SharpeRatio = Float

type Weight = Float

type Price = Float

data Stock = Stock
  { stockTicker :: !Ticker,
    stockReturns :: ![DailyReturn]
  }
  deriving (Show)

data Portfolio = Portfolio
  { portfolioSharpe :: SharpeRatio,
    portfolioWeights :: [Weight],
    portfolioTickers :: [Ticker]
  }
  deriving (Show, Generic)

instance NFData Portfolio

csvFilePath :: String
csvFilePath = "data/dow_jones_close_prices_aug_dec_2024.csv"

generateIndexCombinations :: Int -> [a] -> [[a]]
generateIndexCombinations k xs = go k xs []
  where
    go 0 _ acc = [acc]
    go _ [] _ = []
    go n (y : ys) acc = go (n - 1) ys (y : acc) ++ go n ys acc

calculateDailyReturns :: [Price] -> [DailyReturn]
calculateDailyReturns [] = []
calculateDailyReturns [_] = []
calculateDailyReturns (p0:p1:ps) = (p1 - p0) / p0 : calculateDailyReturns (p1:ps)

instance FromRecord Stock where
  parseRecord v = do
    prices <- mapM (parseField . (v V.!)) [1 .. V.length v - 1]
    let ticker = v V.! 0
        returns = calculateDailyReturns prices
    pure $ Stock ticker returns

loadStockCsvData :: IO (Either String (V.Vector Stock))
loadStockCsvData = do
  withFile csvFilePath ReadMode $ \handle -> do
    csvData <- BS.hGetContents handle
    return $ decode NoHeader (BL.fromStrict csvData)

average :: [Float] -> Float
average xs = if null xs then 0 else foldl (+) 0 xs / fromIntegral (length xs)

calculateCovariance :: [Float] -> [Float] -> Float
calculateCovariance xs ys
  | null xs || null ys || length xs == 1 = 0
  | otherwise =
      let mx = average xs
          my = average ys
          n = fromIntegral (length xs)
          products = zipWith (\x y -> (x - mx) * (y - my)) xs ys
       in foldl (+) 0 products / (n - 1)

computeCovarianceMatrix :: V.Vector [DailyReturn] -> V.Vector (U.Vector Float)
computeCovarianceMatrix returnsVec =
  V.generate n (\i -> U.generate n (\j -> calculateCovariance (returnsVec V.! i) (returnsVec V.! j)))
  where
    n = V.length returnsVec

evaluatePortfolio :: [[DailyReturn]] -> V.Vector (U.Vector Float) -> [Weight] -> (Float, Float)
evaluatePortfolio timeSeries covarianceMatrix weights =
  let weightedReturnsOverTime = map (applyWeights weights) timeSeries
      avgDailyReturn = average weightedReturnsOverTime
      daysPerYear = 252 :: Float
      annualizedReturn = avgDailyReturn * daysPerYear

      n = length weights
      portfolioVariance = sum [weights !! i * sum [covarianceMatrix V.! i U.! j * weights !! j | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]
      portfolioStdDev = if portfolioVariance <= 0 then 0 else sqrt portfolioVariance * sqrt daysPerYear
   in (annualizedReturn, portfolioStdDev)
  where
    applyWeights :: [Weight] -> [DailyReturn] -> Float
    applyWeights ws rs = sum (zipWith (*) ws rs)

calculateSharpeRatio :: Float -> Float -> SharpeRatio
calculateSharpeRatio annualReturn stdDev =
  if stdDev <= 0
    then 0
    else annualReturn / stdDev

generateNormalizedWeights :: Int -> IO [Weight]
generateNormalizedWeights n = do
  trySample
  where
    trySample = do
      rawWeights <- replicateM n (randomRIO (0, 1))
      let total = max (sum rawWeights) 1e-10
          normalized = map (/ total) rawWeights
          maxAllowedWeight = 0.2
      if any (> maxAllowedWeight) normalized
        then trySample
        else pure normalized

generateMultipleWeightSets :: Int -> Int -> IO [[Weight]]
generateMultipleWeightSets trials n = replicateM trials (generateNormalizedWeights n)

evaluateWeightSets :: V.Vector Stock -> V.Vector (U.Vector Float) -> [Int] -> [[Weight]] -> IO Portfolio
evaluateWeightSets stockVec covMatrix selectedIdx weightSets = do
  let selectedStocks = map (stockVec V.!) selectedIdx
      tickers = map stockTicker selectedStocks
      returns = map stockReturns selectedStocks
      timeSeries = transpose returns
      selectedIdxVec = U.fromList selectedIdx
      selectedCovMatrix = V.generate (length selectedIdx) $ \i ->
                          U.generate (length selectedIdx) $ \j ->
                            covMatrix V.! (selectedIdxVec U.! i) U.! (selectedIdxVec U.! j)

      testWeightSet bestSoFar weights =
        let (annReturn, stdDev) = evaluatePortfolio timeSeries selectedCovMatrix weights
            sharpe = calculateSharpeRatio annReturn stdDev
         in if sharpe > portfolioSharpe bestSoFar
              then Portfolio sharpe weights tickers
              else bestSoFar

  numCaps <- getNumCapabilities
  let numWeightSets = length weightSets
      chunkSize = max 1 (numWeightSets `div` numCaps)  -- Ensure chunkSize is at least 1
      wsChunks = chunksOf chunkSize weightSets
  
  let initialPortfolio = Portfolio (-1.0/0.0) [] tickers
  let processChunk :: [[Weight]] -> Portfolio
      processChunk chunk = foldl' testWeightSet initialPortfolio chunk

  let bestPortfoliosFromChunks :: [Portfolio]
      bestPortfoliosFromChunks = withStrategy (parList rdeepseq) (map processChunk wsChunks)
  
  return $ foldl' (\acc p -> if portfolioSharpe p > portfolioSharpe acc then p else acc) initialPortfolio bestPortfoliosFromChunks

main :: IO ()
main = do
  stockData <- loadStockCsvData
  let portfolioSize = 25
  case stockData of
    Left err -> putStrLn $ "Error parsing CSV: " ++ err
    Right stockVec -> do
      let allCombinations = generateIndexCombinations portfolioSize [0 .. V.length stockVec - 1]
          covMatrix = computeCovarianceMatrix (V.map stockReturns stockVec)
          initialBest = Portfolio (-1) [] []

      bestPortfolio <- foldM (\best idxs -> tryPortfolio covMatrix best idxs) initialBest allCombinations

      putStrLn $ "\nBest portfolio: " ++ show bestPortfolio
      putStrLn $ "\nSum of weights: " ++ show (sum $ portfolioWeights bestPortfolio)
      where
        tryPortfolio :: V.Vector (U.Vector Float) -> Portfolio -> [Int] -> IO Portfolio
        tryPortfolio covMatrix currentBest selectedIdx = do
          let numTrials = 100
          weightSets <- generateMultipleWeightSets numTrials portfolioSize
          !candidate <- evaluateWeightSets stockVec covMatrix selectedIdx weightSets
          return $ if portfolioSharpe candidate > portfolioSharpe currentBest then candidate else currentBest