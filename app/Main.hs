module Main where

import Control.Monad (foldM, replicateM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Csv
  ( FromRecord (parseRecord),
    HasHeader (NoHeader),
    decode,
    parseField,
  )
import Data.List (transpose)
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

numTrials :: Int
numTrials = 1000

filePath :: String
filePath = "data/dow_jones_close_prices_aug_dec_2024.csv"

computeCombinations :: Int -> [a] -> [[a]]
computeCombinations k xs = go k xs []
  where
    go 0 _ acc = [acc]
    go _ [] _ = []
    go n (y : ys) acc = go (n - 1) ys (y : acc) ++ go n ys acc

computeReturns :: [Price] -> [DailyReturn]
computeReturns [] = []
computeReturns [_] = []
computeReturns (p0 : p1 : ps) = (p1 - p0) / p0 : computeReturns (p1 : ps)

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
mean xs = if null xs then 0 else foldl' (+) 0 xs / fromIntegral (length xs)

covariance :: [Float] -> [Float] -> Float
covariance xs ys
  | null xs || null ys || length xs == 1 = 0
  | otherwise =
      let mx = mean xs
          my = mean ys
          n = fromIntegral (length xs)
          products = zipWith (\x y -> (x - mx) * (y - my)) xs ys
       in foldl' (+) 0 products / (n - 1)

computeCovarianceMatrix :: [[DailyReturn]] -> [[Float]]
computeCovarianceMatrix dailyReturns =
  [[covariance (dailyReturns !! i) (dailyReturns !! j) | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]
  where
    n = length dailyReturns

computePortfolioMetrics :: [[DailyReturn]] -> [[Float]] -> [Weight] -> (Float, Float)
computePortfolioMetrics dailyReturns covMatrix weights =
  let portfolioReturns = map (weightedReturn weights) dailyReturns
      expectedDailyReturn = mean portfolioReturns
      daysInYear = 252 :: Float
      annualizedReturn = expectedDailyReturn * daysInYear

      n = length weights
      portfolioVariance = sum [weights !! i * sum [covMatrix !! i !! j * weights !! j | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]
      portfolioStdDev = if portfolioVariance <= 0 then 0 else sqrt portfolioVariance * sqrt daysInYear
   in (annualizedReturn, portfolioStdDev)
  where
    weightedReturn :: [Weight] -> [DailyReturn] -> Float
    weightedReturn wList rs = sum (zipWith (*) wList rs)

computeSharpe :: Float -> Float -> SharpeRatio
computeSharpe annualizedReturn portfolioStdDev =
  let riskFreeRate = 0
   in if portfolioStdDev <= 0
        then 0
        else (annualizedReturn - riskFreeRate) / portfolioStdDev

generateWeights :: Int -> IO [Weight]
generateWeights n = do
  tryWeights
  where
    tryWeights = do
      raw <- replicateM n (randomRIO (0, 1))
      let total = max (sum raw) 1e-10
          weights = map (/ total) raw
          maxWeight = 0.2
      if any (> maxWeight) weights
        then tryWeights
        else pure weights

generateAllWeightSets :: Int -> Int -> IO [[Weight]]
generateAllWeightSets trials n = replicateM trials (generateWeights n)

findBestPortfolio :: V.Vector Stock -> [Int] -> [[Weight]] -> IO Portfolio
findBestPortfolio stockVec indices weightSets = do
  let selectedStocks = map (stockVec V.!) indices
      tickers = map t selectedStocks
      dailyReturns = map drs selectedStocks
      transposedReturns = transpose dailyReturns
      -- Trocar para apenas montar a covMatrix a partir das covariacias gerais
      covMatrix = computeCovarianceMatrix dailyReturns

  let initialBest = Portfolio (-1) [] tickers

      processSingleWeightSet bestSoFar weights =
        let (annRet, stdDev) = computePortfolioMetrics transposedReturns covMatrix weights
            sharpe = computeSharpe annRet stdDev
         in if sharpe > sr bestSoFar
              then Portfolio sharpe weights tickers
              else bestSoFar

  return $ foldl' processSingleWeightSet initialBest weightSets

main :: IO ()
main = do
  -- Trocar uso de listas por uso de vetores
  --    Usar "import qualified Data.Vector.Unboxed as U" quando for vetor de tipo primitivo
  -- Pre-computar a variancia e a covariancia entre todos os ativos antes de entrar em loops
  --    Criar um Map em que a chave é uma tupla com os dois tickers e o valor é a covariancia.
  result <- readStockData
  let numSelectedAssets = 25
  case result of
    Left err -> putStrLn $ "Error parsing CSV: " ++ err
    Right records -> do
      let combinations = computeCombinations numSelectedAssets [0 .. V.length records - 1]
          initialBestPortfolio = Portfolio (-1) [] []

      finalBestPortfolio <- foldM processOneCombination initialBestPortfolio combinations

      putStrLn $ "\nBest portfolio: " ++ show finalBestPortfolio
      putStrLn $ "\nSum of weights: " ++ show (sum $ ws finalBestPortfolio)
      where
        processOneCombination :: Portfolio -> [Int] -> IO Portfolio
        processOneCombination currentBest indices = do
          weightSets <- generateAllWeightSets numTrials numSelectedAssets
          newBest <- findBestPortfolio records indices weightSets
          let !srNew = sr newBest
          let !srCurrent = sr currentBest
          return $ if srNew > srCurrent then newBest else currentBest
