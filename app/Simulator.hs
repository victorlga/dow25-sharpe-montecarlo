-- |
-- Module      : Simulator
-- Description : Portfolio simulation and optimization functionality
--
-- This module handles portfolio generation, simulation, and optimization.
module Simulator
  ( Portfolio (..),
    SharpeRatio,
    Weight,
    daysPerYear,
    maxAllowedWeight,
    numTrials,
    portfolioSize,
    average,
    calculateCovariance,
    computeCovarianceMatrix,
    applyWeights,
    calculatePortfolioVariance,
    calculatePortfolioStdDev,
    calculateAnnualizedReturn,
    evaluatePortfolio,
    calculateSharpeRatio,
    generateIndexCombinations,
    generateNormalizedWeights,
    generateMultipleWeightSets,
    extractSelectedStocks,
    extractSelectedCovMatrix,
    testWeightSet,
    processWeightChunk,
    distributeWeightSetProcessing,
    evaluateWeightSets,
    generatePortfolioWeightSets,
    selectBetterPortfolio,
    tryPortfolio,
    negativeInfinity,
    createEmptyPortfolio,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (replicateM)
import Control.Parallel.Strategies (parList, rdeepseq, withStrategy)
import DataLoader (DailyReturn, Stock(stockTicker, stockReturns), Ticker)
import Data.List (maximumBy, transpose, foldl')
import Data.List.Split (chunksOf)
import Data.Ord (comparing)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import GHC.Conc (getNumCapabilities)
import GHC.Generics (Generic)
import System.Random (randomRIO)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Portfolio performance metric (return/risk)
type SharpeRatio = Float

-- | Portfolio allocation percentage for a stock
type Weight = Float

-- | Represents a portfolio with performance metrics and composition
data Portfolio = Portfolio
  { -- | The portfolio's Sharpe ratio (higher is better)
    portfolioSharpe :: SharpeRatio,
    -- | Weight allocation for each stock
    portfolioWeights :: [Weight],
    -- | List of stock tickers in the portfolio
    portfolioTickers :: [Ticker]
  }
  deriving (Show, Generic)

instance NFData Portfolio -- For parallel evaluation

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Number of trading days in a year, used for annualization
daysPerYear :: Float
daysPerYear = 252

-- | Maximum allowed weight for any single stock in the portfolio
maxAllowedWeight :: Float
maxAllowedWeight = 0.2

-- | Number of Monte Carlo trials per portfolio combination
numTrials :: Int
numTrials = 1000

-- | Size of portfolios to generate
portfolioSize :: Int
portfolioSize = 25

--------------------------------------------------------------------------------
-- Financial Calculations
--------------------------------------------------------------------------------

-- | Calculate the arithmetic mean of a list of numbers
average :: [Float] -> Float
average xs =
  if null xs
    then 0
    else
      let !s = sum xs
          !l = fromIntegral (length xs)
       in s / l

-- | Calculate covariance between two time series
calculateCovariance :: [Float] -> [Float] -> Float
calculateCovariance xs ys
  | null xs || null ys || length xs == 1 = 0
  | otherwise =
      let !mx = average xs
          !my = average ys
          !n = fromIntegral (length xs)
          !products = zipWith (\x y -> let !dx = x - mx; !dy = y - my in dx * dy) xs ys
          !s = sum products
       in s / (n - 1)

-- | Compute the full covariance matrix for multiple return series
computeCovarianceMatrix :: V.Vector [DailyReturn] -> V.Vector (U.Vector Float)
computeCovarianceMatrix returnsVec =
  let !n = V.length returnsVec
   in V.generate n (\i -> U.generate n (\j -> calculateCovariance (returnsVec V.! i) (returnsVec V.! j)))

-- | Apply weights to a set of daily returns
applyWeights :: [Weight] -> [DailyReturn] -> Float
applyWeights ws rs =
  let !products = zipWith (*) ws rs
   in sum products

-- | Calculate portfolio variance given weights and a covariance matrix
calculatePortfolioVariance :: V.Vector (U.Vector Float) -> [Weight] -> Float
calculatePortfolioVariance covarianceMatrix weights =
  let !n = length weights
      !sum_val = foldl' (\acc i -> 
          let !w_i = weights !! i
              !inner_sum = foldl' (\inner_acc j -> 
                  let !w_j = weights !! j
                      !cov_ij = covarianceMatrix V.! i U.! j
                  in inner_acc + cov_ij * w_j) 0 [0 .. n - 1]
          in acc + w_i * inner_sum) 0 [0 .. n - 1]
  in sum_val

-- | Calculate annualized portfolio standard deviation
calculatePortfolioStdDev :: Float -> Float
calculatePortfolioStdDev portfolioVariance =
  if portfolioVariance <= 0
    then 0
    else
      let !sqrtVar = sqrt portfolioVariance
          !sqrtDays = sqrt daysPerYear
       in sqrtVar * sqrtDays

-- | Calculate annualized return for a weighted portfolio
calculateAnnualizedReturn :: [[DailyReturn]] -> [Weight] -> Float
calculateAnnualizedReturn timeSeries weights =
  let !weightedReturnsOverTime = map (applyWeights weights) timeSeries
      !avgDailyReturn = average weightedReturnsOverTime
   in avgDailyReturn * daysPerYear

-- | Calculate annualized return and standard deviation for a portfolio
evaluatePortfolio :: [[DailyReturn]] -> V.Vector (U.Vector Float) -> [Weight] -> (Float, Float)
evaluatePortfolio timeSeries covarianceMatrix weights =
  let !annualizedReturn = calculateAnnualizedReturn timeSeries weights
      !portfolioVariance = calculatePortfolioVariance covarianceMatrix weights
      !portfolioStdDev = calculatePortfolioStdDev portfolioVariance
   in (annualizedReturn, portfolioStdDev)

-- | Calculate Sharpe ratio from annualized return and standard deviation
calculateSharpeRatio :: Float -> Float -> SharpeRatio
calculateSharpeRatio annualReturn stdDev =
  if stdDev <= 0
    then 0
    else annualReturn / stdDev

--------------------------------------------------------------------------------
-- Portfolio Generation
--------------------------------------------------------------------------------

-- | Generate all possible combinations of k elements from a list
generateIndexCombinations :: Int -> [a] -> [[a]]
generateIndexCombinations k xs = go k xs []
  where
    go 0 _ acc = [acc]
    go _ [] _ = []
    go n (y : ys) acc = go (n - 1) ys (y : acc) ++ go n ys acc

-- | Generate a set of random portfolio weights that sum to 1.0
-- and respect the maximum weight constraint
generateNormalizedWeights :: Int -> IO [Weight]
generateNormalizedWeights n = do
  trySample
  where
    trySample = do
      rawWeights <- replicateM n (randomRIO (0, 1))
      let !total = max (sum rawWeights) 1e-10
          !normalized = map (/ total) rawWeights
      if any (> maxAllowedWeight) normalized
        then trySample
        else pure normalized

-- | Generate multiple sets of portfolio weights
generateMultipleWeightSets :: Int -> Int -> IO [[Weight]]
generateMultipleWeightSets trials n = replicateM trials (generateNormalizedWeights n)

--------------------------------------------------------------------------------
-- Optimization Functions
--------------------------------------------------------------------------------

-- | Extract selected stocks from the full vector
extractSelectedStocks :: V.Vector Stock -> [Int] -> [Stock]
extractSelectedStocks stockVec selectedIdx = map (stockVec V.!) selectedIdx

-- | Extract the covariance submatrix for selected stocks
extractSelectedCovMatrix :: V.Vector (U.Vector Float) -> [Int] -> V.Vector (U.Vector Float)
extractSelectedCovMatrix covMatrix selectedIdx =
  let !selectedIdxVec = U.fromList selectedIdx
      !n = length selectedIdx
      !result = V.generate n $ \i ->
        U.generate n $ \j ->
          covMatrix V.! (selectedIdxVec U.! i) U.! (selectedIdxVec U.! j)
   in result

-- | Evaluate a single weight set and determine if it's better than the current best
testWeightSet :: [[DailyReturn]] -> V.Vector (U.Vector Float) -> [Ticker] -> Portfolio -> [Weight] -> Portfolio
testWeightSet timeSeries covMatrix tickers bestSoFar weights =
  let !(annReturn, stdDev) = evaluatePortfolio timeSeries covMatrix weights
      !sharpe = calculateSharpeRatio annReturn stdDev
   in if sharpe > portfolioSharpe bestSoFar
        then Portfolio sharpe weights tickers
        else bestSoFar

-- | Process a chunk of weight sets to find the best performing one
processWeightChunk :: [[DailyReturn]] -> V.Vector (U.Vector Float) -> [Ticker] -> Portfolio -> [[Weight]] -> Portfolio
processWeightChunk timeSeries covMatrix tickers initialPortfolio chunk =
  foldl' (testWeightSet timeSeries covMatrix tickers) initialPortfolio chunk

-- | Distribute weight sets processing across multiple cores
distributeWeightSetProcessing :: [[DailyReturn]] -> V.Vector (U.Vector Float) -> [Ticker] -> [[Weight]] -> IO Portfolio
distributeWeightSetProcessing timeSeries covMatrix tickers weightSets = do
  numCaps <- getNumCapabilities
  let !numWeightSets = length weightSets
      !chunkSize = max 1 (numWeightSets `div` numCaps) -- Ensure chunkSize is at least 1 to prevent empty chunks
      !wsChunks = chunksOf chunkSize weightSets

      !initialPortfolio = Portfolio (negate infinity) [] tickers
      !infinity = 1.0 / 0.0 :: Float

      processChunk :: [[Weight]] -> Portfolio
      processChunk chunk = processWeightChunk timeSeries covMatrix tickers initialPortfolio chunk

      bestPortfoliosFromChunks :: [Portfolio]
      bestPortfoliosFromChunks = withStrategy (parList rdeepseq) (map processChunk wsChunks)

  return $! maximumBy (comparing portfolioSharpe) bestPortfoliosFromChunks

-- | Evaluate multiple weight sets for a portfolio and find the best
evaluateWeightSets :: V.Vector Stock -> V.Vector (U.Vector Float) -> [Int] -> [[Weight]] -> IO Portfolio
evaluateWeightSets stockVec covMatrix selectedIdx weightSets = do
  let !selectedStocks = extractSelectedStocks stockVec selectedIdx
      !tickers = map stockTicker selectedStocks
      !returns = map stockReturns selectedStocks
      !timeSeries = transpose returns
      !selectedCovMatrix = extractSelectedCovMatrix covMatrix selectedIdx

  distributeWeightSetProcessing timeSeries selectedCovMatrix tickers weightSets

-- | Generate weight sets for a portfolio combination
generatePortfolioWeightSets :: Int -> IO [[Weight]]
generatePortfolioWeightSets size = generateMultipleWeightSets numTrials size

-- | Compare two portfolios and return the one with higher Sharpe ratio
selectBetterPortfolio :: Portfolio -> Portfolio -> Portfolio
selectBetterPortfolio p1 p2 =
  let !s1 = portfolioSharpe p1
      !s2 = portfolioSharpe p2
   in if s1 > s2 then p1 else p2

-- | Try a specific portfolio combination and update the best if better
tryPortfolio :: V.Vector Stock -> V.Vector (U.Vector Float) -> Portfolio -> [Int] -> IO Portfolio
tryPortfolio stockVec covMatrix currentBest selectedIdx = do
  !weightSets <- generatePortfolioWeightSets portfolioSize
  !candidate <- evaluateWeightSets stockVec covMatrix selectedIdx weightSets
  return $! selectBetterPortfolio candidate currentBest

-- | Create negative infinity value for initial portfolio comparison
negativeInfinity :: Float
negativeInfinity = negate (1.0 / 0.0)

-- | Initialize an empty portfolio with negative infinity Sharpe ratio
createEmptyPortfolio :: Portfolio
createEmptyPortfolio = Portfolio negativeInfinity [] []