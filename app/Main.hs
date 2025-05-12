-- |
-- Module      : Main
-- Description : Portfolio optimization using Sharpe ratio
--
-- This program processes stock price data, calculates various metrics,
-- and finds the optimal portfolio weights that maximize the Sharpe ratio.
module Main where

import Control.Monad (foldM)
import DataLoader (Stock(stockReturns), loadStockCsvData)
import qualified Data.Vector as V
import Simulator
  ( Portfolio(portfolioSharpe, portfolioWeights, portfolioTickers),
    computeCovarianceMatrix,
    createEmptyPortfolio,
    generateIndexCombinations,
    portfolioSize,
    tryPortfolio,
    numTrials
  )

--------------------------------------------------------------------------------
-- Main Program
--------------------------------------------------------------------------------

-- | Handle the optimization process
runOptimization :: V.Vector Stock -> IO Portfolio
runOptimization stockVec = do
  putStrLn $ "\nLoaded " ++ show (V.length stockVec) ++ " stocks."
  putStrLn $ "Optimizing for portfolios of size " ++ show portfolioSize

  let !allCombinations = generateIndexCombinations portfolioSize [0 .. V.length stockVec - 1]
      !covMatrix = computeCovarianceMatrix (V.map stockReturns stockVec)
      !initialBest = createEmptyPortfolio

  putStrLn $ "\nTesting " ++ show (length allCombinations) ++ " portfolio combinations."
  putStrLn $ "Running " ++ show numTrials ++ " Monte Carlo trials per combination."

  foldM (tryPortfolio stockVec covMatrix) initialBest allCombinations

-- | Display the portfolio results
displayPortfolioResults :: Portfolio -> IO ()
displayPortfolioResults portfolio = do
  putStrLn $ "\nBest portfolio found:"
  putStrLn $ "- Sharpe Ratio: " ++ show (portfolioSharpe portfolio)
  putStrLn $ "- Stock Count: " ++ show (length $ portfolioTickers portfolio)
  putStrLn $ "- Sum of weights: " ++ show (sum $ portfolioWeights portfolio)

  putStrLn "\nPortfolio composition:"
  let !weightedTickers = zip (portfolioTickers portfolio) (portfolioWeights portfolio)
      formatWeight weight = show (weight * 100) ++ "%"
      formatTicker ticker weight = "  " ++ show ticker ++ ": " ++ formatWeight weight

  mapM_ (uncurry formatTicker >>> putStrLn) weightedTickers
  where
    (>>>) f g = \x -> g (f x)

-- | Main program entry point
main :: IO ()
main = do
  stockData <- loadStockCsvData
  case stockData of
    Left err -> putStrLn $ "Error parsing CSV: " ++ err
    Right stockVec -> do
      bestPortfolio <- runOptimization stockVec
      displayPortfolioResults bestPortfolio