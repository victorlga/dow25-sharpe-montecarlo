module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
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
    go n (x:ys) acc = go (n-1) ys (x:acc) ++ go n ys acc

computeReturns :: V.Vector Float -> V.Vector Float
computeReturns vec =
    V.zipWith (\p1 p0 -> (p1 - p0) / p0) (V.tail vec) (V.init vec)

instance FromRecord StockData where
  parseRecord v = do
    when (null v) $ fail "CSV record must have at least one column"
    let tickerStr = v V.! 0
    priceList <- mapM (parseField . (v V.!)) [1..V.length v - 1]
    let priceVec = V.fromList priceList
    let returnVec = computeReturns priceVec
    pure $ StockData tickerStr priceVec returnVec

readStockData :: FilePath -> IO (Either String (V.Vector StockData))
readStockData filePath = do
  withFile filePath ReadMode $ \handle -> do
    csvData <- BS.hGetContents handle
    return $ decode NoHeader (BL.fromStrict csvData)

main :: IO ()
main = do
  let totalAssets = 30 :: Int
      selectedAssets = 25 :: Int
      indices = [0 .. totalAssets - 1]
      assetSubsets = subsets selectedAssets indices
      total = length assetSubsets
      filePath = "data/dow_jones_close_prices_aug_dec_2024.csv"

  putStrLn $ "Total combinations of " ++ show selectedAssets ++ " assets from " ++ show totalAssets ++ " assets: " ++ show total

  result <- readStockData filePath
  case result of
    Left err -> putStrLn $ "Error parsing CSV: " ++ err
    Right records -> V.mapM_ print records