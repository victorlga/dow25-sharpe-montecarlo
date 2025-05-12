-- |
-- Module      : DataLoader
-- Description : Stock data loading and processing functionality
--
-- This module handles loading and parsing stock price data from CSV files.
module DataLoader
  ( Stock (..),
    Ticker,
    DailyReturn,
    Price,
    calculateDailyReturns,
    loadStockCsvData,
    csvFilePath,
  )
where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Csv
  ( FromRecord (parseRecord),
    HasHeader (NoHeader),
    decode,
    parseField,
  )
import qualified Data.Vector as V
import GHC.Generics (Generic)
import System.IO (IOMode (ReadMode), withFile)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Stock ticker symbol
type Ticker = BS.ByteString

-- | Day-to-day return percentage
type DailyReturn = Float

-- | Stock price
type Price = Float

-- | Represents a stock with its ticker and historical daily returns
data Stock = Stock
  { -- | The stock's ticker symbol
    stockTicker :: Ticker,
    -- | List of historical daily returns
    stockReturns :: [DailyReturn]
  }
  deriving (Show, Generic)

instance NFData Stock -- For forcing evaluation

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Path to the CSV file containing historical stock prices
csvFilePath :: String
csvFilePath = "data/dow_jones_close_prices_aug_dec_2024.csv"

--------------------------------------------------------------------------------
-- CSV Parsing
--------------------------------------------------------------------------------

-- | Parse a CSV record into a Stock
instance FromRecord Stock where
  parseRecord v = do
    prices <- mapM (parseField . (v V.!)) [1 .. V.length v - 1]
    let ticker = v V.! 0
        !returns = calculateDailyReturns prices
    pure $! Stock ticker returns

-- | Load and parse stock data from the CSV file
loadStockCsvData :: IO (Either String (V.Vector Stock))
loadStockCsvData = do
  withFile csvFilePath ReadMode $ \handle -> do
    csvData <- BS.hGetContents handle
    return $! decode NoHeader (BL.fromStrict csvData)

--------------------------------------------------------------------------------
-- Financial Calculations
--------------------------------------------------------------------------------

-- | Convert a sequence of prices to daily returns
calculateDailyReturns :: [Price] -> [DailyReturn]
calculateDailyReturns [] = []
calculateDailyReturns [_] = []
calculateDailyReturns (p0 : p1 : ps) =
  let !r = (p1 - p0) / p0
   in r : calculateDailyReturns (p1 : ps)