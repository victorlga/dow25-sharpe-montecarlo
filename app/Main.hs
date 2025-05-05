module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv (FromRecord(parseRecord), decode, HasHeader(NoHeader), parseField)
import Control.Monad (when)
import System.IO (withFile, IOMode(ReadMode))

-- Define the record structure
data StockData = StockData
  { ticker :: !BS.ByteString
  , prices :: !(V.Vector Float)
  } deriving (Show)

-- Custom parser for the CSV
instance FromRecord StockData where
  parseRecord v = do
    when (V.length v < 1) $ fail "CSV record must have at least one column"
    let tickerStr = v V.! 0 :: BS.ByteString
    priceList <- mapM (parseField . (v V.!)) [1..V.length v - 1]
    pure $ StockData tickerStr (V.fromList priceList)

-- Function to read and parse CSV into a Vector of StockData
readStockData :: FilePath -> IO (Either String (V.Vector StockData))
readStockData filePath = do
  withFile filePath ReadMode $ \handle -> do
    csvData <- BS.hGetContents handle
    return $ decode NoHeader (BL.fromStrict csvData)

main :: IO ()
main = do
  let filePath = "data/dow_jones_close_prices_aug_dec_2024.csv"
  result <- readStockData filePath
  case result of
    Left err -> putStrLn $ "Error parsing CSV: " ++ err
    Right _ -> putStrLn "\nDone"