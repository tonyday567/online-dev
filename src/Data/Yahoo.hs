{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Data.Yahoo where

import Chart
import Control.Lens hiding ((:>), (<&>), Unwrapped, Wrapped)
import Data.Csv hiding (Field)
import Data.Generics.Labels ()
import Data.Maybe
import Data.Text.Encoding (decodeUtf8)
import Data.Time
import Options.Generic
import PMLB
import PMLB.Csv
import Prelude
import qualified Control.Foldl as L
import qualified Data.Attoparsec.ByteString.Char8 as AC

-- yahoo formatted data
-- See: https://finance.yahoo.com/quote/%5EGSPC/history?period1=-631015200&period2=1497103200&interval=1d&filter=history&frequency=1d
data YahooData
  = YahooData
      { yDate :: !Text,
        yOpen :: Double,
        yHigh :: Double,
        yLow :: Double,
        yClose :: Double,
        yAdjClose :: !Double,
        yVolume :: Double
      }
  deriving (Generic, Show)

instance FromNamedRecord YahooData where
  parseNamedRecord m =
    YahooData <$> m .: "Date" <*> m .: "Open" <*> m .: "High" <*> m .: "Low"
      <*> m
        .: "Close"
      <*> m
        .: "Adj Close"
      <*> m
        .: "Volume"

pYahoo :: Char -> AC.Parser YahooData
pYahoo c = do
  d <- AC.takeWhile (/= c)
  _ <- AC.char c
  o <- AC.double
  _ <- AC.char c
  h <- AC.double
  _ <- AC.char c
  l <- AC.double 
  _ <- AC.char c
  close <- AC.double
  _ <- AC.char c
  ac <- AC.double
  _ <- AC.char c
  v <- AC.double
  _ <- AC.endOfLine
  pure (YahooData (decodeUtf8 d) (realToFrac o) (realToFrac h) (realToFrac l) (realToFrac close) (realToFrac ac) (realToFrac v))

yahooConfig :: CsvConfig
yahooConfig = defaultCsvConfig & #dataSet .~ "^GSPC" & #csep .~ ','

getYahoo :: Int -> IO [YahooData]
getYahoo n = taker n . snd <$> runBS yahooConfig (parseCsv PMLB.Csv.HasHeader 10000000 ',' pYahoo)

taker :: Int -> [a] -> [a]
taker n = reverse . take n . reverse

getdc :: [YahooData] -> [(UTCTime, Double)]
getdc xs = lret $ (\x -> (view #yDate x, view #yClose x)) <$> xs

-- compute the log return from a price series
-- returns are geometric by nature, and using log(1+daily return) as the base unit of the time series leads to all sorts of benefits, not least of which is you can then add up the variates to get the cumulative return, without having to go through log and exp chicanery.  Ditto for distributional assumptions.
lret :: [(Text, Double)] -> [(UTCTime, Double)]
lret [] = []
lret [_] = []
lret ((_, v0) : xs) = L.fold (L.Fold step ([], v0) (reverse . fst)) xs
  where
    step (acc, v) (d', v') = case parseUTCTime d' of
      Just d -> ((d, log (v' / v)) : acc, v')
      Nothing -> (acc, v)

