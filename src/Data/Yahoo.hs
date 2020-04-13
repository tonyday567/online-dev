{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Data.Yahoo
  ( runReturn
  , taker
  , pYahoo
  , yahooCsvConfig
  ) where

import NumHask.Prelude
import Chart
import Control.Lens hiding ((:>), (<&>), Unwrapped, Wrapped)
import Data.Csv
import Data.Generics.Labels ()
import Data.Time
import qualified Data.Attoparsec.Text as A
import qualified Control.Foldl as L

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

-- | yahoo data format
-- >>> A.parseOnly (pYahoo ',') "1927-12-30,17.660000,17.660000,17.660000,17.660000,17.660000,0"
-- Right (YahooData {yDate = "1927-12-30", yOpen = 17.66, yHigh = 17.66, yLow = 17.66, yClose = 17.66, yAdjClose = 17.66, yVolume = 0.0})
pYahoo :: Char -> A.Parser YahooData
pYahoo c = do
  d <- A.takeWhile (/= c)
  _ <- A.char c
  o <- A.double
  _ <- A.char c
  h <- A.double
  _ <- A.char c
  l <- A.double 
  _ <- A.char c
  close <- A.double
  _ <- A.char c
  ac <- A.double
  _ <- A.char c
  v <- A.double
  pure (YahooData d (realToFrac o) (realToFrac h) (realToFrac l) (realToFrac close) (realToFrac ac) (realToFrac v))

yahooCsvConfig :: CsvConfig
yahooCsvConfig = defaultCsvConfig & #name .~ "^GSPC" & #csep .~ ',' & #dir .~ "./other"

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

getdc :: [YahooData] -> [(UTCTime, Double)]
getdc xs = lret $ (\x -> (view #yDate x, view #yClose x)) <$> xs

taker :: Int -> [a] -> [a]
taker n = reverse . take n . reverse

runReturn :: Int -> IO [(UTCTime, Double)]
runReturn n = do
  r <- runE yahooCsvConfig pYahoo
  pure $ taker n $ getdc $ rights r
