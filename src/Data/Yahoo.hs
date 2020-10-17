{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Data.Yahoo
  ( makeReturnSeries,
    taker,
    YahooData (..),
    pYahoo,
    yahooCsvConfig,
  )
where

import Box.Csv
import Control.Lens hiding ((:>), (<&>), Unwrapped, Wrapped)
import qualified Data.Attoparsec.Text as A
import Data.Generics.Labels ()
import Data.Mealy
import Data.Time
import NumHask.Prelude

-- | yahoo csv data
-- See: https://finance.yahoo.com/quote/%5EGSPC/history?period1=-631015200&period2=1497103200&interval=1d&filter=history&frequency=1d
data YahooData
  = YahooData
      { yDate :: Day,
        yOpen :: Double,
        yHigh :: Double,
        yLow :: Double,
        yClose :: Double,
        yAdjClose :: Double,
        yVolume :: Double
      }
  deriving (Generic, Show)

pDay :: A.Parser Day
pDay = do
  y <- A.decimal
  _ <- A.char '-'
  m <- A.decimal
  _ <- A.char '-'
  d <- A.decimal
  pure $ fromGregorian y m d

-- | yahoo data format
-- >>> A.parseOnly (pYahoo ',') "1927-12-30,17.660000,17.660000,17.660000,17.660000,17.660000,0"
-- Right (YahooData {yDate = "1927-12-30", yOpen = 17.66, yHigh = 17.66, yLow = 17.66, yClose = 17.66, yAdjClose = 17.66, yVolume = 0.0})
pYahoo :: Char -> A.Parser YahooData
pYahoo c = do
  d <- pDay
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
yahooCsvConfig = defaultCsvConfig & #name .~ "data" & #fsep .~ ',' & #dir .~ "./other"

-- compute the log return from a price series
-- returns are geometric by nature, and using log(1+daily return) as the base unit of the time series leads to all sorts of benefits, not least of which is you can then add up the variates to get the cumulative return, without having to go through log and exp chicanery.  Ditto for distributional assumptions.
lret :: (ExpField a) => [a] -> [a]
lret xs = drop 1 $ scan ((\x x' -> log (x / x')) <$> id <*> delay [zero]) xs

getdc :: [YahooData] -> [(Day, Double)]
getdc xs = zip (drop 1 $ view #yDate <$> xs) (lret (view #yClose <$> xs))

taker :: Int -> [a] -> [a]
taker n = reverse . take n . reverse

makeReturnSeries :: Int -> IO [(Day, Double)]
makeReturnSeries n = do
  r <- runCsv yahooCsvConfig pYahoo
  pure $ taker n $ getdc $ rights r
