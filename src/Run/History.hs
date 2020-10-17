{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Run.History
  ( HistoryConfig (..),
    defaultHistoryConfig,
    defaultQuantiles,
    quantileNames,
    nAll,
    makeReturns,
    repHistoryConfig,
    Model1HistoryConfig (..),
    defaultModel1HistoryConfig,
    repModel1HistoryConfig,
    historyChartNames,
    historyCharts,

    -- * analysis
    model1fit,
    model1BetaFit,
    stats,
    qscan,
    dscan,
  )
where

import Chart
import Chart.Mealy hiding (taker)
import Chart.Various
import Control.Category ((<<<), (>>>))
import Control.Lens hiding ((:>), (<&>), Unwrapped, Wrapped)
import Data.Generics.Labels ()
import qualified Data.HashMap.Strict as HashMap
import Data.List ((!!))
import Data.Mealy
import Data.Mealy.Quantiles
import Data.Time
import Data.Yahoo
import NumHask.Array.Fixed
import NumHask.Prelude hiding (fold)
import Web.Rep

-- * representation

data HistoryConfig
  = HistoryConfig
      { hN :: Int,
        hRunup :: Int,
        hRate :: Double,
        hRates :: [Double],
        hRateBetaMa2X :: Double,
        hRateBetaStd2X :: Double,
        hCharts :: [Text],
        hQuantiles :: [Double],
        hQuantileRate :: Double
      }
  deriving (Eq, Show, Generic)

defaultHistoryConfig :: HistoryConfig
defaultHistoryConfig = HistoryConfig 10000 2000 0.99 [0.99, 0.95] 0.99 0.99 ["ma", "std", "betama"] defaultQuantiles 0.99

defaultQuantiles :: [Double]
defaultQuantiles = (0.1 *) <$> [1 .. 9]

quantileNames :: [Double] -> [Text]
quantileNames qs = (<> "th") . comma (Just 0) . (100 *) <$> qs

nAll :: HistoryConfig -> Int
nAll c = (c ^. #hN) + (c ^. #hRunup)

makeReturns :: HistoryConfig -> IO [(UTCTime, Double)]
makeReturns c =
  fmap (first (`UTCTime` 0)) <$> makeReturnSeries ((c ^. #hN) + (c ^. #hRunup))

repHistoryConfig :: (Monad m) => [Text] -> HistoryConfig -> SharedRep m HistoryConfig
repHistoryConfig allItems cfg = bimap hmap HistoryConfig a <<*>> b <<*>> c <<*>> d <<*>> e <<*>> f <<*>> g <<*>> h <<*>> i
  where
    a =
      either (const (view #hN cfg)) id
        <$> readTextbox (Just "n") (view #hN cfg)
    b =
      either (const (view #hRunup cfg)) id
        <$> readTextbox (Just "runup") (view #hRunup cfg)
    c =
      either (const (view #hRate cfg)) id
        <$> readTextbox (Just "rate") (view #hRate cfg)
    d =
      either (const (view #hRates cfg)) id
        <$> readTextbox (Just "rates") (view #hRates cfg)
    e =
      either (const (view #hRateBetaMa2X cfg)) id
        <$> readTextbox (Just "rate betaMa2X") (view #hRateBetaMa2X cfg)
    f =
      either (const (view #hRateBetaStd2X cfg)) id
        <$> readTextbox (Just "rate betaStd2X") (view #hRateBetaStd2X cfg)
    g = repItemsSelect (view #hCharts cfg) allItems
    h =
      either (const (view #hQuantiles cfg)) id
        <$> readTextbox (Just "quantiles") (view #hQuantiles cfg)
    i =
      either (const (view #hQuantileRate cfg)) id
        <$> readTextbox (Just "quantile rate") (view #hQuantileRate cfg)
    hmap a b c d e f g h i = a <> b <> c <> d <> e <> f <> g <> h <> i

data Model1HistoryConfig
  = Model1HistoryConfig
      { m1Min :: Double,
        m1Max :: Double,
        m1RegRate :: Double
      }
  deriving (Eq, Show, Generic)

defaultModel1HistoryConfig :: Model1HistoryConfig
defaultModel1HistoryConfig = Model1HistoryConfig (-10) 10 0.999

repModel1HistoryConfig :: (Monad m) => Model1HistoryConfig -> SharedRep m Model1HistoryConfig
repModel1HistoryConfig cfg = bimap hmap Model1HistoryConfig a <<*>> b <<*>> c
  where
    a =
      either (const (cfg ^. #m1Min)) id
        <$> readTextbox (Just "model1 min beta") (cfg ^. #m1Min)
    b =
      either (const (cfg ^. #m1Max)) id
        <$> readTextbox (Just "model1 max beta") (cfg ^. #m1Max)
    c =
      either (const (cfg ^. #m1RegRate)) id
        <$> readTextbox (Just "model1 regression beta") (cfg ^. #m1RegRate)
    hmap a b c = a <> b <> c

-- |
--
-- > rs <- makeReturns defaultHistoryConfig
-- > HashMap.keys $ historyCharts defaultHistoryConfig defaultModel1HistoryConfig rs
-- ["betaMa2X","quantile histogram","betaStd2X","stats","laststd","quantiles","model1JustBeta","ma","digit pixel","ma vs std","digitise","std","lastma"]
historyChartNames :: [Text]
historyChartNames = ["betaMa2X", "quantile histogram", "betaStd2X", "stats", "laststd", "quantiles", "model1JustBeta", "ma", "digit pixel", "ma vs std", "digitise", "std", "lastma"]

-- | a map of the possible history charts
historyCharts :: HistoryConfig -> Model1HistoryConfig -> [(UTCTime, Double)] -> HashMap.HashMap Text (HudOptions, [Chart Double])
historyCharts hc m1hc xs =
  HashMap.fromList
    [ ( "stats",
        (mempty, tableChart (stats defaultHistoryConfig xs))
      ),
      ( "ma",
        ( tsRatesHud "ma" (hc ^. #hRates) utcs,
          scannerChart (hc ^. #hN) (hc ^. #hRates) (scan . ma) rs
        )
      ),
      ( "std",
        ( tsRatesHud "std" (hc ^. #hRates) utcs,
          scannerChart (hc ^. #hN) (hc ^. #hRates) (scan . std) rs
        )
      ),
      ( "lastma",
        histChart
          "last ma"
          (Just $ show <$> (hc ^. #hRates))
          (Range (-0.01) 0.01)
          20
          (lastma rs)
      ),
      ( "laststd",
        histChart
          "last ma"
          (Just $ show <$> (hc ^. #hRates))
          (Range (-0.01) 0.01)
          20
          (laststd rs)
      ),
      ( "betaMa2X",
        ( tsRatesHud "beta ma measure" (hc ^. #hRates) utcs,
          scannerChart
            (hc ^. #hN)
            (hc ^. #hRates)
            (betama2x (hc ^. #hRateBetaMa2X))
            rs
        )
      ),
      ( "betaStd2X",
        ( tsRatesHud "beta std to ma" (hc ^. #hRates) utcs,
          scannerChart
            (hc ^. #hN)
            (hc ^. #hRates)
            (betastd2x (hc ^. #hRateBetaStd2X))
            rs
        )
      ),
      ( "model1JustBeta",
        ( tsModel1BetaHud "model1 just beta" utcs,
          scannersChart (hc ^. #hN) (hc ^. #hRate) (model1fit m1hc) rs
        )
      ),
      ( "quantiles",
        quantileChart
          "quantiles"
          (quantileNames $ hc ^. #hQuantiles)
          ( blendMidLineStyles
              (length qss)
              0.001
              (Colour 0.7 0.1 0.3 0.2, Colour 0.1 0.4 0.8 1)
          )
          (tsAxes utcs)
          qss
      ),
      ("quantile histogram", quantileHistChart "quantile histogram" Nothing (hc ^. #hQuantiles) qs),
      ( "digitise",
        digitChart "digitized return" utcs digits
          & second (<> stdLineChart 0.01 palette1 (scan ((\x y -> [x, y]) <$> ma 0.95 <*> std 0.95) digits))
      ),
      ("digit pixel", (mempty, digitPixelChart defaultPixelStyle (defaultPixelLegendOptions "legend!") ("digit pixel", "ma", "std") (quantileNames $ hc ^. #hQuantiles) d1)),
      ( "ma vs std",
        (defaultHudOptions & #hudTitles .~ [defaultTitle "ma vs std"], scatterChart (scat rs))
      )
    ]
  where
    n = hc ^. #hN
    utcs = taker n (fst <$> xs)
    rs = snd <$> xs
    lastma rs = (\r -> fold (ma r) rs) <$> (hc ^. #hRates)
    laststd rs = (\r -> fold (std r) rs) <$> (hc ^. #hRates)
    scat rs = (\r -> scan (Point <$> ma r <*> std r) rs) <$> (hc ^. #hRates)
    betama2x br r rs = scan (beta1 (ma br)) $ zip rs (scan (ma r >>> delay [0]) rs)
    betastd2x br r rs = scan (beta1 (ma br)) $ zip rs (scan (std r >>> delay [0]) rs)
    digitscan rs = taker 1000 $ scan (dscan (hc ^. #hQuantiles) (hc ^. #hQuantileRate)) rs
    digits = fromIntegral <$> digitscan rs
    qss = scan (qscan (hc ^. #hQuantiles) (hc ^. #hQuantileRate)) rs
    qs = fold (qscan (hc ^. #hQuantiles) (hc ^. #hQuantileRate)) rs
    d1 =
      scan
        ( (,)
            <$> (ma 0.95 >>> dscan (hc ^. #hQuantiles) (hc ^. #hQuantileRate))
            <*> (std 0.95 >>> dscan (hc ^. #hQuantiles) (hc ^. #hQuantileRate))
        )
        rs

-- * scans

-- | scan of quantiles
qscan :: [Double] -> Double -> Mealy Double [Double]
qscan qs r = quantiles r qs

-- | scan of digits
dscan :: [Double] -> Double -> Mealy Double Int
dscan qs r = digitize r qs

tsModel1BetaHud :: Text -> [UTCTime] -> HudOptions
tsModel1BetaHud title ds =
  defaultHudOptions
    & #hudTitles
      .~ [defaultTitle title & #style . #size .~ 0.08]
    & #hudLegend ?~ leg
    & #hudAxes .~ tsAxes ds
  where
    leg =
      ( defaultLegendOptions
          & #ltext . #size .~ 0.3
          & #lplace .~ PlaceBottom
          & #legendFrame ?~ RectStyle 0.02 (palette1 !! 5) white,
        zipWith
          (\a r -> (LineA a, r))
          (stdLines 0.005)
          ["alpha", "ma2X", "std2X", "ma2S", "std2S"]
      )

-- * modelling

-- | take a decay rate for regression fits, and a list of decay rates for the underyling stats, and fit model 1
model1fit :: Model1HistoryConfig -> Double -> [Double] -> [[Double]]
model1fit m1hc r rs =
  fmap
    ( fmap
        ( min (m1hc ^. #m1Max)
            . max (m1hc ^. #m1Min)
        )
    )
    . scan (model1BetaFit (m1hc ^. #m1RegRate) r)
    $ rs

model1BetaFit' :: Double -> Double -> Mealy Double (Array '[4] Double, Double)
model1BetaFit' br r =
  reg br
    <<< (\a b c d e -> (fromList [a, b, c, d], e))
    <$> (delay [0] <<< ma r)
    <*> (delay [0] <<< std r)
    <*> (delay [0] <<< ma r <<< std r)
    <*> (delay [0] <<< std r <<< std r)
    <*> id

model1BetaFit :: Double -> Double -> Mealy Double [Double]
model1BetaFit br r = (\(x, y) -> y : toList x) <$> model1BetaFit' br r

-- |
--
-- > let ts = stats defaultHistoryConfig (first (`UTCTime` 0) <$> rs)
stats :: HistoryConfig -> [(UTCTime, Double)] -> [[Text]]
stats cfg rs =
  [ ["Pre-load from", maybe "" ftime (head ds)],
    ["Start Date", maybe "" ftime (head $ taker n ds)],
    ["End Date", maybe "" ftime (head (reverse ds))],
    ["n", show n],
    ["nAll", show (nAll cfg)],
    ["daily average return", formatN (FormatPercent (Just 3)) (fold (ma 1) (taker n xs))],
    ["average return pa", formatN (FormatPercent (Just 3)) (250 * fold (ma 1) (taker n xs))],
    ["daily average sd return", formatN (FormatPercent (Just 3)) (fold (std 1) (taker n xs))],
    ["average sd return pa", formatN (FormatPercent (Just 3)) (sqrt 250.0 * fold (std 1) (taker n xs))]
  ]
  where
    n = cfg ^. #hN
    ftime = pack . formatTime defaultTimeLocale (iso8601DateFormat Nothing)
    xs = snd <$> rs
    ds = fst <$> rs
