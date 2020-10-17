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

module Run.Stats
  ( StatsConfig(..),
    defaultStatsConfig,
    repStatsConfig,
    testStatsCharts,
    testStatsChartsNames,
    Model1(..),
    zeroModel1,
    repModel1,
    model1Charts,
    model1ChartsNames,
  )
where

import Chart
import Chart.Mealy hiding (taker)
import Control.Category ((>>>))
import Control.Lens hiding ((:>), (<&>), Unwrapped, Wrapped)
import Control.Monad
import Data.Generics.Labels ()
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import NumHask.Prelude hiding (fold, asum)
import Data.Mealy
import Data.List ((!!))
import Web.Rep
import Run.Random
import Chart.Various

-- | various configuration details
data StatsConfig = StatsConfig
  { stRates :: [Double],
    stBeta :: Double,
    stBetas :: [Double],
    stStdMaDecay :: Double,
    stMaDepBeta :: Double,
    stMaDepRate :: Double,
    stMaDepBetaRate :: Double
  } deriving (Generic, Eq, Show)

defaultStatsConfig :: StatsConfig
defaultStatsConfig =
  StatsConfig
  [0, 0.01, 0.02]
  0.1
  [0.001, 0.01, 0.02, 0.05, 0.1]
  1
  0.1
  0.01
  0

-- | representation of configuration
repStatsConfig :: (Monad m) => StatsConfig -> SharedRep m StatsConfig
repStatsConfig cfg = bimap hmap StatsConfig rates' <<*>> beta' <<*>> betas' <<*>> stdmadecay' <<*>> madepbeta' <<*>> madeprate' <<*>> madepbetarate'
  where
    rates' = either (const []) id <$>
      readTextbox (Just "rates") (view #stRates cfg)
    beta' = either (const (view #stBeta cfg)) id <$>
      readTextbox (Just "test beta") (view #stBeta cfg)
    betas' = either (const []) id <$>
      readTextbox (Just "test betas") (view #stBetas cfg)
    stdmadecay' = either (const 1) id <$>
      readTextbox (Just "std of ma decay rate") (view #stStdMaDecay cfg)
    madepbeta' = either (const 0.1) id <$>
      readTextbox (Just "beta ma dependency") (view #stMaDepBeta cfg)
    madeprate' = either (const 0.01) id <$>
      readTextbox (Just "decay rate of ma dep") (view #stMaDepRate cfg)
    madepbetarate' = either (const 0.001) id <$>
      readTextbox (Just "rate of beta calc for madep") (view #stMaDepBetaRate cfg)
    hmap rates'' beta'' betas'' stdmadecay'' madepbeta'' madeprate'' madepbetarate'' = rates'' <> beta'' <> betas'' <> stdmadecay'' <> madepbeta'' <> madeprate'' <> madepbetarate''

testStatsCharts :: StatsConfig -> RandomSets -> HashMap.HashMap Text (HudOptions, [Chart Double])
testStatsCharts st rs = HashMap.fromList
    [ ("ex-madep",
        (titlesHud ("beta check: " <> show (view #stMaDepBeta st)) "n" "beta measure",
        betaCheckChart
         (view #stMaDepBeta st)
         (view #stMaDepRate st)
         (view #stMaDepBetaRate st)
         1000
         xs)),
      ("ex-ma",
        (scanHud 0.01 "ma" ((1 -) <$> view #stRates st),
        scanChart ma ((1 -) <$> view #stRates st) 0 (xs <> ((1+) . (2*) <$> xs1)))),
      ("ex-std",
        (scanHud 0.01 "std" ((1 -) <$> view #stRates st),
        scanChart std ((1 -) <$> view #stRates st) 0 (xs <> ((1+) . (2*) <$> xs1)))),
      ("ex-stdma",
        (titlesHud "std of ma" "rate" "std of ma / (0.5*r)**0.5",
        foldScanChart ma (scaledStd (view #stStdMaDecay st)) (view #stRates st) xs))
    ]
  where
    xs = (rs ^. #rs) !! 0
    xs1 = (rs ^. #rs) !! 1

testStatsChartsNames :: [Text]
testStatsChartsNames = ["ex-ma", "ex-std", "ex-stdma", "ex-madep"]

betaCheckChart :: Double -> Double -> Double -> Int -> [Double] -> [Chart Double]
betaCheckChart b r rb d xs =
  [ Chart (LineA defaultLineStyle) $ drop d $ xify' (scan (beta1 (ma (1 - rb))) $ fromList $ drop 100 $ scan (betaCheck b r) xs)
  ]

betaCheck :: Double -> Double -> Mealy Double (Double, Double)
betaCheck b r = (,) <$> xs' <*> ma'
  where
    xs' = depState (\a m -> a + b * m) (ma (1 - r))
    ma' = ma r >>> delay [0]

scaledStd :: (Fractional b, Eq b, ExpField b) => b -> b -> Mealy b b
scaledStd stdr r = (/ sqrt (0.5 * r')) <$> std stdr
  where
    r' = bool r 0.00001 (r==0)

-- | representation of model1
repModel1 :: (Monad m) => Model1 -> SharedRep m Model1
repModel1 m1 = bimap hmap Model1 alphaX' <<*>> alphaS' <<*>> betaMa2X' <<*>> betaMa2S' <<*>> betaStd2X' <<*>> betaStd2S'
  where
    alphaX' = either (const (view #alphaX m1)) id <$>
      readTextbox (Just "alphaX") (view #alphaX m1)
    alphaS' = either (const (view #alphaS m1)) id <$>
      readTextbox (Just "alphaS") (view #alphaS m1)
    betaMa2X' = either (const (view #betaMa2X m1)) id <$>
      readTextbox (Just "betaMa2X") (view #betaMa2X m1)
    betaMa2S' = either (const (view #betaMa2S m1)) id <$>
      readTextbox (Just "betaMa2S") (view #betaMa2S m1)
    betaStd2X' = either (const (view #betaStd2X m1)) id <$>
      readTextbox (Just "betaStd2X") (view #betaStd2X m1)
    betaStd2S' = either (const (view #betaStd2S m1)) id <$>
      readTextbox (Just "betaStd2S") (view #betaStd2S m1)
    hmap alphaX'' alphaS'' betaMa2X'' betaMa2S'' betaStd2X'' betaStd2S'' = alphaX'' <> alphaS'' <> betaMa2X'' <> betaMa2S'' <> betaStd2X'' <> betaStd2S''

model1ChartsNames :: [Text]
model1ChartsNames = ["ex-stats", "ex-model1", "ex-orig", "ex-model1-compare"]

model1Charts :: Model1 -> RandomSets -> HashMap.HashMap Text (HudOptions, [Chart Double])
model1Charts m1 rs =
  HashMap.fromList
    [
      ("ex-model1",
       (titlesHud "model1 random walk: " "" "",
       scanChart (\r -> depModel1 r m1 >>> M id (+) id) [0.01] 0 xs)),
      ("ex-orig",
        (titlesHud "origin random walk: " "" "",
        scanChart (\_ -> M id (+) id) [0.01] 0 xs)),
      ("ex-model1-compare",
       (titlesHud "model1 random walk: " "" ""  &
         #hudLegend ?~
         ( defaultLegendOptions
          & #ltext . #size .~ 0.2
          & #lplace .~ PlaceAbsolute (Point 0.3 (-0.3))
          & #legendFrame ?~ RectStyle 0.02 (palette1 !! 5) white,
           zipWith
           (\a l -> (LineA a, l))
           (stdLines 0.005)
           ["model1", "original"]
         ),
       scanChart (\r -> depModel1 r m1 >>> M id (+) id) [0.01] 0 xs <>
       scanChart (\_ -> M id (+) id) [0.01] 0 xs))
    ]
  where
    xs = (rs ^. #rs) !! 0
