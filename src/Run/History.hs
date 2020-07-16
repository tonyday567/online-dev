{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Run.History where

import Chart
import Control.Category ((>>>), (<<<))
import Control.Lens hiding ((:>), (<&>), Unwrapped, Wrapped)
import Data.Generics.Labels ()
import qualified Data.Text as Text
import Data.Yahoo
import NumHask.Prelude
import qualified Data.HashMap.Strict as HashMap
import Web.Rep
import Run.Types
import Data.Time
import Data.Mealy
import Data.Quantiles
import Data.List ((!!), transpose)
import NumHask.Array.Fixed

-- scatterChart
-- histChart
-- quantileHistChart
-- digitPixelChart
-- statistics table
{-
  let ftime = Text.pack . formatTime defaultTimeLocale (iso8601DateFormat Nothing)
  let output' = do
        output "run" $ Fence $
          Text.unlines
            [ toStrict (pShowNoColor c)
            ]
        output "stats" $ Native $
          (: [])
            ( table
                mempty
                []
                [AlignLeft, AlignRight]
                []
                [ ["Pre-load from", maybe "" (ftime . fst) (head (taker nAll rs))],
                  ["Start Date", maybe "" (ftime . fst) (head $ taker n rs)],
                  ["End Date", maybe "" (ftime . fst) (head (reverse rs))],
                  ["n", show n],
                  ["nAll", show nAll],
                  ["daily average return", formatN (FormatPercent 3) (L.fold (ma 1) (taker n $ snd <$> rs))],
                  ["average return pa", formatN (FormatPercent 3) (250 * L.fold (ma 1) (taker n $ snd <$> rs))],
                  ["daily average sd return", formatN (FormatPercent 3) (L.fold (std 1) (taker n $ snd <$> rs))],
                  ["average sd return pa", formatN (FormatPercent 3) (sqrt 250.0 * L.fold (std 1) (taker n $ snd <$> rs))]
                ]
            )

-}

data HistoryConfig = HistoryConfig
  { hN :: Int,
    hRunup :: Int,
    hRate :: Double,
    hRates :: [Double],
    hRateBetaMa2X :: Double,
    hRateBetaStd2X :: Double,
    hCharts :: [Text],
    hQuantiles :: [Double],
    hQuantileRate :: Double
  } deriving (Eq, Show, Generic)

defaultHistoryConfig :: HistoryConfig
defaultHistoryConfig = HistoryConfig 10000 2000 0.99 [0.99, 0.95] 0.99 0.99 ["ma", "std", "betama"] defaultQuantiles 0.99

makeReturns :: HistoryConfig -> IO [(Day, Double)]
makeReturns c = do
  makeReturnSeries ((c ^. #hN) + (c ^. #hRunup))

repHistoryConfig :: (Monad m) => [Text] -> HistoryConfig -> SharedRep m HistoryConfig
repHistoryConfig allItems cfg = bimap hmap HistoryConfig a <<*>> b <<*>> c <<*>> d <<*>> e <<*>> f <<*>> g <<*>> h <<*>> i
  where
    a = either (const (view #hN cfg)) id <$>
      readTextbox (Just "n") (view #hN cfg)
    b = either (const (view #hRunup cfg)) id <$>
      readTextbox (Just "runup") (view #hRunup cfg)
    c = either (const (view #hRate cfg)) id <$>
      readTextbox (Just "rate") (view #hRate cfg)
    d = either (const (view #hRates cfg)) id <$>
      readTextbox (Just "rates") (view #hRates cfg)
    e = either (const (view #hRateBetaMa2X cfg)) id <$>
      readTextbox (Just "rate betaMa2X") (view #hRateBetaMa2X cfg)
    f = either (const (view #hRateBetaStd2X cfg)) id <$>
      readTextbox (Just "rate betaStd2X") (view #hRateBetaStd2X cfg)
    g = repItemsSelect (view #hCharts cfg) allItems
    h = either (const (view #hQuantiles cfg)) id <$>
      readTextbox (Just "quantiles") (view #hQuantiles cfg)
    i = either (const (view #hQuantileRate cfg)) id <$>
      readTextbox (Just "quantile rate") (view #hQuantileRate cfg)
    hmap a b c d e f g h i = a <> b <> c <> d <> e <> f <> g <> h <> i

data Model1HistoryConfig = Model1HistoryConfig
  { m1Min :: Double,
    m1Max :: Double,
    m1RegRate :: Double
  } deriving (Eq, Show, Generic)

defaultModel1HistoryConfig :: Model1HistoryConfig
defaultModel1HistoryConfig = Model1HistoryConfig (-10) 10 0.999

repModel1HistoryConfig :: (Monad m) => Model1HistoryConfig -> SharedRep m Model1HistoryConfig
repModel1HistoryConfig cfg = bimap hmap Model1HistoryConfig a <<*>> b <<*>> c
  where
    a = either (const (cfg ^. #m1Min)) id <$>
      readTextbox (Just "model1 min beta") (cfg ^. #m1Min)
    b = either (const (cfg ^. #m1Max)) id <$>
      readTextbox (Just "model1 max beta") (cfg ^. #m1Max)
    c = either (const (cfg ^. #m1RegRate)) id <$>
      readTextbox (Just "model1 regression beta") (cfg ^. #m1RegRate)
    hmap a b c = a <> b <> c

historyChartNames :: [Text]
historyChartNames = ["ma", "std", "betaMa2X", "betaStd2X", "model1JustBeta", "quantiles", "digitise"]

historyCharts :: SvgOptions -> HistoryConfig -> Model1HistoryConfig -> [(Day,Double)] -> HashMap.HashMap Text Text
historyCharts svgo hc m1hc xs = HashMap.fromList
  [ ("ma",
      renderHudOptionsChart svgo
      (tsHud (hc ^. #hN) (hc ^. #hRates) "ma" ds)
      []
      (tsZeroChart (hc ^. #hN) (hc ^. #hRates) (\r -> scan (ma r)) rs
      )),
    ("std",
      renderHudOptionsChart svgo
      (tsHud (hc ^. #hN) (hc ^. #hRates) "std" ds)
      []
      (tsZeroChart (hc ^. #hN) (hc ^. #hRates) (\r -> scan (std r)) rs
      )),
    ("betaMa2X",
      renderHudOptionsChart svgo
      (tsHud (hc ^. #hN) (hc ^. #hRates) "beta ma measure" ds)
      []
      (tsZeroChart (hc ^. #hN) (hc ^. #hRates) (betama2x (hc ^. #hRateBetaMa2X)) rs
      )),
    ("betaStd2X",
      renderHudOptionsChart svgo
      (tsHud (hc ^. #hN) (hc ^. #hRates) "beta std to ma" ds)
      []
      (tsZeroChart (hc ^. #hN) (hc ^. #hRates) (betastd2x (hc ^. #hRateBetaStd2X)) rs
      )),
    ("model1JustBeta",
      renderHudOptionsChart svgo
      (tsModel1BetaHud (hc ^. #hN) "model1 just beta" ds)
      []
      (tsCharts (hc ^. #hN) (hc ^. #hRate) model1fit rs)),
    ("quantiles",
      quantileChart svgo "quantiles" (quantileNames $ hc ^. #hQuantiles) (zip ds (qscan (hc ^. #hQuantileRate) rs))
    ),
    ("digitise",
     digitChart svgo "digitized return" (quantileNames (hc ^. #hQuantiles))
      (zip ds (digitscan rs))
    )
  ]
  where
    ds = fst <$> xs
    rs = snd <$> xs
    betama2x br r rs = scan (beta1 (ma br)) $ zip rs (scan (ma r >>> delay [0]) rs)
    betastd2x br r rs = scan (beta1 (ma br)) $ zip rs (scan (std r >>> delay [0]) rs)
    model1fit r rs = fmap (fmap ((min (m1hc ^. #m1Max)) . (max (m1hc ^. #m1Min)))) . scan (model1BetaFit (m1hc ^. #m1RegRate) r) $ rs
    qscan r rs = scan (fromFoldl (onlineQuantiles r (hc ^. #hQuantiles))) rs
    digitscan rs = taker 1000 $ scan (fromFoldl $ onlineDigitize (hc ^. #hQuantileRate) (hc ^. #hQuantiles)) rs

tsrs :: Int -> [Double] -> (Double -> [a] -> [Double]) -> [a] -> [[Point Double]]
tsrs n rs rscan xs =
  zipWith Point [0 ..]
    . taker n
    . (\r -> rscan r xs)
    <$> rs

tsZeroChart :: Int -> [Double] -> (Double -> [a] -> [Double]) -> [a] -> [Chart Double]
tsZeroChart n rs rscan xs = zipWith (\l c -> Chart (LineA l) c)
  tsZeroLineStyle (zeroLine : (fmap SpotPoint <$> tsrs n rs rscan xs))
  where
    zeroLine = SpotPoint <$> [Point lx 0, Point ux 0]
    (Rect lx ux _ _) = space1 $ mconcat (tsrs n rs rscan xs)

tsZeroLineStyle :: [LineStyle]
tsZeroLineStyle = zipWith (\c w -> defaultLineStyle & #color .~ c & #width .~ w) palette1 (0.001 : repeat 0.005)

tsHud :: Int -> [Double] -> Text -> [Day] -> HudOptions
tsHud n rs title ds =
  defaultHudOptions
    & #hudTitles
    .~ [defaultTitle title & #style . #size .~ 0.08]
    & #hudLegend
    .~ Just
      ( defaultLegendOptions
          & #ltext . #size .~ 0.3
          & #lplace .~ PlaceBottom
          & #legendFrame .~ Just (RectStyle 0.02 (palette1 !! 5) white),
        zipWith
          (\a r -> (LineA a, ("rate = " <>) . Text.pack . show $ r))
          (drop 1 tsZeroLineStyle)
          rs)
    & #hudAxes
    .~ [ defaultAxisOptions
           & #atick . #tstyle .~ TickRound (FormatPercent 0) 6 TickExtend
           & #place .~ PlaceLeft,
         defaultAxisOptions & #atick . #tstyle
           .~ TickPlaced
             ( first fromIntegral
                 <$> makeTickDates PosIncludeBoundaries Nothing 8 ((`UTCTime` 0) <$> taker n ds)
             )
       ]

model1BetaFit' :: Double -> Double -> Mealy Double (Array '[4] Double, Double)
model1BetaFit' br r =
  reg br <<<
  (\a b c d e -> (fromList [a,b,c,d], e)) <$>
  (delay [0] <<< ma r) <*>
  (delay [0] <<< std r) <*>
  (delay [0] <<< ma r <<< std r) <*>
  (delay [0] <<< std r <<< std r) <*>
  id

model1BetaFit :: Double -> Double -> Mealy Double [Double]
model1BetaFit br r = (\(x,y) -> y:toList x) <$> model1BetaFit' br r

tsCharts :: Int -> Double -> (Double -> [a] -> [[Double]]) -> [a] -> [Chart Double]
tsCharts n r rscan xs = zipWith (\l c -> Chart (LineA l) c)
  tsZeroLineStyle (zeroLine : (fmap SpotPoint <$> tsr n r rscan xs))
  where
    zeroLine = SpotPoint <$> [Point lx 0, Point ux 0]
    (Rect lx ux _ _) = space1 $ mconcat (tsr n r rscan xs)

tsr :: Int -> Double -> (Double -> [a] -> [[Double]]) -> [a] -> [[Point Double]]
tsr n r rscan xs = zipWith Point [0 ..] . taker n <$> (Data.List.transpose $ rscan r xs)

tsModel1BetaHud :: Int -> Text -> [Day] -> HudOptions
tsModel1BetaHud n title ds =
  defaultHudOptions
    & #hudTitles
    .~ [defaultTitle title & #style . #size .~ 0.08]
    & #hudLegend
    .~ Just
      ( defaultLegendOptions
          & #ltext . #size .~ 0.3
          & #lplace .~ PlaceBottom
          & #legendFrame .~ Just (RectStyle 0.02 (palette1 !! 5) white),
        zipWith
          (\a r -> (LineA a, r))
          (drop 1 tsZeroLineStyle)
          ["alpha", "ma2X", "std2X", "ma2S", "std2S"])
    & #hudAxes
    .~ [ defaultAxisOptions
           & #atick . #tstyle .~ TickRound (FormatPercent 0) 6 TickExtend
           & #place .~ PlaceLeft,
         defaultAxisOptions & #atick . #tstyle
           .~ TickPlaced
             ( first fromIntegral
                 <$> makeTickDates PosIncludeBoundaries Nothing 8 ((`UTCTime` 0) <$> taker n ds)
             )
       ]

testQ :: IO ()
testQ = do
  let hc = defaultHistoryConfig
  xs <- makeReturns defaultHistoryConfig
  let rs = taker 1000 $ snd <$> xs
  let ds = taker 1000 $ fst <$> xs
  let dgs rs = scan (fromFoldl $ onlineDigitize (hc ^. #hQuantileRate) (hc ^. #hQuantiles)) rs
  let c = digitChart defaultSvgOptions "digitized return" (quantileNames (hc ^. #hQuantiles)) (zip ds (dgs rs))
  writeFile "other/scratch.svg" c

