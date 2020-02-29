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

module Main where

import Chart
import qualified Control.Foldl as L
import Control.Lens hiding ((:>), (<&>), Unwrapped, Wrapped)
import Control.Monad
import Data.Generics.Labels ()
import Data.Maybe
import qualified Data.Text as Text
import Data.Yahoo
import Online
import Readme.Lhs
import Protolude
import Run.Charts
import Run.Types
import qualified Turtle as T
import Data.Time.Format

fore :: (Floating a) => a -> a -> [a] -> [(a, a)]
fore r1 r0 xs =
  L.scan
    ( (\b o a r -> (a + b * o, r)) <$> beta (ma r1)
        <*> L.premap snd (ma 0.00001)
        <*> alpha (ma r1)
        <*> L.premap fst (ma 0.00001)
    )
    $ drop 2
    $ zip xs (L.scan (ma r0) xs)

main :: IO ()
main = run defaultRunConfig defaultRunStyleConfig

run :: RunConfig -> RunStyleConfig -> IO ()
run c sc = do
  T.mktree (T.fromText $ sc ^. #runName)
  let dir fn = Text.unpack $ "runs/" <> (sc ^. #runName) <> "/" <> fn
  let cs = sc ^. #chartSize
  let n = c ^. #n
  let nall = c ^. #nAll
  let qsNames = (<>"th") . comma 0 . (100*) <$> (c ^. #qs)

  ydata <- getYahoo nall
  let dcs = getdc ydata
  let xs' = snd <$> dcs
  let xs = taker n xs'
  let ds = fst <$> dcs
  let ds' = taker n ds

  writeChartSvg (dir "ma.svg") cs False $ onlineChart dcs c "moving average return" (L.scan . ma)
  writeChartSvg (dir "std.svg") cs False $ onlineChart dcs c "moving std dev return" (L.scan . std)
  writeChartSvg (dir "mabeta.svg") cs False $
    onlineChart
      dcs
      c
      "ma beta"
      (\r xs -> drop 2 $ L.scan (beta (ma r)) $ drop 2 $ zip xs (L.scan (ma r) xs))
  writeChartSvg (dir "maalpha.svg") cs False $
    onlineChart
      dcs
      c
      "ma alpha"
      (\r xs -> drop 2 $ L.scan (alpha (ma r)) $ drop 2 $ zip xs (L.scan (ma r) xs))
  writeChartSvg (dir "stdbeta.svg") cs False $
    onlineChart
      dcs
      c
      "std beta"
      (\r xs -> drop 2 $ L.scan (beta (ma r)) $ drop 2 $ zip xs (L.scan (std r) xs))
  writeChartSvg (dir "stdalpha.svg") cs False $
    onlineChart
      dcs
      c
      "std alpha"
      (\r xs -> drop 2 $ L.scan (alpha (ma r)) $ drop 2 $ zip xs (L.scan (std r) xs))

  -- return quantile
  let qs = taker n $ drop 1 $ L.scan (onlineQuantiles (c ^. #qsRate) (c ^. #qs)) xs'
  writeChartSvg
    (dir "quantiles.svg")
    cs
    False
    (quantileChart "moving quantiles" qsNames (zip ds' qs))

  -- (final) return quantile histogram
  let lastqs = head $ reverse qs
  maybe (pure ())
    (writeChartSvg
    (dir "qhist.svg")
    cs
    False .
    quantileHistChart "most recent moving histogram" Nothing (c ^. #qs))
    lastqs

  -- digitize
  let dgs = taker n $ L.scan (onlineDigitize (c ^. #qsRate) (c ^. #qs)) xs'
  writeChartSvg (dir "digitalise.svg") cs False (digitChart "digitalised return" qsNames (zip ds' dgs))

  -- quantile histogram
  writeChartSvg (dir "digitcheck.svg") cs False (histChart "digit count" (Just qsNames) (Range 0 (fromIntegral $ length (c ^. #qs))) (length (c ^. #qs)) (fromIntegral <$> dgs))

  -- value histogram
  writeChartSvg (dir "histogram.svg") cs False (histChart "daily return (all series)" Nothing (c ^. #histRange) (c ^. #histGrain) xs)

  -- std vs ma
  writeChartSvg (dir "stdvsma.svg") cs False $
    scatterChart
      ("std vs ma", "std", "ma")
      (filter (\(Point _ y) -> y > -6e-3) (taker n $ drop 2 $ L.scan
       (Point <$> std (fst $ c ^. #versus) <*> ma (snd $ c ^. #versus)) xs))

  -- digitized std vs ma
  let dma = taker n $ L.scan (onlineDigitize (c ^. #qsRate) (c ^. #qs)) (drop 2 $ L.scan (ma 0.99) xs')
  let dstd = taker n $ L.scan (onlineDigitize (c ^. #qsRate) (c ^. #qs)) (drop 2 $ L.scan (std 0.95) xs')
  writeChartSvg (dir "digitpixel.svg") cs False $
    digitPixelChart defaultPixelStyle (defaultPixelLegendOptions "count")
    ("std versus ma", "ma quantiles", "std quantiles") ("0th":qsNames)
    (zip dma dstd)

  -- forecast error histogram
  let fa' = fore (fst $ c ^. #foreRate) (snd $ c ^. #foreRate) xs'
  let fa = taker n fa'
  writeChartSvg (dir "histogramf.svg") cs False (histChart "forecast error" Nothing (c ^. #histRange) (c ^. #histGrain) (fmap (\(f, a) -> a - f) fa))

  -- forecast vs actual scatter
  writeChartSvg (dir "scatterf.svg") cs False $
    scatterChart ("forecast versus actual", "forecast", "actual") (uncurry Point <$> fa)

  -- digitized forecast vs actual
  let df = drop 1 $ taker n $ L.scan (onlineDigitize (c ^. #qsRate) (c ^. #qs)) (fst <$> fa')
  let dxs = taker n $ L.scan (onlineDigitize (c ^. #qsRate) (c ^. #qs)) xs'
  writeChartSvg (dir "digitf.svg") cs False $
    digitPixelChart defaultPixelStyle (defaultPixelLegendOptions "count" & #ploLegendOptions . #lsize .~ 0.4)
    ("forecasted versus actual", "actual", "forecast") ("0th":qsNames)
    (zip dxs df)

  let ftime = Text.pack . formatTime defaultTimeLocale (iso8601DateFormat Nothing)

  void
    $ runOutput
      ("other/online_.md", GitHubMarkdown)
      (dir "index.html", Html)
    $ do
      output "run" $ Fence $ Text.unlines
        [ show sc
        , show c
        ]
      output "stats" $ Native $
        (: [])
          ( table
              mempty
              []
              [AlignLeft, AlignRight]
              []
              [ ["Start Date", fromMaybe "" $ ftime <$> head ds'],
                ["End Date", fromMaybe "" $ ftime <$> head (reverse ds')],
                ["n", show n],
                ["daily average return", formatN (FormatPercent 3) (L.fold (ma 1) xs)],
                ["average return pa", formatN (FormatPercent 3) (250 * L.fold (ma 1) xs)],
                ["daily average sd return", formatN (FormatPercent 3) (L.fold (std 1) xs)],
                ["average sd return pa", formatN (FormatPercent 3) ((250.0 ** 0.5) * L.fold (std 1) xs)]
              ]
          )
      output "forecast" $ Native $
        (: [])
          ( table
              mempty
              []
              [AlignLeft, AlignRight]
              []
              [ ["daily average forecast", formatN (FormatPercent 3) (L.fold (ma 1) (fst <$> fa))],
                ["daily average sd forecast", formatN (FormatPercent 3) (L.fold (std 1) (fst <$> fa))]
              ]
          )
