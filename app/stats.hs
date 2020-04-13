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
import Data.Time.Format
import Data.Yahoo
import Online
import NumHask.Prelude
import Readme.Lhs
import Run.Charts
import Run.Types
import qualified Turtle as T

fore :: (Floating a, Multiplicative a, Additive a) => a -> a -> [a] -> [(a, a)]
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
main = run defaultRunConfig (defaultRunStyleConfig & #runName .~ "stats")

run :: RunConfig -> RunStyleConfig -> IO ()
run c sc = do
  T.mktree (T.fromText $ sc ^. #runName)
  T.mktree (T.fromText $ (sc ^. #runName <> "/svg"))
  let dir fn = Text.unpack $ (sc ^. #runName) <> "/" <> fn
  let n = c ^. #n
  let nall = c ^. #nAll
  let qsNames = (<> "th") . comma 0 . (100 *) <$> (c ^. #qs)
  dcs <- runReturn nall
  let xs' = snd <$> dcs
  let xs = taker n xs'
  let ds = fst <$> dcs
  let ds' = taker n ds
  writeCharts (dir "svg/ma.svg") $ onlineChart dcs c "moving average return" (L.scan . ma)
  writeCharts (dir "svg/std.svg") $ onlineChart dcs c "moving std dev return" (L.scan . std)
  writeCharts (dir "svg/mabeta.svg") $ onlineChart dcs c "ma beta" (\r xs -> drop 2 $ L.scan (beta (ma r)) $ drop 2 $ zip xs (L.scan (ma r) xs))
  writeCharts (dir "svg/maalpha.svg") $ onlineChart dcs c "ma alpha" (\r xs -> drop 2 $ L.scan (alpha (ma r)) $ drop 2 $ zip xs (L.scan (ma r) xs))
  writeCharts (dir "svg/stdbeta.svg") $ onlineChart dcs c "std beta" (\r xs -> drop 2 $ L.scan (beta (ma r)) $ drop 2 $ zip xs (L.scan (std r) xs))
  writeCharts (dir "svg/stdalpha.svg") $ onlineChart dcs c "std alpha" (\r xs -> drop 2 $ L.scan (alpha (ma r)) $ drop 2 $ zip xs (L.scan (std r) xs))
  -- return quantile
  let qs = taker n $ drop 1 $ L.scan (onlineQuantiles (c ^. #qsRate) (c ^. #qs)) xs'
  writeCharts (dir "svg/quantiles.svg") (quantileChart "moving quantiles" qsNames (zip ds' qs))
  -- (final) return quantile histogram
  let lastqs = head $ reverse qs
  maybe (pure ()) ( writeCharts (dir "svg/qhist.svg") . quantileHistChart "most recent moving histogram" Nothing (c ^. #qs) ) lastqs
  -- digitize
  let dgs = taker n $ L.scan (onlineDigitize (c ^. #qsRate) (c ^. #qs)) xs'
  writeCharts (dir "svg/digitalise.svg") (digitChart "digitalised return" qsNames (zip ds' dgs))
  -- quantile histogram
  writeCharts (dir "svg/digitcheck.svg") (histChart "digit count" (Just qsNames) (Range 0 (fromIntegral $ length (c ^. #qs))) (length (c ^. #qs)) (fromIntegral <$> dgs))
  -- value histogram
  writeCharts (dir "svg/histogram.svg") (histChart "daily return (all series)" Nothing (c ^. #histRange) (c ^. #histGrain) xs)
  -- std vs ma
  writeCharts (dir "svg/stdvsma.svg") $
    scatterChart
      ( filter
          (\(Point _ y) -> y > -6e-3)
          ( taker n $ drop 2 $
              L.scan
                (Point <$> std (fst $ c ^. #versus) <*> ma (snd $ c ^. #versus))
                xs
          )
      )
  -- digitized std vs ma
  let dma = taker n $ L.scan (onlineDigitize (c ^. #qsRate) (c ^. #qs)) (drop 2 $ L.scan (ma 0.99) xs')
  let dstd = taker n $ L.scan (onlineDigitize (c ^. #qsRate) (c ^. #qs)) (drop 2 $ L.scan (std 0.95) xs')
  writeCharts (dir "svg/digitpixel.svg") $
    digitPixelChart
      defaultPixelStyle
      (defaultPixelLegendOptions "count")
      ("std versus ma", "ma quantiles", "std quantiles")
      ("0th" : qsNames)
      (zip dma dstd)
  -- forecast error histogram
  let fa' = fore (fst $ c ^. #foreRate) (snd $ c ^. #foreRate) xs'
  let fa = taker n fa'
  writeCharts (dir "svg/histogramf.svg") (histChart "forecast error" Nothing (c ^. #histRange) (c ^. #histGrain) (fmap (\(f, a) -> a - f) fa))
  -- forecast vs actual scatter
  writeCharts (dir "svg/scatterf.svg") $
    scatterChart (uncurry Point <$> fa)
  -- digitized forecast vs actual
  let df = drop 1 $ taker n $ L.scan (onlineDigitize (c ^. #qsRate) (c ^. #qs)) (fst <$> fa')
  let dxs = taker n $ L.scan (onlineDigitize (c ^. #qsRate) (c ^. #qs)) xs'
  writeCharts (dir "svg/digitf.svg") $
    digitPixelChart
      defaultPixelStyle
      (defaultPixelLegendOptions "count" & #ploLegendOptions . #lsize .~ 0.4)
      ("forecasted versus actual", "actual", "forecast")
      ("0th" : qsNames)
      (zip dxs df)
  let ftime = Text.pack . formatTime defaultTimeLocale (iso8601DateFormat Nothing)
  void
    $ runOutput
      ("other/online-market_.md", GitHubMarkdown)
      (dir "online-market.md", GitHubMarkdown)
    $ do
      output "run" $ Fence $
        Text.unlines
          [ show sc,
            show c
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
