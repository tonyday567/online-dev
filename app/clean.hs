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
import Stats

main :: IO ()
main = run defaultRunConfig

dir :: RunConfig -> FilePath -> FilePath
dir c fp = c ^. #name <> "/" <> fp

run :: RunConfig -> IO ()
run c = do
  rs <- runReturn (c ^. #nAll)
  onlineChart rs c (dir c "ma.svg") "moving average return" (L.scan . ma)
  onlineChart rs c (dir c "std.svg") "moving standard deviation" (L.scan . std)
  onlineChart rs c (dir c "mabeta.svg") "beta of moving average"
    (\r xs -> drop 2 $ L.scan (beta (ma r)) $ drop 2 $ zip xs (L.scan (ma r) xs))
  onlineChart rs c (dir c "maalpha.svg") "alpha of moving average"
    (\r xs -> drop 2 $ L.scan (alpha (ma r)) $ drop 2 $ zip xs (L.scan (ma r) xs))
  onlineChart rs c (dir c "stdbeta.svg") "standard deviation beta"
    (\r xs -> drop 2 $ L.scan (beta (ma r)) $ drop 2 $ zip xs (L.scan (std r) xs))
  onlineChart rs c (dir c "stdalpha.svg") "standard deviation alpha"
    (\r xs -> drop 2 $ L.scan (alpha (ma r)) $ drop 2 $ zip xs (L.scan (std r) xs))

  let qsNames = (<> "th") . comma 0 . (100 *) <$> (c ^. #qs)
  let qs = taker (c ^. #n) $ drop 1 $ L.scan (onlineQuantiles (c ^. #qsRate) (c ^. #qs)) (snd <$> rs)
  quantileChart "moving quantiles" qsNames (zip (fst <$> rs) qs) (dir c "quantiles.svg")

  -- (final) return quantile histogram
  let lastqs = head $ reverse qs
  fromMaybe (pure ()) $ (\x -> quantileHistChart "most recent moving histogram" Nothing (c ^. #qs) x (dir c "qhist.svg")) <$> lastqs
  -- digitize
  let dgs = taker (c ^. #n) $ L.scan (onlineDigitize (c ^. #qsRate) (c ^. #qs)) (snd <$> rs)
  digitChart "digitalised return" qsNames (zip (fst <$> rs) dgs) (dir c "digitalise.svg")
  -- quantile histogram
  histChart "digit count" (Just qsNames) (Range 0 (fromIntegral $ length (c ^. #qs))) (length (c ^. #qs)) (fromIntegral <$> dgs) (dir c "digitcheck.svg")
  -- value histogram
  histChart "daily return (all series)" Nothing (c ^. #histRange) (c ^. #histGrain) (snd <$> rs) (dir c "histogram.svg")
  -- std vs ma
  scatterChart
      ( filter
          (\(Point _ y) -> y > -6e-3)
          ( taker (c ^. #n) $ drop 2 $
              L.scan
                (Point <$> std (fst $ c ^. #versus) <*> ma (snd $ c ^. #versus))
                (snd <$> rs)
          )
      )
      (dir c "stdvsma.svg")

  -- digitized std vs ma
  let dma = taker (c ^. #n) $ L.scan (onlineDigitize (c ^. #qsRate) (c ^. #qs)) (drop 2 $ L.scan (ma 0.99) (snd <$> rs))
  let dstd = taker (c ^. #n) $ L.scan (onlineDigitize (c ^. #qsRate) (c ^. #qs)) (drop 2 $ L.scan (std 0.95) (snd <$> rs))
  writeCharts (dir c "digitpixel.svg") $
    digitPixelChart
      defaultPixelStyle
      (defaultPixelLegendOptions "count")
      ("std versus ma", "ma quantiles", "std quantiles")
      ("0th" : qsNames)
      (zip dma dstd)

  let ftime = Text.pack . formatTime defaultTimeLocale (iso8601DateFormat Nothing)
  let output' = do
        output "run" $ Fence $
          Text.unlines
          [ show c
          ]
        output "stats" $ Native $
          (: [])
          ( table
              mempty
              []
              [AlignLeft, AlignRight]
              []
              [ ["Start Date", fromMaybe "" $ ftime . fst <$> head rs],
                ["End Date", fromMaybe "" $ ftime . fst <$> head (reverse rs)],
                ["n", show (c ^. #n)],
                ["daily average return", formatN (FormatPercent 3) (L.fold (ma 1) (snd <$> rs))],
                ["average return pa", formatN (FormatPercent 3) (250 * L.fold (ma 1) (snd <$> rs))],
                ["daily average sd return", formatN (FormatPercent 3) (L.fold (std 1) (snd <$> rs))],
                ["average sd return pa", formatN (FormatPercent 3) ((250.0 ** 0.5) * L.fold (std 1) (snd <$> rs))]
              ]
          )
  void
    $ runOutput
      ("other/online-market_.md", GitHubMarkdown)
      (dir c "index.html", Html)
    $ output'
  void
    $ runOutput
      ("other/online-market_.md", GitHubMarkdown)
      (dir c (c ^. #name <> ".md"), GitHubMarkdown)
    $ output'

