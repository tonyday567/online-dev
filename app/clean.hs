{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Text.InterpolatedString.Perl6
import Text.Pretty.Simple

-- let c = defaultRunConfig & #n .~ 10000 & #nAll .~ 20000 & #dir .~ "/Users/tonyday/site/content/posts/online-market/" & #name .~ "longrun"
-- rs' <- runReturn (c ^. #nAll)

main :: IO ()
main = run (defaultRunConfig & #n .~ 10000 & #nAll .~ 20000 & #dir .~ "/Users/tonyday/site/content/posts/online-market/" & #name .~ "longrun")

svgName :: RunConfig -> FilePath -> FilePath
svgName c f = view #dir c <> view #name c <> "/" <> f

run :: RunConfig -> IO ()
run c = do
  rs' <- runReturn (c ^. #nAll)
  let nAll = min (c ^. #inAll) (length rs')
  let rs = take nAll rs'
  let n = min (c ^. #n) (length rs)
  onlineChart rs c (svgName c "ma.svg") "moving average return" (L.scan . ma)
  onlineChart rs c (svgName c "std.svg") "moving standard deviation" (L.scan . std)
  onlineChart rs c (svgName c "maabs.svg") "moving absolute deviation" (L.scan . absma)
  onlineChart rs' c (svgName c "madbeta.svg") "beta of absma on itself" (\r xs -> drop 2 $ L.scan (beta (absma r)) $ drop 2 $ zip xs (L.scan (absma r) xs))
  onlineChart rs (c & #rates .~ [0.99, 0.9925]) (svgName c "mabeta.svg") "beta of moving average" (\r xs -> drop 2 $ L.scan (beta (ma r)) $ drop 2 $ zip xs (L.scan (ma r) xs))
  onlineChart rs c (svgName c "maalpha.svg") "alpha of moving average"
    (\r xs -> drop 2 $ L.scan (alpha (ma r)) $ drop 2 $ zip xs (L.scan (ma r) xs))
  onlineChart rs c (svgName c "stdbeta.svg") "standard deviation beta"
    (\r xs -> drop 2 $ L.scan (beta (ma r)) $ drop 2 $ zip xs (L.scan (std r) xs))
  onlineChart rs c (svgName c "stdalpha.svg") "standard deviation alpha"
    (\r xs -> drop 2 $ L.scan (alpha (ma r)) $ drop 2 $ zip xs (L.scan (std r) xs))

  let qsNames = (<> "th") . comma 0 . (100 *) <$> (c ^. #qs)
  let qs = taker n $ drop 1 $ L.scan (onlineQuantiles (c ^. #qsRate) (c ^. #qs)) (snd <$> rs)
  quantileChart "moving quantiles" qsNames (zip (fst <$> rs) qs) (svgName c "quantiles.svg")

  -- (final) return quantile histogram
  let lastqs = head $ reverse qs
  fromMaybe (pure ()) $ (\x -> quantileHistChart "most recent moving histogram" Nothing (c ^. #qs) x (svgName c "qhist.svg")) <$> lastqs
  -- digitize
  let dgs = taker n $ L.scan (onlineDigitize (c ^. #qsRate) (c ^. #qs)) (snd <$> rs)
  digitChart "digitalised return" qsNames (zip (fst <$> rs) dgs) (svgName c "digitalise.svg")
  -- quantile histogram
  histChart "digit count" (Just qsNames) (Range 0 (fromIntegral $ length (c ^. #qs))) (length (c ^. #qs)) (fromIntegral <$> dgs) (svgName c "digitcheck.svg")
  -- value histogram
  histChart "daily return (all series)" Nothing (c ^. #histRange) (c ^. #histGrain) (snd <$> rs) (svgName c "histogram.svg")
  -- std vs ma
  scatterChart
      ( filter
          (\(Point _ y) -> y > -6e-3)
          ( taker n $ drop 2 $
              L.scan
                (Point <$> std (fst $ c ^. #versus) <*> ma (snd $ c ^. #versus))
                (snd <$> rs)
          )
      )
      (svgName c "stdvsma.svg")

  -- digitized std vs ma
  let dma = taker n $ L.scan (onlineDigitize (c ^. #qsRate) (c ^. #qs)) (drop 2 $ L.scan (ma 0.99) (snd <$> rs))
  let dstd = taker n $ L.scan (onlineDigitize (c ^. #qsRate) (c ^. #qs)) (drop 2 $ L.scan (std 0.95) (snd <$> rs))
  writeCharts (svgName c "digitpixel.svg") $
    digitPixelChart
      defaultPixelStyle
      (defaultPixelLegendOptions "count")
      ("std versus ma", "ma quantiles", "std quantiles")
      ("0th" : qsNames)
      (zip dma dstd)

  let ftime = Text.pack . formatTime defaultTimeLocale (iso8601DateFormat Nothing)
  let output' = do
        output "header" $ Native $ [Plain [Str hugoHeader]]
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
              [ ["Pre-load from", fromMaybe "" $ ftime . fst <$> (head $ taker nAll rs)],
                ["Start Date", fromMaybe "" $ ftime . fst <$> (head $ taker n rs)],
                ["End Date", fromMaybe "" $ ftime . fst <$> (head (reverse rs))],
                ["n", show n],
                ["nAll", show nAll],
                ["daily average return", formatN (FormatPercent 3) (L.fold (ma 1) (taker n $ snd <$> rs))],
                ["average return pa", formatN (FormatPercent 3) (250 * L.fold (ma 1) (taker n $ snd <$> rs))],
                ["daily average sd return", formatN (FormatPercent 3) (L.fold (std 1) (taker n $ snd <$> rs))],
                ["average sd return pa", formatN (FormatPercent 3) ((250.0 ** 0.5) * L.fold (std 1) (taker n $ snd <$> rs))]
              ]
          )
  void
    $ runOutput
      ("other/online-market_.md", GitHubMarkdown)
      (view #dir c <> ((c ^. #name) <> ".md"), GitHubMarkdown)
    $ output'

hugoHeader :: Text
hugoHeader = [qq|
+++
date = "2020-04-10"
title = "online-market-run"
+++
|]
