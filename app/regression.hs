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
main = run (defaultRunConfig & #n .~ 10000 & #nAll .~ 20000 & #dir .~ "/Users/tonyday/site/content/posts/online-market/" & #name .~ "regression" & #rates .~ [0.95])

-- | with an online fold for the central tendency, return an (alpha, beta) regression values tuple.
reg :: (Fractional a) => L.Fold a a -> L.Fold (a,a) (a,a)
reg m = (,) <$> alpha m <*> beta m

run :: RunConfig -> IO ()
run c = do
  rs' <- runReturn (c ^. #nAll)
  let nAll = min (c ^. #nAll) (length rs')
  let rs = take nAll rs'
  let n = min (c ^. #n) (length rs)
  let r' = c^. #betaRate
  let ma' r xs = drop 1 $ L.scan (ma r) xs
  writeOnline "ma.svg" c ma' rs
  let b' r xs = drop 1 $ L.scan (beta (ma r')) $ drop 1 $ zip xs (L.scan (ma r) xs)
  writeOnline "beta.svg" c b' rs

  let qsNames = (<> "th") . comma 0 . (100 *) <$> (c ^. #qs)
  let qs = taker n $ drop 1 $ L.scan (onlineQuantiles (c ^. #qsRate) (c ^. #qs)) (snd <$> rs)
  quantileChart (svgName c "quantiles.svg") "moving quantiles" qsNames (zip (taker n $ fst <$> rs) qs)

  let dgs = taker n $ drop 1 $ L.scan (onlineDigitize (c ^. #qsRate) (c ^. #qs)) (snd <$> rs)
  digitChart (svgName c "digit.svg") "digitalised return" qsNames (zip (drop 1 $ taker n $ (fst <$> rs)) dgs)

  let bdgs r xs = drop 1 $ L.scan (beta (ma r')) $ zip (drop 1 xs) ((*0.1) . fromIntegral <$> dgs)
  scatterChart "compare.svg" $ drop 1 (zipWith Point (bdgs 0.95 (snd <$> rs)) (taker n $ snd <$> rs))

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
      ("other/regression.md", GitHubMarkdown)
      (view #dir c <> ((c ^. #name) <> ".md"), GitHubMarkdown)
    $ output'

hugoHeader :: Text
hugoHeader = [qq|
+++
date = "2020-04-10"
title = "regression"
+++
|]
