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
main = run (defaultRunConfig & #n .~ 10000 & #nAll .~ 20000 & #dir .~ "/Users/tonyday/site/content/posts/online-market/" & #name .~ "regression")

svgName :: RunConfig -> FilePath -> FilePath
svgName c f = view #dir c <> view #name c <> "/" <> f

run :: RunConfig -> IO ()
run c = do
  rs' <- runReturn (c ^. #nAll)
  let nAll = min (c ^. #nAll) (length rs')
  let rs = take nAll rs'
  let n = min (c ^. #n) (length rs)
  onlineChart rs c (svgName c "betama.svg") "moving average return" (L.scan . ma)

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
