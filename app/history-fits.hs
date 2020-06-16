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

module Main where

-- import Chart
import Control.Lens hiding ((:>), (<&>), Unwrapped, Wrapped)
import Data.Generics.Labels ()
-- import qualified Data.Text as Text
-- import Data.Time.Format
import Data.Yahoo
import NumHask.Prelude
-- import Stats
-- import Readme.Lhs
-- import Run
-- import Text.InterpolatedString.Perl6
-- import Text.Pretty.Simple
import Data.Time ( Day(..) )

type Rate = Double

-- sChart :: SvgOptions -> (Double -> Mealy a Double) -> [Double] -> Int -> Text -> [a] -> Text
-- chartOnline :: RunConfig -> (Double -> [Double] -> [Double]) -> [Double] -> [Chart Double]
-- writeOnline :: FilePath -> RunConfig -> (Double -> [Double] -> [Double]) -> [(Day, Double)] -> IO ()
-- quantileChart :: FilePath -> Text -> [Text] -> [(Day, [Double])] -> IO ()
-- digitChart :: FilePath -> Text -> [Text] -> [(Day, Int)] -> IO ()
-- scatterChart :: FilePath -> [Point Double] -> IO ()
-- histChart :: Text -> Maybe [Text] -> Range Double -> Int -> [Double] -> FilePath -> IO ()
-- quantileHistChart :: Text -> Maybe [Text] -> [Double] -> [Double] -> FilePath -> IO ()
-- digitPixelChart :: PixelStyle -> PixelLegendOptions -> (Text, Text, Text) -> [Text] -> [(Int, Int)] -> [Chart Double]

data HistoryConfig = HistoryConfig
  { hN :: Int,
    hRunup :: Int,
    hRates :: [Rate],
    hCharts :: [Text]
  } deriving (Eq, Show, Generic)

defaultHistoryConfig :: HistoryConfig
defaultHistoryConfig = HistoryConfig 10000 2000 [0.99, 0.95] ["ma", "std"]

main :: IO ()
main  = pure ()

makeReturns :: HistoryConfig -> IO [(Day, Double)]
makeReturns c = do
  runReturn (c ^. #hN)

-- charts :: Map Text ([Chart])

{-
run :: RunConfig -> IO ()
run c = do
  rs' <- runReturn (c ^. #nAll)
  let nAll = min (c ^. #nAll) (length rs')
  let rs = take nAll rs'
  let n = min (c ^. #n) (length rs)
  let r' = c ^. #betaRate
  let ma' r xs = drop 1 $ L.scan (ma r) xs
  writeOnline "ma.svg" c ma' rs
  let b' r xs = drop 1 $ L.scan (beta (ma r')) $ drop 1 $ zip xs (L.scan (ma r) xs)
  writeOnline "beta.svg" c b' rs
  let qsNames = (<> "th") . comma 0 . (100 *) <$> (c ^. #qs)
  let qs = taker n $ drop 1 $ L.scan (onlineQuantiles (c ^. #qsRate) (c ^. #qs)) (snd <$> rs)
  quantileChart (svgName c "quantiles.svg") "moving quantiles" qsNames (zip (taker n $ fst <$> rs) qs)
  let dgs = taker n $ drop 1 $ L.scan (onlineDigitize (c ^. #qsRate) (c ^. #qs)) (snd <$> rs)
  digitChart (svgName c "digit.svg") "digitalised return" qsNames (zip (drop 1 $ taker n (fst <$> rs)) dgs)
  let bdgs _ xs = drop 1 $ L.scan (beta (ma r')) $ zip (drop 1 xs) ((* 0.1) . fromIntegral <$> dgs)
  scatterChart "compare.svg" $ drop 1 (zipWith Point (bdgs 0.95 (snd <$> rs)) (taker n $ snd <$> rs))
  let ftime = Text.pack . formatTime defaultTimeLocale (iso8601DateFormat Nothing)
  let output' = do
        output "header" $ Native [Plain [Str hugoHeader]]
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
  void $
    runOutput
      ("other/regression.md", GitHubMarkdown)
      (view #dir c <> ((c ^. #name) <> ".md"), GitHubMarkdown)
      output'


-}

