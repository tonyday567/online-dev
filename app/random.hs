{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import Chart
import qualified Control.Foldl as L
import Control.Lens hiding ((:>), Unwrapped, Wrapped)
import Data.Generics.Labels ()
import Data.Random
import NumHask.Array.Dynamic
import NumHask.Prelude
import Online
import Readme.Lhs
import System.Random.MWC

-- correlation of a time series with it's moving average.
-- x = b * (ma r xs) + a

--
-- sqrt(2) ~ (\r -> (r ** 0.5) * (1/(L.fold (std 1) $ drop 1 $ (L.scan (ma (1-r)) rs)))) r for wide r.

stdOfMa :: [Double] -> Double -> Double
stdOfMa rs r = 1 / L.fold (std 1) (drop 1 $ L.scan (ma (1 - r)) rs)

stdOfMaChart :: FilePath -> [Double] -> [Double] -> IO ()
stdOfMaChart fp rates rs =
  writeHudOptionsChart
    fp
    defaultSvgOptions
    (titlesHud "std of ma" "rate" "sqrt (1/r) * std of ma")
    []
    ( (: []) $
        Chart
          (LineA defaultLineStyle)
          (zipWith SP rates ((\r -> sqrt r * stdOfMa rs r) <$> rates))
    )

-- | the average beta measure given an autocorrelation of the form r = a + b * ma.
-- The random variance in the beta measure (ie assuming beta = 0) is removed.
avBetaMeasure :: (Subtractive b, Fractional b, Multiplicative b) => [b] -> b -> b -> b
avBetaMeasure rs r b =
  ( \x ->
      x
        - L.fold
          (beta (ma 1))
          (zip ((\r -> drop 1 $ zipWith (\o m -> o + 0 * m) rs (L.scan (ma (1 - r)) rs)) 0.001) rs)
  )
    $ L.fold (beta (ma 1))
    $ zip ((\r -> drop 1 $ zipWith (\o m -> o + b * m) rs (L.scan (ma (1 - r)) rs)) r) rs

-- beta measure ~ (1/r) * original beta, if the beta was set at 0.1
-- this breaks down at less than 0.003, so that long ma's will be unstable.
avBetaMeasureChart :: FilePath -> [Double] -> [Double] -> Double -> IO ()
avBetaMeasureChart fp rates rs b =
  writeHudOptionsChart
    fp
    defaultSvgOptions
    (titlesHud "beta measure check" "rate" "average beta * (1/r)")
    []
    ( (: []) $
        Chart
          (LineA defaultLineStyle)
          (zipWith SP rates ((\r -> avBetaMeasure rs r b / r) <$> rates))
    )

betaErrors :: (Divisive c, Subtractive c, Fractional c) => [c] -> [c] -> [c] -> Array c
betaErrors rs rates bs =
  expand
    (\r b -> (1 / (r * b)) * avBetaMeasure rs r b)
    (fromFlatList [length rates] rates)
    (fromFlatList [length bs] bs)

betaErrorScatter :: FilePath -> [Double] -> [Double] -> [Double] -> IO ()
betaErrorScatter fp rs rates bs =
  writeHudOptionsChart
    fp
    defaultSvgOptions
    (titlesHud "beta measure error" "rates" "betas" & hudOptions)
    h0
    c0
  where
    f1 :: (Point Double -> Double) = \(Point x y) ->
      NumHask.Array.Dynamic.index
        (betaErrors rs rates bs)
        [fromIntegral (floor x :: Integer), fromIntegral (floor y :: Integer)]
    (c0, h0) = pixelfl f1 pixelOptions (defaultPixelLegendOptions "")
    pixelOptions =
      defaultPixelOptions
        & #poGrain .~ Point (fromIntegral $ length rates) (fromIntegral $ length bs)
        & #poRange .~ Rect 0 (fromIntegral $ length rates) 0 (fromIntegral $ length bs)
    hudOptions =
      #hudAxes
        .~ [ defaultAxisOptions
               & #atick . #tstyle .~ TickLabels (show <$> rates),
             defaultAxisOptions
               & #place .~ PlaceLeft
               & #atick . #tstyle .~ TickLabels (show <$> bs)
           ]

main :: IO ()
main = do
  let n = 10000
  let rates = [0.001, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2]
  let bs = [0.001, 0.01, 0.02, 0.05, 0.1]
  g <- create
  rs <- rvs g n
  stdOfMaChart "other/stdofma.svg" rates rs
  avBetaMeasureChart "other/avbetameasure.svg" rates rs 0.1
  betaErrorScatter "other/betascatter.svg" rs rates bs
  _ <-
    runOutput
      ("other/random.md", GitHubMarkdown)
      ("random.html", Html)
      $ pure ()
  pure ()

-- | common pattern of chart title, x-axis title and y-axis title
titlesHud :: Text -> Text -> Text -> HudOptions
titlesHud t x y =
  defaultHudOptions
    & #hudTitles
    .~ [ defaultTitle t,
         defaultTitle x & #place .~ PlaceBottom & #style . #size .~ 0.08,
         defaultTitle y & #place .~ PlaceLeft & #style . #size .~ 0.08
       ]
