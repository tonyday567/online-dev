{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}

import NumHask.Prelude
import Chart
import Control.Lens hiding (Wrapped, (:>), Unwrapped)
import Data.Generics.Labels()
import Online
import Data.Random
import Options.Generic
import System.Random.MWC
import qualified Control.Foldl as L
import Data.Maybe
import Data.List (transpose)
import qualified Data.Text as Text
import Readme.Lhs
import NumHask.Array.Dynamic
import Data.List ((!!))

lopts :: [LineStyle]
lopts =
  zipWith (\w c -> defaultLineStyle & #color .~ c & #width .~ w)
  [0.001 :: Double, 0.001, 0.001] palette

lopts0 = defaultLineStyle & #color .~ palette !! 0 & #width .~ 0.001

-- correlation of a time series with it's moving average.
-- x = b * (ma r xs) + a

--
-- sqrt(2) ~ (\r -> (r ** 0.5) * (1/(L.fold (std 1) $ drop 1 $ (L.scan (ma (1-r)) rs)))) r for wide r.

stdOfMa :: [Double] -> Double -> Double
stdOfMa rs r = 1/(L.fold (std 1) $ drop 1 $ (L.scan (ma (1-r)) rs))

stdOfMaChart fp rates rs =
  writeHudOptionsChart fp defaultSvgOptions (titlesHud "std of ma" "rate" "sqrt (1/r) * std of ma") [] $
  ((:[]) $ Chart (LineA defaultLineStyle)
   (zipWith SP rates ((\r -> (r ** 0.5) * stdOfMa rs r) <$> rates)))


-- | the average beta measure given an autocorrelation of the form r = a + b * ma.
-- The random variance in the beta measure (ie assuming beta = 0) is removed.
avBetaMeasure rs r b = ((\x -> x - ((L.fold (beta (ma 1)) $ (zip ((\r -> drop 1 $ zipWith (\o m -> o + 0 * m) rs (L.scan (ma (1-r)) rs)) 0.001) rs)))) $ L.fold (beta (ma 1)) $ (zip ((\r -> drop 1 $ zipWith (\o m -> o + b * m) rs (L.scan (ma (1-r)) rs)) r) rs))

-- beta measure ~ (1/r) * original beta, if the beta was set at 0.1
-- this breaks down at less than 0.003, so that long ma's will be unstable.
avBetaMeasureChart fp rates rs b =
  writeHudOptionsChart fp defaultSvgOptions (titlesHud "beta measure check" "rate" "average beta * (1/r)") [] $
  ((:[]) $ Chart (LineA defaultLineStyle)
   (zipWith SP rates ((\r -> avBetaMeasure rs r 0.1 / r) <$> rates)))

betaErrors rs rates bs = expand (\r b -> (1/(r * b)) * avBetaMeasure rs r b) (fromFlatList [length rates] rates) (fromFlatList [length bs] bs)

betaErrorScatter fp rs rates bs = writeHudOptionsChart fp defaultSvgOptions (titlesHud "beta measure error" "rates" "betas" & hudOptions) h0 c0
  where
    f1 :: (Point Double -> Double) = (\(Point x y) -> NumHask.Array.Dynamic.index (betaErrors rs rates bs) [fromIntegral (floor x :: Integer), fromIntegral $ (floor y :: Integer)])
    (c0, h0) = pixelfl f1 pixelOptions (defaultPixelLegendOptions "")
    pixelOptions = defaultPixelOptions & #poGrain .~ Point (fromIntegral $ length rates) (fromIntegral $ length bs) & #poRange .~ Rect 0 (fromIntegral $ length rates) 0 (fromIntegral $ length bs)
    hudOptions =
      #hudAxes .~
      [ defaultAxisOptions &
        #atick . #tstyle .~ TickLabels (show <$> rates),
        defaultAxisOptions &
        #place .~ PlaceLeft &
        #atick . #tstyle .~ TickLabels (show <$> bs)
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
  _ <- runOutput
    ("other/random.md", GitHubMarkdown)
    ("random.html", Html)
    $ pure ()

  pure ()

-- titlesHud "std of ma" "rate" "sqrt (1/r) * std of ma"

titlesHud t x y =
      defaultHudOptions &
      #hudTitles .~
      [ defaultTitle t,
        defaultTitle x & #place .~ PlaceBottom & #style . #size .~ 0.08,
        defaultTitle y & #place .~ PlaceLeft & #style . #size .~ 0.08
      ]
