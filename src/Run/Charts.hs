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

module Run.Charts
  ( onlineChart,
    histChart,
    scatterChart,
    quantileChart,
    quantileHistChart,
    digitChart,
    digitPixelChart,
  )
where

import Chart
import qualified Control.Foldl as L
import Control.Lens hiding ((:>), (<&>), Unwrapped, Wrapped)
import Control.Monad
import Data.Bifunctor
import Data.Generics.Labels ()
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as Text
import Data.Time
import Data.Yahoo
import Online
import Options.Generic
import Protolude
import Run.Types
import NumHask.Space

-- | A chart showing a time-series of a statistic at different online rates.
onlineChart :: [(UTCTime, Double)] -> RunConfig -> Text -> (Double -> [Double] -> [Double]) -> ChartSvg Double
onlineChart xs c title f =
  makeHudChartSvg
    (aspect 2)
    ( defaultHudConfig
        & #hudTitles .~ [defaultTitle title & #style . #size .~ 0.08]
        & #hudLegend
          .~ Just
            ( defaultLegendOptions
                & #scale .~ 0.2
                & #ltext . #size .~ 0.2
                & #lplace .~ PlaceRight
            ,   zipWith
                    (\a r -> (LineA a, ("rate = " <>) . Text.pack . show $ r))
                    (drop 1 lopts)
                    (c ^. #rates)
            )
        & #hudAxes
          .~ [ defaultAxisConfig
                 & #atick . #tstyle .~ TickRound (FormatPercent 0) 6 TickExtend
                 & #place .~ PlaceLeft,
               defaultAxisConfig & #atick . #tstyle
                 .~ TickPlaced
                   ( first fromIntegral
                       <$> makeTickDates PosIncludeBoundaries Nothing 8 (fst <$> taker (c ^. #n) xs)
                   )
             ]
    )
    chart'
  where
    lopts :: [LineStyle]
    lopts = zipWith (\c w -> defaultLineStyle & #color .~ c & #width .~ w) (drop 3 d3Palette1) (0.002 : repeat 0.007)
    chartd =
      zipWith Point [0 ..]
        . taker (c ^. #n)
        . drop 1
        . (\r -> f r (snd <$> xs))
        <$> (c ^. #rates)
    chart' = zipWith (\l c -> Chart (LineA l) c) lopts (zeroLine : (fmap SpotPoint <$> chartd))
    zeroLine = SpotPoint <$> [Point lx 0, Point ux 0]
    (Rect lx ux _ _) = space1 $ mconcat chartd

-- | a chart showing a time-series of quantile boundary values
quantileChart ::
  Text ->
  [Text] ->
  [(UTCTime, [Double])] ->
  ChartSvg Double
quantileChart title names xs =
  makeHudChartSvg
    (aspect 2)
    ( defaultHudConfig
        & #hudTitles .~ [defaultTitle title]
        & ( #hudLegend
              .~ Just
                ( defaultLegendOptions
                    & #scale .~ 0.3
                    & #ltext . #size .~ 0.1
                    & #vgap .~ 0.05
                    & #innerPad .~ 0.2
                    & #lplace .~ PlaceRight
                , legendFromChart names chart'
                )
          )
        & #hudAxes
          .~ [ defaultAxisConfig
                 & #atick . #tstyle .~ TickRound (FormatPercent 0) 6 TickExtend
                 & #place .~ PlaceLeft,
               defaultAxisConfig & #atick . #tstyle .~ TickPlaced dateTicks
             ]
    )
    chart'
  where
    qss = transpose $ snd <$> xs
    dateTicks = first fromIntegral <$> makeTickDates PosIncludeBoundaries Nothing 8 (fst <$> xs)
    chart' =
      zipWith (\l c -> Chart (LineA l) c) lo (zipWith SP [0 ..] <$> qss)
    l = length names
    m = (fromIntegral l - 1) / 2 :: Double
    cs = (\x -> 1 - abs (fromIntegral x - m) / m) <$> [0 .. (l - 1)]
    bs = (\x -> blend' x (grey, 0.2) (red, 1)) <$> cs
    lo = (\(c, o) -> defaultLineStyle & #width .~ 0.01 & #color .~ c & #opacity .~ o) <$> bs

-- | a chart showing a time series of digitized values (eg this return was a 30th percentile)
digitChart ::
  Text ->
  [Text] ->
  [(UTCTime, Int)] ->
  ChartSvg Double
digitChart title names xs =
  makeHudChartSvg
    (aspect 2)
    ( defaultHudConfig
        & #hudTitles .~ [defaultTitle title]
        & #hudAxes
          .~ [ defaultAxisConfig
                 & #atick . #tstyle .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) names)
                 & #place .~ PlaceLeft,
               defaultAxisConfig & #atick . #tstyle .~ TickPlaced dateTicks
             ]
    )
    [chart', chartma, chartstd]
  where
    xs' = fromIntegral . snd <$> xs
    dateTicks = first fromIntegral <$> makeTickDates PosIncludeBoundaries Nothing 8 (fst <$> xs)
    chart' = Chart (GlyphA (defaultGlyphStyle & #color .~ blue & #shape .~ CircleGlyph & #size .~ 0.01)) (zipWith SP [0 ..] xs')
    chartma = Chart (LineA defaultLineStyle) (zipWith SP [0 ..] (drop 1 $ L.scan (ma 0.95) xs'))
    chartstd = Chart (LineA (defaultLineStyle & #color .~ red)) (zipWith SP [0 ..] (drop 1 $ L.scan (std 0.95) xs'))

-- | scatter chart
scatterChart ::
  (Text, Text, Text) ->
  [Point Double] ->
  ChartSvg Double
scatterChart ts rels =
  makeHudChartSvg
    (aspect 1)
    (returnHud ts)
    [chart']
  where
    chart' =
      Chart
        ( GlyphA
            ( defaultGlyphStyle
                & #color .~ red
                & #borderOpacity .~ 0
                & #opacity .~ 0.3
                & #size .~ 0.02
            )
        )
        (SpotPoint <$> rels)

-- | histogram chart
histChart ::
  Text ->
  Maybe [Text] ->
  Range Double ->
  Int ->
  [Double] ->
  ChartSvg Double
histChart title names r g xs =
  makeHudChartSvg
    (aspect 2)
    ( defaultHudConfig
        & #hudTitles .~ [defaultTitle title]
        & #hudAxes
          .~ [ maybe
                 (defaultAxisConfig & #atick . #tstyle .~ TickRound (FormatPercent 0) 8 TickExtend)
                 ( \x ->
                     defaultAxisConfig & #atick . #tstyle
                       .~ TickPlaced (zip [0 ..] x)
                 )
                 names
             ]
    )
    [chart']
  where
    chart' = Chart (RectA defaultRectStyle) (SpotRect <$> hr)
    hcuts = grid OuterPos r g
    h = fill hcuts xs
    hr = makeRects (IncludeOvers (NumHask.Space.width r / fromIntegral g)) h

-- | a chart drawing a histogram based on quantile information
quantileHistChart ::
  Text ->
  Maybe [Text] ->
  -- | quantiles
  [Double] ->
  -- | quantile values
  [Double] ->
  ChartSvg Double
quantileHistChart title names qs vs =
  makeHudChartSvg
  (aspect 2)
    ( defaultHudConfig
        & #hudTitles .~ [defaultTitle title]
        & #hudAxes
          .~ [ maybe
                 (defaultAxisConfig & #atick . #tstyle .~ TickRound (FormatPercent 0) 8 TickExtend)
                 ( \x ->
                     defaultAxisConfig & #atick . #tstyle
                       .~ TickPlaced (zip vs x)
                 )
                 names
             ]
    )
    [chart']
  where
    chart' = Chart (RectA defaultRectStyle) (SpotRect <$> hr)
    hr = zipWith (\(y,w) (x,z) -> Rect x z 0 ((w-y)/(z-x))) (zip qs (drop 1 qs)) (zip vs (drop 1 vs))

-- | pixel chart of digitized vs digitzed counts
digitPixelChart ::
  PixelStyle ->
  PixelLegendOptions ->
  (Text, Text, Text) ->
  [Text] ->
  [(Int, Int)] ->
  ChartSvg Double
digitPixelChart pixelStyle plo ts names ps =
  runHudSvg (aspect 1) (hs0 <> hs1) (cs0 <> cs1)
  where
    l = length names
    pts = Point l l
    gr :: Rect Double
    gr = fromIntegral <$> Rect 0 l 0 l
    mapCount = foldl' (\m x -> Map.insertWith (+) x 1.0 m) Map.empty ps
    f :: Point Double -> Double
    f (Point x y) = fromMaybe 0 $ Map.lookup (floor x, floor y) mapCount
    (hs0, cs0) = makeHud gr (qvqHud ts names)
    (cs1, hs1) =
      pixelfl
        f
        (PixelOptions pixelStyle pts gr)
        plo

returnHud :: (Text, Text, Text) -> HudConfig
returnHud ts =
  defaultHudConfig
    & #hudTitles .~ makeTitles ts
    & #hudAxes
      .~ [ defaultAxisConfig
             & #atick . #tstyle .~ TickRound (FormatPercent 0) 6 TickExtend
             & #place .~ PlaceLeft,
           defaultAxisConfig
             & #atick . #tstyle .~ TickRound (FormatPercent 0) 6 TickExtend
             & #place .~ PlaceBottom
         ]

qvqHud :: (Text, Text, Text) -> [Text] -> HudConfig
qvqHud ts labels =
  defaultHudConfig
    & #hudTitles .~ makeTitles ts
    & #hudAxes
      .~ [ defaultAxisConfig
             & #atick . #tstyle .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) labels)
             & #place .~ PlaceLeft,
           defaultAxisConfig
             & #atick . #tstyle .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) labels)
             & #place .~ PlaceBottom
         ]

makeTitles :: (Text, Text, Text) -> [Title]
makeTitles (t, xt, yt) = reverse
  [ defaultTitle t,
    defaultTitle xt & #place .~ PlaceBottom & #style . #size .~ 0.06,
    defaultTitle yt & #place .~ PlaceLeft & #style . #size .~ 0.06
  ]
