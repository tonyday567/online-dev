{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main where

import Chart
import Control.Lens hiding ((:>), (<&>), Unwrapped, Wrapped)
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.Generics.Labels ()
import Data.List (transpose, foldl')
import Data.Maybe
import Data.Time
import Data.Yahoo
import Online
import Options.Generic
import Prelude
import qualified Control.Foldl as L
import qualified Data.Text as Text
import Readme.Lhs
import Data.Scientific
import qualified Data.Map.Strict as Map

-- charting tech
lopts :: [LineStyle]
lopts =
  zipWith
    (\w c -> defaultLineStyle & #color .~ c & #width .~ w)
    [0.005 :: Double, 0.005, 0.005, 0.005]
    [ PixelRGB8 197 140 75,
      PixelRGB8 60 127 43,
      PixelRGB8 56 42 140,
      PixelRGB8 2 200 20
    ]

makeChartDates :: [UTCTime] -> [(Int, Text)]
makeChartDates dates =
  lastOnes (\(_, x0) (_, x1) -> x0 == x1) $
  fst $ placedTimeLabelDiscontinuous PosIncludeBoundaries Nothing 8 dates
  where
    lastOnes :: (a -> a -> Bool) -> [a] -> [a]
    lastOnes _ [] = []
    lastOnes _ [x] = [x]
    lastOnes f (x : xs) = L.fold (L.Fold step (x, []) (\(x0, x1) -> reverse $ x0 : x1)) xs
      where
        step (a0, rs) a1 = if f a0 a1 then (a1, rs) else (a1, a0 : rs)

onlineChart :: Text -> [Double] -> [(Double, Text)] -> [[Point Double]] ->
  ChartSvg Double
onlineChart title rs ticks' chartd =
  hud
    ( defaultHudConfig
        & #hudTitles .~ [defaultTitle title]
        & (#hudLegend .~ Just
           ( LegendFromChart (["zero"] <> (("rate = " <>) . Text.pack . show <$> rs)),
             (defaultLegendOptions :: LegendOptions Double) &
             #scale .~ (0.2 :: Double) &
             #ltext . #size .~ (0.2 :: Double) &
             #lplace .~ PlaceAbsolute (Point 0.6 (-0.2))
           )
          )
        & #hudAxes .~
        [ defaultAxisConfig & #atick . #tstyle .~ TickPlaced ticks'
        , defaultAxisConfig &
          #atick . #tstyle .~ TickRound TickFormatDefault 6 TickExtend &
          #place .~ PlaceLeft
        ]
    )
    (aspect 2)
    chart'
  where
    chart' = zipWith (\l c -> Chart (LineA l) c) lopts (zeroLine : (fmap SpotPoint <$> chartd))
    zeroLine = SpotPoint <$> [Point lx 0, Point ux 0]
    (Rect lx ux _ _) = space1 $ mconcat chartd

onlineChart' :: [(UTCTime, Double)] -> RunConfig -> Text -> (Double -> [Double] -> [Double]) -> [ChartSvg Double]
onlineChart' xs c title f =
  onlineChart title (c ^. #rates) (first fromIntegral . makeChartDates . fst <$> xs)
    (zipWith Point [0 ..] .
     taker (c ^. #n) .
     drop 1 .
     (\f -> f xs) <$>
     (f <$> (c ^ #rates)))

quantileChart :: Text -> [Text] -> [(Double, Text)] -> [[Double]] ->
  ChartSvg Double
quantileChart title names ticks' xs =
  hud
    ( defaultHudConfig
        & #hudTitles .~ [defaultTitle title]
        & (#hudLegend .~ Just
           ( LegendFromChart names,
             (defaultLegendOptions :: LegendOptions Double) &
             #scale .~ (0.2 :: Double) &
             #ltext . #size .~ (0.2 :: Double) &
             #lplace .~ PlaceAbsolute (Point 0.6 (-0.2))
           )
          )
        & #hudAxes .~
        [ defaultAxisConfig & #atick . #tstyle .~ TickPlaced ticks'
        , defaultAxisConfig &
          #atick . #tstyle .~ TickRound TickFormatDefault 6 TickExtend &
          #place .~ PlaceLeft
        ]
    )
    (aspect 2)
    chart'
  where
    chart' =
      zipWith (\l c -> Chart (LineA l) c) (cycle lopts) (zipWith SP [0..] <$> xs)

relChart :: (Text, Text, Text) -> [Point Double] ->
  ChartSvg Double
relChart (title, xt, yt) rels =
  hud
    ( defaultHudConfig
        & #hudTitles .~ [defaultTitle title, (defaultTitle xt :: Title Double) & #place .~ PlaceBottom & #style . #size .~ 0.06, (defaultTitle yt :: Title Double) & #place .~ PlaceLeft & #style . #size .~ 0.06]
    )
    (aspect 1)
    [chart']
  where
    chart' = Chart (GlyphA (defaultGlyphStyle & #borderOpacity .~ 0 & #opacity .~ 0.3 & #size .~ 0.02)) (SpotPoint <$> rels)

sqChart :: (Text, Text, Text) -> [Chart Double] ->
  ChartSvg Double
sqChart (title, xt, yt) cs =
  hud
    ( defaultHudConfig
        & #hudTitles .~ [defaultTitle title, (defaultTitle xt :: Title Double) & #place .~ PlaceBottom & #style . #size .~ 0.06, (defaultTitle yt :: Title Double) & #place .~ PlaceLeft & #style . #size .~ 0.06]
    )
    (aspect 1)
    cs

histChart :: Text -> [Rect Double] ->
  ChartSvg Double
histChart title xs =
  hud
    ( defaultHudConfig
        & #hudTitles .~ [defaultTitle title]
        & #hudAxes .~
        [ defaultAxisConfig
        ]
    )
    (aspect 2)
    [chart']
  where
    chart' =
      Chart (RectA defaultRectStyle) (SpotRect <$> xs)

digitChart :: Text -> [(Double, Text)] -> [Double] ->
  ChartSvg Double
digitChart title ticks' chartd =
  hud
    ( defaultHudConfig
        & #hudTitles .~ [defaultTitle title]
        & #hudAxes .~
        [ defaultAxisConfig & #atick . #tstyle .~ TickPlaced ticks'
        , defaultAxisConfig &
          #atick . #tstyle .~ TickRound TickFormatDefault 6 TickExtend &
          #place .~ PlaceLeft
        ]
    )
    (aspect 2)
    [chart', chartma]
  where
    chart' = Chart (GlyphA (defaultGlyphStyle & #size .~ 0.02)) (zipWith SP [0..] chartd)
    chartma = Chart (LineA defaultLineStyle) (zipWith SP [0..] (drop 1 $ L.scan (ma 0.99) chartd))

expt :: Int -> Double -> Text
expt x n = Text.pack $ formatScientific Exponent (Just x) (fromFloatDigits n)

-- fixed :: Int -> Double -> Text
-- fixed x n = Text.pack $ formatScientific Fixed (Just x) (fromFloatDigits n)

fore :: (Floating a) => a -> a -> [a] -> [(a,a)]
fore r1 r0 xs =
  L.scan
    ( (\b o a r -> (a + b * o, r)) <$> beta (ma r1)
        <*> L.premap snd (ma 0.00001)
        <*> alpha (ma r1)
        <*> L.premap fst (ma 0.00001)
    )
    $ drop 2
    $ zip xs (L.scan (ma r0) xs)

data RunConfig = RunConfig { nAll :: Int, n :: Int, rates :: [Double] } deriving (Eq, Show, Generic)

defaultRunConfig :: RunConfig
defaultRunConfig = RunConfig 1000 500 [0.95, 0.99]

getRets :: RunConfig -> IO [Double]
getRets c =
  taker (c ^. #n) . fmap snd . getdc <$> getYahoo (c ^. #nAll)

data RunStyleConfig = RunStyleConfig { chartSize :: Point Double } deriving (Eq, Show, Generic)

defaultRunStyleConfig :: RunStyleConfig
defaultRunStyleConfig = RunStyleConfig (Point 600 400)

main :: IO ()
main = run defaultRunConfig

run :: RunConfig -> RunStyleConfig -> IO ()
run c sc = do
  let cs = sc ^. #chartSize
  let n = c ^. #n
  let nall = c ^. #nAll
  let rs = c ^. #rates
  ydata <- getYahoo nall
  let dcs = getdc ydata
  let xs' = snd <$> dcs
  let xs = taker n xs'
  let ds = fst <$> dcs
  let dates = makeChartDates (taker n ds)
  let putes =
        [ ("c1", "moving average", L.scan . ma),
          ("c2", "std deviation", L.scan . std),
          ( "c3",
            "beta on ma",
            \r xs -> drop 2 $ L.scan (beta (ma r)) $ drop 2 $ zip xs (L.scan (ma r) xs)
          ),
          ( "c4",
            "alpha on ma",
            \r xs -> L.scan (alpha (ma r)) $ drop 2 $ zip xs (L.scan (ma r) xs)
          ),
          ( "c5",
            "beta on std",
            \r xs -> drop 2 $ L.scan (beta (ma r)) $ drop 2 $ zip xs (L.scan (std r) xs)
          ),
          ( "c6",
            "alpha on std",
            \r xs -> L.scan (alpha (ma r)) $ drop 2 $ zip xs (L.scan (std r) xs)
          )
        ]
  writeChartSvg ("other/ma.svg") cs computeCharts dates xs' n rs putes
  writeChartSvg "other/scatter.svg" (Point 600 600) $ relChart ("std 0.95 vs ma 0.99", "std 0.95", "ma 0.99") (taker n $ drop 2 $ L.scan (Point <$> std 0.95 <*> ma 0.99) xs)
  -- quantile calcs
  let qsCuts = (0.1*) <$> [1..9]
  let qsNames = fixed 1 <$> qsCuts
  let qs = taker n $ drop 1 $ L.scan (onlineQuantiles 0.99 qsCuts) xs'
  writeChartSvg "other/quantiles.svg" (Point 600 400) (quantileChart "quantiles @ 0.99" qsNames (first fromIntegral <$> dates) (transpose qs))

  -- histogram
  let h = fill ((-0.03+) . (0.003*) <$> [0..20]) xs
  let hr = makeRects (IncludeOvers 0.003) h
  writeChartSvg "other/histogram.svg" (Point 600 400) (histChart "daily return" hr)

  -- digitize
  let d = taker n $ L.scan (onlineDigitize 0.996 $ (0.1*) <$> [0..10]) xs'
  writeChartSvg "other/digitalise.svg" (Point 600 400) (digitChart "digitalised return" (first fromIntegral <$> dates) (fromIntegral <$> d))
  let hqs = regular 11 (fromIntegral <$> d)
  let hrqs = makeRects IgnoreOvers hqs
  writeChartSvg "other/digitcheck.svg" (Point 600 400) (histChart "digit count" hrqs)

  -- digitize ma vs std
  let dma = taker n $ L.scan (onlineDigitize 0.996 $ (0.1*) <$> [0..10]) (drop 2 $ L.scan (ma 0.99) xs')
  let dstd = taker n $ L.scan (onlineDigitize 0.996 $ (0.1*) <$> [0..10]) (drop 2 $ L.scan (std 0.95) xs')
  let d2 = foldl' (\m x -> Map.insertWith (+) x 1.0 m) Map.empty (zip dma dstd)
  let ps = (\(r, c) -> Chart (RectA (RectStyle 0 black 0 c 1)) [SpotRect r]) <$> pixelate (\(Point x y) -> fromMaybe 0 $ Map.lookup (floor x,floor y) d2) (Rect 0 10 0 10) (Point 10 10) white black
  writeChartSvg "other/digitpixel.svg" (Point 600 400) (sqChart ("digitalised scatter of ma 0.99 vs std 0.95", "std 0.95", "ma 0.99") ps)

  -- forecast
  let fa' = fore 0.95 0.95 xs'
  let fa = taker n fa'
  let h = fill ((-0.03+) . (0.003*) <$> [0..20]) $ fmap (\(f,a) -> a - f) fa
  let hr = makeRects (IncludeOvers 0.003) h
  writeChartSvg "other/histogramf.svg" (Point 600 400) (histChart "forecast error" hr)
  writeChartSvg "other/scatterf.svg" (Point 600 600) $ relChart ("forecast versus actual", "forecast", "actual") (uncurry Point <$> fa)
  -- digitize ma vs std
  let df = taker n $ L.scan (onlineDigitize 0.996 $ (0.1*) <$> [0..10]) (fst <$> fa')
  let da = taker n $ L.scan (onlineDigitize 0.996 $ (0.1*) <$> [0..10]) (fst <$> fa')
  let d2 = foldl' (\m x -> Map.insertWith (+) x 1.0 m) Map.empty (zip df da)
  let ps = (\(r, c) -> Chart (RectA (RectStyle 0 black 0 c 1)) [SpotRect r]) <$> pixelate (\(Point x y) -> fromMaybe 0 $ Map.lookup (floor x,floor y) d2) (Rect 0 10 0 10) (Point 10 10) white black
  writeChartSvg "other/digitf.svg" (Point 600 400) (sqChart ("digital actual v forecast", "forecast", "actual") ps)

  void $ runOutput
      ("readme.md", GitHubMarkdown)
      ("index.html", Html)
    $ do
      output "stats" $ Native $ (: [])
          (table mempty [] [AlignLeft, AlignRight] [] [
              ["Start Date", snd $ head dates],
              ["End Date", snd $ head $ reverse dates],
              ["n", Text.pack $ show n],
              ["daily average return", expt 2 (L.fold (ma 1) xs)],
              ["average return pa", expt 2 (250 * L.fold (ma 1) xs)],
              ["daily average sd return", expt 2 (L.fold (std 1) xs)],
              ["average sd return pa", expt 2 ((250.0**0.5) * L.fold (std 1) xs)]
              ])

      output "forecast" $ Native $ (: [])
          (table mempty [] [AlignLeft, AlignRight] [] [
              ["daily average forecast", expt 2 (L.fold (ma 1) (fst <$> fa))],
              ["daily average sd forecast", expt 2 (L.fold (std 1) (fst <$> fa))]
              ])
