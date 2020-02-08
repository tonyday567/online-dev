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
import Data.Csv hiding (Field)
import Data.Generics.Labels ()
import Data.List (transpose)
import Data.Maybe
import Data.Text.Encoding (decodeUtf8)
import Data.Time
import Online
import Options.Generic
import PMLB
import PMLB.Csv
import Prelude
import qualified Control.Foldl as L
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Text as Text
import Readme.Lhs
import Data.Scientific
-- import Data.Reflection
-- import Data.TDigest.Postprocess (HistBin (..))
-- import Numeric.Backprop as B

-- yahoo formatted data
-- See: https://finance.yahoo.com/quote/%5EGSPC/history?period1=-631015200&period2=1497103200&interval=1d&filter=history&frequency=1d
data YahooData
  = YahooData
      { yDate :: !Text,
        yOpen :: Double,
        yHigh :: Double,
        yLow :: Double,
        yClose :: Double,
        yAdjClose :: !Double,
        yVolume :: Double
      }
  deriving (Generic, Show)

instance FromNamedRecord YahooData where
  parseNamedRecord m =
    YahooData <$> m .: "Date" <*> m .: "Open" <*> m .: "High" <*> m .: "Low"
      <*> m
        .: "Close"
      <*> m
        .: "Adj Close"
      <*> m
        .: "Volume"

pYahoo :: Char -> AC.Parser YahooData
pYahoo c = do
  d <- AC.takeWhile (/= c)
  _ <- AC.char c
  o <- AC.double
  _ <- AC.char c
  h <- AC.double
  _ <- AC.char c
  l <- AC.double 
  _ <- AC.char c
  close <- AC.double
  _ <- AC.char c
  ac <- AC.double
  _ <- AC.char c
  v <- AC.double
  _ <- AC.endOfLine
  pure (YahooData (decodeUtf8 d) (realToFrac o) (realToFrac h) (realToFrac l) (realToFrac close) (realToFrac ac) (realToFrac v))

csvConfig :: Config
csvConfig = defaultConfig & #dataSet .~ "^GSPC" & #csep .~ ','

getYahoo :: Int -> IO [YahooData]
getYahoo n = taker n . snd <$> runBS csvConfig (parseCsv PMLB.Csv.HasHeader 10000000 ',' pYahoo)

getdc :: [YahooData] -> [(UTCTime, Double)]
getdc xs = lret $ (\x -> (view #yDate x, view #yClose x)) <$> xs

-- compute the log return from a price series
-- returns are geometric by nature, and using log(1+daily return) as the base unit of the time series leads to all sorts of benefits, not least of which is you can then add up the variates to get the cumulative return, without having to go through log and exp chicanery.  Ditto for distributional assumptions.
lret :: [(Text, Double)] -> [(UTCTime, Double)]
lret [] = []
lret [_] = []
lret ((_, v0) : xs) = L.fold (L.Fold step ([], v0) (reverse . fst)) xs
  where
    step (acc, v) (d', v') = case parseUTCTime d' of
      Just d -> ((d, log (v' / v)) : acc, v')
      Nothing -> (acc, v)

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

taker :: Int -> [a] -> [a]
taker n = reverse . take n . reverse

lastOnes :: (Eq a) => (a -> a -> Bool) -> [a] -> [a]
lastOnes _ [] = []
lastOnes _ [x] = [x]
lastOnes f (x : xs) = L.fold (L.Fold step (x, []) (\(x0, x1) -> reverse $ x0 : x1)) xs
  where
    step (a0, rs) a1 = if f a0 a1 then (a1, rs) else (a1, a0 : rs)

makeChartDates :: [UTCTime] -> [(Int, Text)]
makeChartDates dates =
  lastOnes (\(_, x0) (_, x1) -> x0 == x1) $
  fst $ placedTimeLabelDiscontinuous PosIncludeBoundaries Nothing 8 dates

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



computeCharts :: [(Int, Text)] -> [Double] -> Int -> [Double] -> [(FilePath, Text, Double -> [Double] -> [Double])] -> IO ()
computeCharts dates xs n rs putes = sequence_ $ makeChart <$> putes
  where
    makeChart (f, title, sc) =
      writeChartSvg ("other/" <> f <> ".svg") (Point 600 300)
      (onlineChart title rs (first fromIntegral <$> dates)
       (zipWith Point [0 ..] .
        taker n .
        drop 1 .
        (\f -> f xs) <$>
        (sc <$> rs)))

expt :: Int -> Double -> Text
expt x n = Text.pack $ formatScientific Exponent (Just x) (fromFloatDigits n)

fixed :: Int -> Double -> Text
fixed x n = Text.pack $ formatScientific Fixed (Just x) (fromFloatDigits n)


-- xs <- taker 500 . fmap snd . getdc <$> getYahoo 1000
main :: IO ()
main = do
  let nall = 1000
  let n = 500
  let rs = [0.95, 0.99, 0.996]
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
  computeCharts dates xs' n rs putes
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
  writeChartSvg "other/digitpixel.svg" (Point 600 400) (digitChart "digitalised return" (first fromIntegral <$> dates) (fromIntegral <$> d))
  let hqs = regular 11 (fromIntegral <$> d)
  let hrqs = makeRects IgnoreOvers hqs
  writeChartSvg "other/digitcheck.svg" (Point 600 400) (histChart "digit count" hrqs)


  void $ runOutput
      ("readme.md", GitHubMarkdown)
      ("index.html", Html)
    $
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

{-
fore2 :: (Floating a) => a -> a -> [a] -> [Point a]
fore2 r1 r0 xs =
  L.scan
    ( (\b o a r -> Point (a + b * o) r) <$> beta (ma r1)
        <*> L.premap snd (ma 0.00001)
        <*> alpha (ma r1)
        <*> L.premap fst (ma 0.00001)
    )
    $ drop 2
    $ zip xs (L.scan (ma r0) xs)

costAbs ::
  (List.ListLike (f a) a, Fractional a, Foldable f, Applicative f) =>
  f a ->
  f a ->
  f a ->
  f (f a) ->
  a
costAbs ms c ys xss = av
  where
    av = (/ (fromIntegral $ length ys)) (L.fold L.sum $ abs <$> es)
    es = ys ~-~ (L.fold L.sum <$> (c ~+ (ms ~* xss)))

(~*) :: (Num a, Applicative f) => f a -> f (f a) -> f (f a)
(~*) ms xss = (<$>) . (*) <$> ms <*> xss

(~+) :: (Num a, Applicative f) => f a -> f (f a) -> f (f a)
(~+) ms xss = (<$>) . (+) <$> ms <*> xss

(~-~) :: (List.ListLike (f a) a, Num a) => f a -> f a -> f a
(~-~) = List.zipWith (-)

costScan ::
  (Reifies s Tape) => [Double] -> [Reverse s Double] -> Reverse s Double
costScan xs (m : c : _) =
  costAbs
    [m]
    [c]
    (AD.auto <$> drop 1 xs)
    [fmap AD.auto <$> drop 1 $ L.scan (ma 0.99) xs]
costScan xs [] =
  costAbs
    []
    []
    (AD.auto <$> drop 1 xs)
    [fmap AD.auto <$> drop 1 $ L.scan (ma 0.99) xs]
costScan xs [m] =
  costAbs
    [m]
    []
    (AD.auto <$> drop 1 xs)
    [fmap AD.auto <$> drop 1 $ L.scan (ma 0.99) xs]

grid ::
  (Fractional b) =>
  Range b ->
  Int ->
  [b]
grid (Range l u) steps =
  (\a -> l + (u - l) / fromIntegral steps * a) . fromIntegral <$> [0 .. steps]

locs0 :: Range Double -> Range Double -> Int -> [Point Double]
locs0 rx ry steps =
  [Point x y | x <- Main.grid rx steps, y <- Main.grid ry steps]

gradF ::
  ( forall s.
    (Reifies s Tape) =>
    [Reverse s Double] ->
    Reverse s Double
  ) ->
  Double ->
  Point Double ->
  Point Double
gradF f step (Point x y) =
  fmap
    ((-1) *)
    (\[x', y'] -> Point x' y')
    (gradWith (\x0 x1 -> x0 + (x1 - x0) * step) f [x, y])

{-
addGrad ::
     (forall s. (Reifies s Tape) =>
                  [Reverse s Double] -> Reverse s Double)
  -> [Pair Double]
  -> Double
  -> [Arrow]
addGrad f xys step =
  zipWith Arrow xys (gradF f step <$> xys)

-}

chartGrad :: Rect Double -> Int -> Double -> [Double] -> ChartSvg Double
chartGrad (Ranges rx ry) grain step xs =
  hud defaultHudConfig unitRect [pos]
  where
    -- d = addGrad (costScan xs) pos step
    pos = undefined -- locs0 rx ry grain

extrap :: [Double] -> (Double, [Double]) -> (Double, [Double])
extrap xs (eta0, x0) = expand eta0 x0
  where
    (res0, _) = grad' (costScan xs) x0
    contract eta x
      | res1 < res0 = (eta, x1)
      | otherwise = contract (eta / 2) x1
      where
        x1 = x ~-~ ((eta *) <$> snd (grad' (costScan xs) x))
        res1 = fst $ grad' (costScan xs) x1
    expand eta x
      | res' < res0 = expand (eta * 2) x'
      | otherwise = contract (eta / 2) x
      where
        x' :: [Double]
        x' = x ~-~ ((eta *) <$> g)
        (_, g) = grad' (costScan xs) x
        (res', _) = grad' (costScan xs) x'

-}


{-
    let hCMeane = toHistogramWithCuts cuts $ L.fold tDigestHist $
          take 5000 $ drop 100 $
          L.scan ((\r b o a -> r - b * o - a) <$>
             L.premap fst (ma 0.00001) <*>
             beta 0.975 <*>
             L.premap snd (ma 0.00001) <*>
             alpha 0.975) $
           drop 400 $ zip xs (L.scan (ma 0.975) xs)
    let hCMeane' = toHistogramWithCuts cuts $ L.fold tDigestHist $ take 5000 $ drop 100 xs

    fileSvg "other/cmeane.svg" (300,300) $ histCompare (IncludeOvers 0.01) hCMeane hCMeane'
    fileSvg "other/csqma.svg" (300,300) $
        withChart def
        (lineChart
         [ LineConfig 0.002 (Color 0.88 0.33 0.12 1)
         , LineConfig 0.002 (Color 0.12 0.33 0.83 1)
         ])
        (fmap (zipWith V2 [0..]) <$> (\x -> [fst <$> x, snd <$> x]) $
         take 12000 $ drop 12000 $ drop 2 $
         L.scan ((,) <$> alpha 0.99 <*> beta 0.99) $
           drop 100 $ zip ((**2)<$> xs) (L.scan (sqma 0.9975) xs))

    fileSvg "other/arrows.svg" (600,600)
        (chartGrad (Rect (V2 one one)) gr 0.01 xs # pad 1.1)

-}
