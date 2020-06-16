{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Run.Types
  ( ls,
    selectItems,
    repItemsSelect,
    scanHud,
    scanChart,
    foldScanChart,
    titlesHud,
    onlineRs,
    writeChartOnline,
    chartOnline,
    svgName,
    writeOnline,
    histChart,
    scatterChart,
    quantileChart,
    quantileHistChart,
    digitChart,
    digitPixelChart,
  )
where

import Chart
import Control.Lens hiding ((:>), (<&>), Unwrapped, Wrapped)
import Control.Monad
import Data.Bifunctor
import Data.Generics.Labels ()
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as Text
import Data.Time
import Data.Yahoo
import NumHask.Prelude hiding (fold, (<<*>>), asum)
import NumHask.Space
import Stats
import Data.List ((!!))
import Web.Page hiding (StateT(..), State, state, get, bool, runState)
import qualified Data.Attoparsec.Text as A

type Rate = Double

data RunConfig
  = RunConfig
      { nAll :: Int,
        n :: Int,
        rates :: [Double],
        betaRate :: Double,
        versus :: (Double, Double),
        qs :: [Double],
        qsRate :: Double,
        foreRate :: (Double, Double),
        histGrain :: Int,
        histRange :: Range Double,
        name :: FilePath,
        dir :: FilePath
      }
  deriving (Eq, Show, Generic)

defaultRunConfig :: RunConfig
defaultRunConfig = RunConfig 12000 10000 [0.95, 0.99] 0.99 (0.95, 0.95) ((0.1 *) <$> [1 .. 9]) 0.99 (0.99, 0.99) 20 (Range (-0.03) 0.03) "default" "other/default"

-- generic rep helpers
selectItems :: [Text] -> Map.Map Text Text -> [(Text,Text)]
selectItems ks m =
  Map.toAscList $
    Map.filterWithKey (\k _ -> k `elem` ks) m

-- selectItems ks (randomCharts defaultSvgOptions rs)
repItemsSelect :: Monad m => [Text] -> [Text] -> SharedRep m [Text]
repItemsSelect init full =
  dropdownMultiple (A.takeWhile (`notElem` [','])) id (Just "items") full init

-- * chart helpers
ls :: [LineStyle]
ls = fmap (\c -> defaultLineStyle & #color .~ c & #width .~ 0.003) palette

-- simple scan of a time series through a Mealy using a list of rates, with time dimension labelled as 0..
scanChart :: (Rate -> Mealy a Double) -> [Rate] -> Int -> [a] -> [Chart Double]
scanChart m rates d xs =
  ( zipWith (\s xs' -> Chart (LineA s) xs') ls $
    (zipWith SP (fromIntegral <$> [d..]) <$> ((\r -> drop d $ scan (m r) xs) <$> rates))
  )

-- | common line chart hud with rates as a legend
scanHud :: Text -> [Double] -> HudOptions
scanHud t rates = 
  (defaultHudOptions &
     #hudTitles .~ [ defaultTitle t] &
     #hudLegend .~ Just
      ( defaultLegendOptions
          & #ltext . #size .~ 0.2
          & #lplace .~ PlaceAbsolute (Point 0.3 (-0.3))
          & #legendFrame .~ Just (RectStyle 0.02 (palette !! 5) white),
        zipWith
          (\a r -> (LineA a, ("rate = " <>) . Text.pack . show $ r))
          ls
          rates
      ))

-- | fold over a scanned time series by rates
foldScanChart :: (Rate -> Mealy a b) -> (Rate -> Mealy b Double) -> [Rate] -> [a] -> [Chart Double]
foldScanChart scan' fold' rates xs =
    ( (: []) $
        Chart
          (LineA defaultLineStyle)
          (zipWith SP rates ((\r -> fold (fold' r) $ scan (scan' r) xs) <$> rates)))

-- | common pattern of chart title, x-axis title and y-axis title
titlesHud :: Text -> Text -> Text -> HudOptions
titlesHud t x y =
  defaultHudOptions
    & #hudTitles
    .~ [ defaultTitle t,
         defaultTitle x & #place .~ PlaceBottom & #style . #size .~ 0.08,
         defaultTitle y & #place .~ PlaceLeft & #style . #size .~ 0.08
       ]

-- uptohere
onlineRs :: RunConfig -> (Double -> [Double] -> [Double]) -> [Double] -> [[Point Double]]
onlineRs c f xs =
  zipWith Point [0 ..]
    . taker (c ^. #n)
    . drop 1
    . (`f` xs)
    <$> (c ^. #rates)

chartOnline :: RunConfig -> (Double -> [Double] -> [Double]) -> [Double] -> [Chart Double]
chartOnline c f xs = zipWith (\l c -> Chart (LineA l) c)
  lineStyles (zeroLine : (fmap SpotPoint <$> onlineRs c f xs))
  where
    zeroLine = SpotPoint <$> [Point lx 0, Point ux 0]
    (Rect lx ux _ _) = space1 $ mconcat (onlineRs c f xs)

lineStyles :: [LineStyle]
lineStyles = zipWith (\c w -> defaultLineStyle & #color .~ c & #width .~ w) palette (0.001 : repeat 0.003)

hudOptionsOnline :: RunConfig -> Text -> [Day] -> HudOptions
hudOptionsOnline c title ds =
  defaultHudOptions
    & #hudTitles
    .~ [defaultTitle title & #style . #size .~ 0.08]
    & #hudLegend
    .~ Just
      ( defaultLegendOptions
          & #ltext . #size .~ 0.3
          & #lplace .~ PlaceBottom,
        zipWith
          (\a r -> (LineA a, ("rate = " <>) . Text.pack . show $ r))
          (drop 1 lineStyles)
          (c ^. #rates)
      )
    & #hudAxes
    .~ [ defaultAxisOptions
           & #atick . #tstyle .~ TickRound (FormatPercent 0) 6 TickExtend
           & #place .~ PlaceLeft,
         defaultAxisOptions & #atick . #tstyle
           .~ TickPlaced
             ( first fromIntegral
                 <$> makeTickDates PosIncludeBoundaries Nothing 8 ((`UTCTime` 0) <$> taker (c ^. #n) ds)
             )
       ]

writeChartOnline :: FilePath -> RunConfig -> Text -> [Day] -> [Chart Double] -> IO ()
writeChartOnline fp c title ds cs = writeFile fp $ renderHudOptionsChart defaultSvgOptions (hudOptionsOnline c title ds) [] cs

svgName :: RunConfig -> FilePath -> FilePath
svgName c f = view #dir c <> view #name c <> "/" <> f

writeOnline :: FilePath -> RunConfig -> (Double -> [Double] -> [Double]) -> [(Day, Double)] -> IO ()
writeOnline fp c f rs =
  writeChartOnline
    (svgName c fp)
    c
    ""
    (taker (c ^. #n) $ fst <$> rs)
    (chartOnline c f (taker (c ^. #n) $ snd <$> rs))

quantileChart ::
  FilePath ->
  Text ->
  [Text] ->
  [(Day, [Double])] ->
  IO ()
quantileChart fp title names xs = writeFile fp $ renderHudOptionsChart defaultSvgOptions hudOptions [] chart'
  where
    hudOptions =
      defaultHudOptions
        & #hudTitles .~ [defaultTitle title]
        & ( #hudLegend
              .~ Just
                ( defaultLegendOptions
                    & #ltext . #size .~ 0.1
                    & #vgap .~ 0.05
                    & #innerPad .~ 0.2
                    & #lplace .~ PlaceRight,
                  legendFromChart names chart'
                )
          )
        & #hudAxes
          .~ [ defaultAxisOptions
                 & #atick . #tstyle .~ TickRound (FormatPercent 0) 6 TickExtend
                 & #place .~ PlaceLeft,
               defaultAxisOptions & #atick . #tstyle .~ TickPlaced dateTicks
             ]
    qss = transpose $ snd <$> xs
    dateTicks = first fromIntegral <$> makeTickDates PosIncludeBoundaries Nothing 8 ((`UTCTime` 0) . fst <$> xs)
    chart' =
      zipWith (\l c -> Chart (LineA l) c) lo (zipWith SP [0 ..] <$> qss)
    l = length names
    m = (fromIntegral l - 1) / 2 :: Double
    cs = (\x -> 1 - abs (fromIntegral x - m) / m) <$> [0 .. (l - 1)]
    bs = (\x -> blend x (Colour 0.7 0.1 0.3 0.2) (Colour 0.1 0.4 0.8 1)) <$> cs
    lo = (\c -> defaultLineStyle & #width .~ 0.001 & #color .~ c) <$> bs

-- | a chart showing a time series of digitized values (eg this return was a 30th percentile)
digitChart ::
  FilePath ->
  Text ->
  [Text] ->
  [(Day, Int)] ->
  IO ()
digitChart fp title names xs = writeFile fp $ renderHudOptionsChart defaultSvgOptions hudOptions [] [chart', chartma, chartstd]
  where
    hudOptions =
      defaultHudOptions
        & #hudTitles .~ [defaultTitle title]
        & #hudAxes
          .~ [ defaultAxisOptions
                 & #atick . #tstyle .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) names)
                 & #place .~ PlaceLeft,
               defaultAxisOptions & #atick . #tstyle .~ TickPlaced dateTicks
             ]
    xs' = fromIntegral . snd <$> xs
    dateTicks = first fromIntegral <$> makeTickDates PosIncludeBoundaries Nothing 8 ((`UTCTime` 0) . fst <$> xs)
    chart' = Chart (GlyphA (defaultGlyphStyle & #color .~ Colour 0 0 1 1 & #shape .~ CircleGlyph & #size .~ 0.01)) (zipWith SP [0 ..] xs')
    -- FIXME: refactor scans out of here
    chartma = Chart (LineA defaultLineStyle) (zipWith SP [0 ..] (drop 1 $ scan (ma 0.95) xs'))
    chartstd = Chart (LineA (defaultLineStyle & #color .~ Colour 1 0 0 1)) (zipWith SP [0 ..] (drop 1 $ scan (std 0.95) xs'))

-- | scatter chart
scatterChart ::
  FilePath ->
  [Point Double] ->
  IO ()
scatterChart fp rels = writeFile fp $ renderHudOptionsChart defaultSvgOptions defaultHudOptions [] [chart']
  where
    chart' =
      Chart
        ( GlyphA
            ( defaultGlyphStyle
                & #color .~ Colour 1 0 0 0.3
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
  FilePath ->
  IO ()
histChart title names r g xs fp = writeFile fp $ renderHudOptionsChart defaultSvgOptions hudOptions [] [chart']
  where
    hudOptions =
      defaultHudOptions
        & #hudTitles .~ [defaultTitle title]
        & #hudAxes
          .~ [ maybe
                 (defaultAxisOptions & #atick . #tstyle .~ TickRound (FormatPercent 0) 8 TickExtend)
                 ( \x ->
                     defaultAxisOptions & #atick . #tstyle
                       .~ TickPlaced (zip [0 ..] x)
                 )
                 names
             ]
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
  FilePath ->
  IO ()
quantileHistChart title names qs vs fp = writeFile fp $ renderHudOptionsChart defaultSvgOptions hudOptions [] [chart']
  where
    hudOptions =
      defaultHudOptions
        & #hudTitles
        .~ [defaultTitle title]
        & #hudAxes
        .~ [ maybe
               (defaultAxisOptions & #atick . #tstyle .~ TickRound (FormatPercent 0) 8 TickExtend)
               ( \x ->
                   defaultAxisOptions & #atick . #tstyle
                     .~ TickPlaced (zip vs x)
               )
               names
           ]
    chart' = Chart (RectA defaultRectStyle) (SpotRect <$> hr)
    hr = zipWith (\(y, w) (x, z) -> Rect x z 0 ((w - y) / (z - x))) (zip qs (drop 1 qs)) (zip vs (drop 1 vs))

-- | pixel chart of digitized vs digitzed counts
digitPixelChart ::
  PixelStyle ->
  PixelLegendOptions ->
  (Text, Text, Text) ->
  [Text] ->
  [(Int, Int)] ->
  [Chart Double]
digitPixelChart pixelStyle plo ts names ps =
  runHud (aspect 1) (hs0 <> hs1) (cs0 <> cs1)
  where
    l = length names
    pts = Point l l
    gr :: Rect Double
    gr = fromIntegral <$> Rect 0 l 0 l
    mapCount = foldl' (\m x -> Map.insertWith (+) x 1.0 m) Map.empty ps
    f :: Point Double -> Double
    f (Point x y) = fromMaybe 0 $ Map.lookup (fromIntegral (floor x :: Integer), fromIntegral (floor y :: Integer)) mapCount
    (hs0, cs0) = makeHud gr (qvqHud ts names)
    (cs1, hs1) =
      pixelfl
        f
        (PixelOptions pixelStyle pts gr)
        plo

-- style helpers
returnHud :: (Text, Text, Text) -> HudOptions
returnHud ts =
  defaultHudOptions
    & #hudTitles .~ makeTitles ts
    & #hudAxes
      .~ [ defaultAxisOptions
             & #atick . #tstyle .~ TickRound (FormatPercent 0) 6 TickExtend
             & #place .~ PlaceLeft,
           defaultAxisOptions
             & #atick . #tstyle .~ TickRound (FormatPercent 0) 6 TickExtend
             & #place .~ PlaceBottom
         ]

qvqHud :: (Text, Text, Text) -> [Text] -> HudOptions
qvqHud ts labels =
  defaultHudOptions
    & #hudTitles .~ makeTitles ts
    & #hudAxes
      .~ [ defaultAxisOptions
             & #atick . #tstyle .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) labels)
             & #place .~ PlaceLeft,
           defaultAxisOptions
             & #atick . #tstyle .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) labels)
             & #place .~ PlaceBottom
         ]

makeTitles :: (Text, Text, Text) -> [Title]
makeTitles (t, xt, yt) =
  reverse
    [ defaultTitle t,
      defaultTitle xt & #place .~ PlaceBottom & #style . #size .~ 0.06,
      defaultTitle yt & #place .~ PlaceLeft & #style . #size .~ 0.06
    ]

