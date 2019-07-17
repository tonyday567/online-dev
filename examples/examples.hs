{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

import Chart.Core
import Chart.Svg
import Chart.Hud
import Chart.Spot
import Chart.Data.Time
import qualified Control.Lens as Lens
import Data.Binary
import Data.Csv hiding (Field)
import Data.Generics.Labels()
import Data.List
import Data.List.NonEmpty (NonEmpty, last)
import Data.Reflection
import Data.TDigest
import Data.Time
import GHC.Base (String)
import NumHask.Histogram
import NumHask.Prelude hiding ((<&>))
import NumHask.Data.Rect
import Numeric.AD
import Numeric.AD.Internal.Reverse
import Online
import Options.Generic
import Data.TDigest.Postprocess (HistBin(..), histogram)
import Codec.Picture.Types
import qualified Control.Foldl as L
import qualified Data.ByteString.Lazy as B
import qualified Data.ListLike as List
import qualified Data.Vector as V
import qualified Protolude as P
import Control.Lens hiding (Wrapped, (&), (<&>), (:>), Unwrapped)

data Opts
  = FromCsvFile String
  | Run (Maybe RunConfig)
  deriving (Generic, Show)

newtype RunConfig = RunConfig
  { grain :: Maybe Int
  } deriving (Generic, Show, Read)

defaultRunConfig = RunConfig Nothing

instance ParseRecord RunConfig

instance ParseFields RunConfig

instance ParseField RunConfig

instance ParseRecord Opts

data YahooData = YahooData
  { yDate :: !Text
  , yOpen :: Float
  , yHigh :: Float
  , yLow :: Float
  , yClose :: Float
  , yAdjClose :: !Float
  , yVolume :: Float
  }

instance FromNamedRecord YahooData where
  parseNamedRecord m =
    YahooData <$> m .: "Date" <*> m .: "Open" <*> m .: "High" <*> m .: "Low" <*>
    m .:
    "Close" <*>
    m .:
    "Adj Close" <*>
    m .:
    "Volume"

main :: IO ()
main = do
  o :: Opts <- getRecord "online-dev example"
  case o of
    FromCsvFile f -> do
      d <- fromFile f
      case d of
        Left e -> putStrLn $ "something happened" <> e
        Right xs -> do
          putStrLn $ "data points: " <> (show $ length xs :: Text)
          encodeFile "other/data.bin" xs

    Run x -> do
        let nall = 20000
        let n = 500
        let rs = [0.95, 0.99, 0.996]
        dates <- chartDates nall (fromMaybe n $ grain $ fromMaybe defaultRunConfig x)
        vs <- fmap snd <$> getData nall
        let putes =
                [ ("c1", "moving average", L.scan . ma)
                , ("c2", "std deviation",  L.scan . std)
                , ("c3", "beta on ma",
                   \r xs -> drop 2 $ L.scan (beta (ma r)) $ drop 2 $ zip xs (L.scan (ma r) xs))
                , ("c4", "alpha on ma",
                   \r xs -> L.scan (alpha (ma r)) $ drop 2 $ zip xs (L.scan (ma r) xs))
                , ("c5", "beta on std",
                   \r xs -> drop 2 $ L.scan (beta (ma r)) $ drop 2 $ zip xs (L.scan (std r) xs))
                , ("c6", "alpha on std",
                   \r xs -> L.scan (alpha (ma r)) $ drop 2 $ zip xs (L.scan (std r) xs))
                ]
        run1 dates vs n rs putes

run1 :: [(Int, Text)] -> [Double] -> Int -> [Double] -> [(FilePath,Text,Double -> [Double] -> [Double])] -> IO ()
run1 dates vs n rs putes = sequence_ $ run' vs n rs <$> putes
  where
    run' vs n rs (f, title, sc) = do
        let res = scanData vs n rs sc
        write ("other/"<>f<>".svg") (Point 600 300) (ch1 title rs dates res)

fromFile :: FilePath -> IO (Either String [(Text, Float)])
fromFile f = do
  s <- B.readFile f
  case decodeByName s of
    (Left e) -> pure $ Left $ "csv decode failed: " <> e
    (Right (_, v)) -> do
      let extract x = (yDate x, yAdjClose x)
      let xs :: [(Text, Float)] = V.toList $ V.map extract v
      pure $ Right xs

-- https://finance.yahoo.com/quote/%5EGSPC/history?period1=-631015200&period2=1497103200&interval=1d&filter=history&frequency=1d
-- compute the log return from a price series
-- returns are geometric by nature, and using log(1+daily return) as the base unit of the time series leads to all sorts of benefits, not least of which is you can then add up the variates to get the cumulative return, without having to go through log and exp chicanery.  Ditto for distributional assumptions.
lret :: [(Text, Double)] -> [(UTCTime, Double)]
lret [] = []
lret [_] = []
lret ((_, v0):xs) = L.fold (L.Fold step ([], v0) (reverse . fst)) xs
  where
    step (acc, v) (d', v') = case parseUTCTime d' of
      Just d -> ((d, log (v' / v)) : acc, v')
      Nothing -> (acc, v)

taker :: Int -> [a] -> [a]
taker n = reverse . take n . reverse

getData :: Int -> IO [(UTCTime, Double)]
getData n = taker n . either (const []) lret <$> decodeFileOrFail "other/data.bin"

(<&>) :: (Functor t) => t (a -> b) -> a -> t b
fs <&> a = fmap (\f -> f a) fs

lastOnes :: (Eq a) => (a -> a -> Bool) -> [a] -> [a]
lastOnes _ [] = []
lastOnes _ [x] = [x]
lastOnes f (x:xs) = L.fold (L.Fold step (x,[]) (\(x0,x1) -> reverse $ x0:x1)) xs
  where
    step (a0, rs) a1 = if f a0 a1 then (a1,rs) else (a1,a0:rs)

chartDates :: Int -> Int -> IO [(Int,Text)]
chartDates alln n = do
  xs <- getData alln
  let dates = taker n $ fst <$> xs
  let (ticks,_) = placedTimeLabelDiscontinuous PosInnerOnly Nothing 8 dates
  let ticks' = lastOnes (\(_,x0) (_,x1) -> x0==x1) ticks
  pure ticks'

scanData :: [Double] -> Int -> [Double] -> (Double -> [Double] -> [Double]) -> [[Point Double]]
scanData xs n rs sc =
  zipWith Point [0 ..] . taker n . drop 1 <$> (sc <$> rs) <&> xs

lopts :: [LineStyle]
lopts =
  P.zipWith (\w c -> defaultLineStyle & #color .~ c & #width .~ w)
  [0.001 :: Double, 0.001, 0.001]
  [ PixelRGB8 197 140 75
  , PixelRGB8 60 127 43
  , PixelRGB8 56 42 140
  ]

ch1 :: Text -> [Double] -> [(Int,Text)] -> [[Point Double]] -> ChartSvg Double
ch1 title rs ticks' chartd =
  hudSvg one [] chart'
  where
    chart' = zipWith (\l c -> Chart (LineA l) mempty c) lopts (zeroLine : (fmap SpotPoint <$> chartd))
    zeroLine = SpotPoint <$> [Point lx 0, Point ux 0]
    (Ranges rx@(Range lx ux) _) = undefined -- getRect $ mconcat $ fmap toArea <$> fmap SpotPoint <$> chartd

fore2 :: (Field a, Floating a, Multiplicative a, Subtractive a, Additive a) => a -> a -> [a] -> [Pair a]
fore2 r1 r0 xs =
  L.scan
    ((\b o a r -> (Pair (a + b * o) r)) <$> beta (ma r1) <*>
     L.premap snd (ma 0.00001) <*>
     alpha (ma r1) <*>
     L.premap fst (ma 0.00001)) $
  drop 2 $
  zip xs (L.scan (ma r0) xs)

costAbs ::
     (List.ListLike (f a) a, Fractional a, Foldable f, Applicative f)
  => f a
  -> f a
  -> f a
  -> f (f a)
  -> a
costAbs ms c ys xss = av
  where
    av = (P./ (P.fromIntegral $ length ys)) (L.fold L.sum $ P.abs <$> es)
    es = ys ~-~ (L.fold L.sum <$> (c ~+ (ms ~* xss)))

(~*) :: (Num a, Applicative f) => f a -> f (f a) -> f (f a)
(~*) ms xss = ((<$>) . (P.*)) <$> ms <*> xss

(~+) :: (Num a, Applicative f) => f a -> f (f a) -> f (f a)
(~+) ms xss = ((<$>) . (P.+)) <$> ms <*> xss

(~-~) :: (List.ListLike (f a) a, Num a) => f a -> f a -> f a
(~-~) = List.zipWith (P.-)

costScan ::
     (Reifies s Tape) => [Double] -> [Reverse s Double] -> Reverse s Double
costScan xs (m:c:_) =
  costAbs
    [m]
    [c]
    (auto <$> drop 1 xs)
    [fmap auto <$> drop 1 $ L.scan (ma 0.99) xs]
costScan xs [] =
  costAbs
    []
    []
    (auto <$> drop 1 xs)
    [fmap auto <$> drop 1 $ L.scan (ma 0.99) xs]
costScan xs [m] =
  costAbs
    [m]
    []
    (auto <$> drop 1 xs)
    [fmap auto <$> drop 1 $ L.scan (ma 0.99) xs]

grid ::
     (NumHask.Prelude.Field b, Subtractive b, FromInteger b, Fractional b)
  => Range b
  -> Int
  -> [b]
grid (Range l u) steps =
  (\a -> l + (u - l) / fromIntegral steps * a) . fromIntegral <$> [0 .. steps]

locs0 :: Range Double -> Range Double -> Int -> [Pair Double]
locs0 rx ry steps =
  [Pair x y | x <- Main.grid rx steps, y <- Main.grid ry steps]

gradF ::
     (forall s. (Reifies s Tape) =>
                  [Reverse s Double] -> Reverse s Double)
  -> Double
  -> Pair Double
  -> Pair Double
gradF f step (Pair x y) =
  -1 *.
  (\[x', y'] -> Pair x' y')
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
    hudSvg one [] [pos]
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

{-


basic stats
---

online mean and std at a 0.99 decay rate:

-}
    let st = drop 1 $ L.scan ((,) <$> ma 0.9 <*> std 0.99) xs
    fileSvg "other/moments.svg" (300,300) (withChart def (lineChart [LineConfig 0.002 (Color 0.33 0.33 0.33 0.4), LineConfig 0.002 (Color 0.88 0.33 0.12 0.4)])
        [ zipWith V2 [0..] (fst <$> st)
        , zipWith V2 [0..] (snd <$> st)
        ])
{-
scan of 1000 recent ma 0.99 and std 0.99, in basis points, rendered as a scatter chart.

-}
    fileSvg "other/scatter.svg" (500,500) $
        withChart def (scatterChart [def]) [drop (length xs - 1000) $
        fmap (10000*) <$> L.scan (V2 <$> ma 0.99 <*> std 0.99) xs]


{-
quantiles
---

-}
    writeFile "other/quantiles.md" $
        "\n    [min, 10th, 20th, .. 90th, max]:\n" <>
        mconcat (sformat (" " % prec 3) <$> toList
                 (L.fold (tDigestQuantiles $ (0.1*) <$> [0..10]) xs)) <>
        "\n    online [min, 10th, 20th, .. 90th, max] with decay rate = 0.996 (one year)\n" <>
        mconcat (sformat (" " % prec 3) <$> toList
                 (L.fold (onlineQuantiles 0.996 $ (0.1*) <$> [0..10]) xs))
{-

digitize
---

-}
    writeFile "other/digitize.md" $
        "\n    first 100 values digitized into quantiles:" <>
        mconcat ((sformat (" " % prec 3) <$>)
                 (take 100 $ L.scan (onlineDigitize 0.996 $ (0.1*) <$> [0..10]) xs))

    fileSvg "other/scratchpad.png" (400,400) $ withChart def (lineChart [def])
        [ zipWith V2 [0..] (L.scan L.sum xs)
        , zipWith V2 [0..] ((2*) <$> L.scan L.sum xs)]

{-
regression
===

-}

-}
toHistogramWithCuts :: [Double] -> Maybe (NonEmpty HistBin) -> Histogram
toHistogramWithCuts cuts Nothing = Histogram cuts mempty
toHistogramWithCuts cuts (Just bins) =
  L.fold (L.Fold step0 (Histogram cuts mempty) done0) bins
  where
    step0 _ HistBin {} = undefined -- insertW h ((l+u)/2) v
    done0 = identity

toHistogram :: Maybe (NonEmpty HistBin) -> Histogram
toHistogram Nothing = Histogram mempty mempty
toHistogram h@(Just bins) = toHistogramWithCuts cuts h
  where
    bins' = toList bins
    cuts =
      ((\(HistBin l _ _ _ _) -> l) <$> bins') <>
      [(\(HistBin _ u _ _ _) -> u) $ Data.List.NonEmpty.last bins]
