{-# OPTIONS_GHC -Wall #-}

import Online

import Chart
import Data.Binary
import Data.List hiding (lines)
import Data.Quandl
import Data.Reflection
import Formatting
import Numeric.AD
import Numeric.AD.Internal.Reverse
import Options.Generic

import Protolude hiding ((%), (&), log, (+), (-), (*), (/), (**), fromIntegral, abs, one)
import Tower.Algebra.Num
import qualified Tower.Algebra as T
import qualified Control.Foldl as L
import qualified Data.Attoparsec.Text as A
import qualified Data.ListLike as List
import qualified Protolude as P
import Data.Default()

data Opts = Download | Run (Maybe RunConfig)
   deriving (Generic, Show)

data RunConfig = RunConfig { grain :: Maybe Int } deriving (Generic, Show, Read)

instance Default RunConfig where
    def = RunConfig Nothing

instance ParseRecord RunConfig

instance ParseFields RunConfig

instance ParseField RunConfig

instance ParseRecord Opts

main :: IO ()
main = do
   o :: Opts <- getRecord "online-dev examples"
   case o of
     Download -> do
         t <- downloadData
         case t of
           (Left e) -> putStrLn $ "download failed: " <> e
           (Right xs) -> do
               putStrLn $ "data points: " <> (show $ length xs :: Text)
               encodeFile "other/data.bin" xs
     Run Nothing -> join $ run def <$> getData
     Run (Just cfg) -> join $ run cfg <$> getData

-- | get data from this site:
-- https://www.quandl.com/data/YAHOO/INDEX_GSPC-S-P-500-Index
downloadData :: IO (Either Text [Double])
downloadData = do
    t <- getTable "YAHOO" "INDEX_GSPC" Nothing
    case t of
      Nothing -> pure $ Left "No data downloaded"
      Just t1 -> case elemIndex "Close" (daColumnNames t1) of
        Nothing -> pure $ Left "No Close column"
        Just i1 ->
            let d1 = (!! i1) <$> daData t1 in
            pure $ Right $
            ( either (const nan) identity
            . A.parseOnly (A.double <* A.endOfInput))
            <$> d1

-- | compute the log return from a price series (which comes in most recent to oldest order, so needs to be reversed)
-- returns are geometric by nature, and using log(1+daily return) as the base unit of the time series leads to all sorts of benefits, not least of which is you can then add up the variates to get the cumulative return, without having to go through log and exp chicanery.  Ditto for distributional assumptions.
lret :: [Double] -> [Double]
lret [] = []
lret [_] = []
lret xs = (\x -> log (1+x)) <$> L.fold dif (reverse xs)
    where
        dif = L.Fold step ([], Nothing) fst
        step x a = case snd x of
            Nothing -> ([], Just a)
            Just a' -> ((a-a')/a':fst x, Just a)

getData :: IO [Double]
getData = either (const []) lret <$> decodeFileOrFail "other/data.bin"

compareHistsChart :: DealOvers -> Histogram -> Histogram -> Chart' a
compareHistsChart o h1 h2 =
    let h = fromHist o h1
        h' = fromHist o h2
        h'' = zipWith (\(V4 x y z w) (V4 _ _ _ w') -> V4 x y z (w-w')) h h'
        flat = Aspect $ V2 (Range (-0.75,0.75)) (Range (-0.25,0.25))
    in
      pad 1.1 $ 
        beside (r2 (0,-1)) (hists
        [ def
        , rectBorderColor .~ Color 0 0 0 0
        $ rectColor .~ Color 0.333 0.333 0.333 0.1
        $ def ] sixbyfour [h,h'] <>
        axes (ChartConfig 1.1
              [def]
              (Just (rangeRects [h,h']))
              sixbyfour (uncolor transparent)))
        (hists
        [ rectBorderColor .~ Color 0 0 0 0
        $ rectColor .~ Color 0.888 0.333 0.333 0.8
        $ def ] flat [h''] <>
        axes (ChartConfig 1.1
              [ axisAlignedTextBottom .~ 0.65 $
                axisAlignedTextRight .~ 1 $
                axisOrientation .~ Y $
                axisPlacement .~ AxisLeft $
                def
              ]
              (Just (rangeRects [h'']))
              flat (uncolor transparent)))

run :: RunConfig -> [Double] -> IO ()
run cfg xs = do
    let gr = fromMaybe 20 (grain cfg)
    fileSvg
        "other/asum.svg" (300,300)
        ( withChart def
          (lines [LineConfig 0.002 (Color 0.33 0.33 0.33 0.4)])
          [zipWith V2 [0..] (L.scan L.sum xs)]
        )

    let cuts = rangeCuts 4 (-0.02) 0.02
    let h = toHistogramWithCuts cuts $ L.fold (onlineDigestHist 0.975) xs
    let h' = toHistogramWithCuts cuts $ L.fold tDigestHist xs
    fileSvg "other/hist.svg" (300,300) $ compareHistsChart (IncludeOvers 0.01) h h'

    let htotal = fromHist IgnoreOvers $ toHistogram $ L.fold tDigestHist xs    
    fileSvg "other/histtotal.svg" (300,300) $ pad 1.1
        (hists
        [ def
        , rectBorderColor .~ Color 0 0 0 0
        $ rectColor .~ Color 0.333 0.333 0.333 0.1
        $ def ] sixbyfour [htotal] <>
        axes (ChartConfig 1.1
              [ axisTickStyle .~ TickLabels (labelsFromCuts (IncludeOvers 0.01) cuts)
              $ def]
              (Just (rangeRects [htotal]))
              sixbyfour (uncolor transparent)))

    fileSvg "other/elems.svg" (300,300)
        ( withChart
          (chartAxes . element 0 . axisTickStyle .~ TickNone $ def)
          (hists [def])
          [zipWith4 V4 [0..] (replicate 2000 0) [1..] (take 2000 xs)])

    fileSvg "other/cmean.svg" (300,300) $
        withChart def
        (lines
         [ LineConfig 0.002 (Color 0.88 0.33 0.12 1)
         , LineConfig 0.002 (Color 0.12 0.33 0.83 1)
         ])
        [ zipWith V2 [0..] $
          take 5000 $ drop 100 $ drop 2 $
          L.scan (beta 0.99) $ drop 1 $
          zip xs (L.scan (ma 0.9975) xs)
        , zipWith V2 [0..] $
          take 5000 $ drop 100 $ drop 2 $
          L.scan (alpha 0.99) $ drop 1 $
          zip xs (L.scan (ma 0.9975) xs)
        ]

    let cmeane =
         [ fromHist IgnoreOvers $ L.fold (Online.hist (rangeCuts 6 (-0.03) 0.03) 1) $
          take 5000 $ drop 100 $
          L.scan ((\r b o a -> r - b * o - a) <$>
             L.premap fst (ma 0.00001) <*>
             beta 0.99 <*>
             L.premap snd (ma 0.00001) <*>
             alpha 0.99) $
           drop 400 $ zip xs (L.scan (ma 0.9975) xs)
         , fromHist IgnoreOvers $ L.fold (Online.hist (rangeCuts 6 (-0.03) 0.03) 1) $
           take 5000 $ drop 100 xs
         ]

    fileSvg "other/cmeane.svg" (300,300) $
        withChart (chartRange .~ Just (rangeRects cmeane) $ def)
        (hists
         [def, RectConfig 0 (Color 0.88 0.33 0.12 0) (Color 0.33 0.33 0.12 0.3)])
        cmeane

    fileSvg "other/csqma.svg" (300,300) $
        withChart def
        (lines
         [ LineConfig 0.002 (Color 0.88 0.33 0.12 1)
         , LineConfig 0.002 (Color 0.12 0.33 0.83 1)
         ])
        (fmap (zipWith V2 [0..]) <$> (\x -> [fst <$> x, snd <$> x]) $
         take 12000 $ drop 12000 $ drop 2 $
         L.scan ((,) <$> alpha 0.99 <*> beta 0.99) $
           drop 100 $ zip ((**2)<$> xs) (L.scan (sqma 0.9975) xs))

    fileSvg "other/arrows.svg" (600,600)
        (chartGrad (V2 T.one T.one) gr 0.01 xs # pad 1.1)

{-


basic stats
---

online mean and std at a 0.99 decay rate:

-}
    let st = drop 1 $ L.scan ((,) <$> ma 0.9 <*> std 0.99) xs
    fileSvg "other/moments.svg" (300,300) (withChart def (lines [LineConfig 0.002 (Color 0.33 0.33 0.33 0.4), LineConfig 0.002 (Color 0.88 0.33 0.12 0.4)])
        [ zipWith V2 [0..] (fst <$> st)
        , zipWith V2 [0..] (snd <$> st)
        ])
{-
scan of 1000 recent ma 0.99 and std 0.99, in basis points, rendered as a scatter chart.

-}
    fileSvg "other/scatter.svg" (500,500) $
        withChart def (scatters [def]) [drop (length xs - 1000) $
        fmap (10000*) <$> L.scan (V2 <$> ma 0.99 <*> std 0.99) xs]


{-
quantiles
---

-}
    writeFile "other/quantiles.md" $
        "\n    [min, 10th, 20th, .. 90th, max]:" <>
        mconcat (sformat (" " % prec 3) <$> toList
                 (L.fold (tDigestQuantiles $ (0.1*) <$> [0..10]) xs)) <>
        "\n    online [min, 10th, 20th, .. 90th, max] with decay rate = 0.996 (one year)" <>
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

    fileSvg "other/scratchpad.png" (400,400) $ withChart def (lines [def])
        [ zipWith V2 [0..] (L.scan L.sum xs)
        , zipWith V2 [0..] ((2*) <$> L.scan L.sum xs)]

{-
regression
===

-}

costAbs ::
    ( List.ListLike (f a) a
    , Fractional a
    , Foldable f
    , Applicative f
    , Ring a
    , Normed a a
    , MultiplicativeGroup a) =>
    f a -> f a -> f a -> f (f a) -> a
costAbs ms c ys xss = av
  where
    av = (/( fromIntegral $ length ys)) (L.fold L.sum $ abs <$> es)
    es = ys ~-~ (L.fold L.sum <$> (c ~+ (ms ~* xss)))

(~*) :: (Num a, Applicative f) => f a -> f (f a) -> f (f a)
(~*) ms xss = ((<$>) . (P.*)) <$> ms <*> xss

(~+) :: (Num a, Applicative f) => f a -> f (f a) -> f (f a)
(~+) ms xss = ((<$>) . (P.+)) <$> ms <*> xss

(~-~) :: (List.ListLike (f a) a, Num a) => f a -> f a -> f a
(~-~) xs xs' = List.zipWith (P.-) xs xs'

costScan :: (Reifies s Tape) => [Double] -> [Reverse s Double] -> Reverse s Double
costScan xs (m:c:_) = costAbs [m] [c] (auto <$> drop 1 xs) [fmap auto <$> drop 1 $ L.scan (ma 0.99) xs]
costScan xs [] = costAbs [] [] (auto <$> drop 1 xs) [fmap auto <$> drop 1 $ L.scan (ma 0.99) xs]
costScan xs [m] = costAbs [m] [] (auto <$> drop 1 xs) [fmap auto <$> drop 1 $ L.scan (ma 0.99) xs]

grid :: (Fractional b) => Range b -> Int -> [b]
grid (Range (l,u)) steps =
    (\a -> l + (u - l) / fromIntegral steps * a) .
    fromIntegral <$>
    [0..steps]

locs0 :: Range Double -> Range Double -> Int -> [V2 Double]
locs0 rx ry steps = [V2 x y | x <- Main.grid rx steps, y <- Main.grid ry steps]

gradF ::
    (forall s. (Reifies s Tape) => [Reverse s Double] -> Reverse s Double) ->
    Double ->
    V2 Double ->
    V2 Double
gradF f step (V2 x y) =
    - r2 ((\[x',y'] -> (x',y')) $
          gradWith (\x0 x1 -> x0 + (x1 - x0) * step) f [x,y])

addGrad ::
    (forall s. (Reifies s Tape) => [Reverse s Double] -> Reverse s Double) ->
    [V2 Double] ->
    Double ->
    [V4 Double]
addGrad f xys step =
    zipWith (\(V2 x y) (V2 z w) -> V4 x y z w) xys (gradF f step <$> xys)

chartGrad :: XY -> Int -> Double -> [Double] -> Chart' a
chartGrad (V2 rx ry) grain step xs =
    arrows [def] (V4 T.one T.one T.one T.one) [d] <> axes
    (chartAspect .~ asquare $ chartRange .~ Just (rangeR2s [pos]) $ def)
  where
    d = addGrad (costScan xs) pos step
    pos = locs0 rx ry grain

extrap :: [Double] -> (Double, [Double]) -> (Double, [Double])
extrap xs (eta0, x0) = expand eta0 x0
  where
    (res0,_) = grad' (costScan xs) x0
    contract eta x
        | res1 < res0 = (eta, x1)
        | otherwise = contract (eta/2) x1
      where
        x1 = x ~-~ ((eta *) <$> snd (grad' (costScan xs) x))
        res1 = fst $ grad' (costScan xs) x1
    expand eta x
        | res' < res0 = expand (eta*2) x'
        | otherwise = contract (eta/2) x
      where
        x' :: [Double]
        x' = x ~-~ ((eta *) <$> g)
        (_,g) = grad' (costScan xs) x
        (res',_) = grad' (costScan xs) x'
