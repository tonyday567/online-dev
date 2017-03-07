{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

import Chart hiding (Vector)
import Control.Monad.Primitive (unsafeInlineIO)
import Data.List hiding (lines)
import Data.Reflection
import Formatting
import Numeric.AD
import Numeric.AD.Internal.Reverse
import Numeric.AD.Internal.Sparse hiding (pack)
import Online hiding (p2)
import Tower.Prelude hiding ((%), (&))
import qualified Control.Foldl as L
import qualified Data.ListLike as List
import qualified Protolude as P

{-
cassava
---

csv data arrives as a bytestring, gets decoded as a Vector, and decoding errors arrive as strings, so there's a fair bit of messiness working with Text Lists.

-}

import Data.Csv
import GHC.Base (String)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8Builder)
import Data.ByteString.Builder (toLazyByteString)
import Data.Vector (Vector)

data YahooRep = YahooRep
  { date :: ByteString
  , open :: ByteString
  , high :: ByteString
  , low :: ByteString
  , close :: ByteString
  , volume :: ByteString
  , adjustedClose :: !Double
  } deriving Generic

instance FromRecord YahooRep

{-
The base unit for analysis (which I've called ys to abstract) is log(1+return).  Returns are geometric by nature, and this premap removes the effect before we get to distributions.
-}

vs :: [Double]
vs = fmap (\x -> log (1+x)) $ ret $ reverse $ unsafeInlineIO $ do
    bs <- readFile "other/YAHOO-INDEX_GSPC.csv"
    let rawdata =
            decode HasHeader (toLazyByteString $ encodeUtf8Builder bs)
            :: Either String (Vector YahooRep)
    case rawdata of
        (Left e) -> panic $ pack e
        (Right xs) -> pure $ adjustedClose <$> toList xs

ret :: [Double] -> [Double]
ret [] = []
ret [_] = []
ret xs = L.fold dif xs
    where
        dif = L.Fold step ([], Nothing) fst
        step x a = case snd x of
            Nothing -> ([], Just a)
            Just a' -> ((a-a')/a':fst x, Just a)

main :: IO ()
main = do
    fileSvg "other/elems.svg" (300,300)
        ( withChart
          (chartAxes . element 0 . axisTickStyle .~ TickNone $ def)
          (hists [def])
          [zipWith4 V4 [0..] (replicate 2000 0) [1..] (take 2000 vs)])

    fileSvg
        "other/asum.svg" (300,300)
        ( withChart def
          (lines [LineConfig 0.002 (Color 0.33 0.33 0.33 0.4)])
          [zipWith V2 [0..] (L.scan L.sum vs)]
        )
    fileSvg "other/cmean.svg" (300,300) $
        withChart def
        (lines
         [ LineConfig 0.002 (Color 0.88 0.33 0.12 1)
         , LineConfig 0.002 (Color 0.12 0.33 0.83 1)
         ])
        [ zipWith V2 [0..] $
          take 5000 $ drop 100 $ drop 2 $
          L.scan (beta 0.99) $ drop 1 $
          zip vs (L.scan (ma 0.9975) vs)
        , zipWith V2 [0..] $
          take 5000 $ drop 100 $ drop 2 $
          L.scan (alpha 0.99) $ drop 1 $
          zip vs (L.scan (ma 0.9975) vs)
        ]

    let cmeane =
         [ toV4 $ L.fold (Online.hist (rangeCuts 6 (-0.03) 0.03) 1) $
          take 5000 $ drop 100 $
          L.scan ((\r b o a -> r - b * o - a) <$>
             L.premap fst (ma 0.00001) <*>
             beta 0.99 <*>
             L.premap snd (ma 0.00001) <*>
             alpha 0.99) $
           drop 400 $ zip vs (L.scan (ma 0.9975) vs)
         , toV4 $ L.fold (Online.hist (rangeCuts 6 (-0.03) 0.03) 1) $
           take 5000 $ drop 100 vs
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
           drop 100 $ zip ((**2)<$> vs) (L.scan (sqma 0.9975) vs))

    fileSvg "other/arrows.svg" (600,600) (chartArrow # pad 1.1)

{-


basic stats
---

online mean and std at a 0.99 decay rate:

-}
    let st = drop 1 $ L.scan ((,) <$> ma 0.9 <*> std 0.99) vs
    fileSvg "other/moments.svg" (300,300) (withChart def (lines [LineConfig 0.002 (Color 0.33 0.33 0.33 0.4), LineConfig 0.002 (Color 0.88 0.33 0.12 0.4)])
        [ zipWith V2 [0..] (fst <$> st)
        , zipWith V2 [0..] (snd <$> st)
        ])
{-
scan of 1000 recent ma 0.99 and std 0.99, in basis points, rendered as a scatter chart.

-}
    fileSvg "other/scatter.svg" (500,500) $
        withChart def (scatters [def]) [drop (length vs - 1000) $
        fmap (10000*) <$> L.scan (V2 <$> ma 0.99 <*> std 0.99) vs]
{-

A histogram with r=0.99 with lifetime stats as the grey background

-}
    let cuts = (rangeCuts 5 (-0.02) 0.02)
    let h = toV4 $ freq $ L.fold (Online.hist cuts 0.99) vs
    let h' = toV4 $ freq $ L.fold (Online.hist cuts 1) vs
    fileSvg "other/hist.svg" (300,300) $
      withChart
      (chartRange .~ Just (rangeRects [h, h']) $ chartAxes .~ [def] $ def)
      (hists [def, rectBorderColor .~ Color 0 0 0 0
       $ rectColor .~ Color 0.333 0.333 0.333 0.1
       $ def])
      [h, h']
{-
quantiles
---

-}
    writeFile "other/quantiles.md" $
        "\n    [min, 10th, 20th, .. 90th, max]:" <>
        mconcat (sformat (" " % prec 3) <$> toList
                 (L.fold (quantiles' 11) vs)) <>
        "\n    online [min, 10th, 20th, .. 90th, max] with decay rate = 0.996 (one year)" <>
        mconcat (sformat (" " % prec 3) <$> toList
                 (L.fold (quantiles 11 identity 0.996) vs))
{-

digitize
---

-}
    writeFile "other/digitize.md" $
        "\n    first 100 values digitized into quantiles:" <>
        mconcat ((sformat (" " % prec 3) <$>)
                 (take 100 $ L.scan (digitize 5 identity 0.996) vs))

    fileSvg "other/scratchpad.png" (400,400) $ withChart def (lines [def])
        [ zipWith V2 [0..] (L.scan L.sum vs)
        , zipWith V2 [0..] ((2*) <$> L.scan L.sum vs)]

{-
regression
===

-}

cost_ ::
    ( List.ListLike (f a) a
    , Fractional a
    , Foldable f
    , Applicative f) =>
    f a -> f a -> f a -> f (f a) -> a
cost_ ms c ys xss = av
  where
    av = (P./( fromIntegral $ length ys)) (L.fold L.sum $ P.abs <$> es)
    es = ys Main..-. (L.fold L.sum <$> (c Main..+ (ms Main..* xss)))


(.*) :: (Num a, Applicative f) => f a -> f (f a) -> f (f a)
(.*) ms xss = ((<$>) . (P.*)) <$> ms <*> xss

(.+) :: (Num a, Applicative f) => f a -> f (f a) -> f (f a)
(.+) ms xss = ((<$>) . (P.+)) <$> ms <*> xss

(.-.) :: (List.ListLike (f a) a, Num a) => f a -> f a -> f a
(.-.) xs xs' = List.zipWith (P.-) xs xs'

(.+.) :: (List.ListLike (f a) a, Num a) => f a -> f a -> f a
(.+.) xs xs' = List.zipWith (P.+) xs xs'


costD :: Double -> Double -> Double
costD m c = cost_ [m] [c] (drop 2 vs) [drop 1 $ L.scan (ma 0.99) vs]

costF :: AD s Double -> AD s Double -> AD s Double
costF m c = cost_ [m] [c] (auto <$> drop 2 vs) [fmap auto <$> drop 1 $ L.scan (ma 0.99) vs]

costS :: AD s (Sparse Double) -> AD s (Sparse Double) -> AD s (Sparse Double)
costS m c = cost_ [m] [c] (auto <$> drop 2 vs) [fmap auto <$> drop 1 $ L.scan (ma 0.99) vs]

costR :: (Reifies s Tape) => Reverse s Double -> Reverse s Double -> Reverse s Double
costR m c = cost_ [m] [c] (auto <$> drop 2 vs) [fmap auto <$> drop 1 $ L.scan (ma 0.99) vs]

cost :: forall a. (Scalar a ~ Double, Mode a, Fractional a) => a -> a -> a
cost m c = cost_ [m] [c] (auto <$> drop 2 vs) [fmap auto <$> drop 1 $ L.scan (ma 0.99) vs]

costD' :: [Double] -> Double
costD' (m:c:_) = cost_ [m] [c] (drop 2 vs) [drop 1 $ L.scan (ma 0.99) vs]

costF' :: [AD s Double] -> AD s Double
costF' (m:c:_) = cost_ [m] [c] (auto <$> drop 2 vs) [fmap auto <$> drop 1 $ L.scan (ma 0.99) vs]

costS' :: [AD s (Sparse Double)] -> AD s (Sparse Double)
costS' (m:c:_) = cost_ [m] [c] (auto <$> drop 2 vs) [fmap auto <$> drop 1 $ L.scan (ma 0.99) vs]

costR' :: (Reifies s Tape) => [Reverse s Double] -> Reverse s Double
costR' (m:c:_) = cost_ [m] [c] (auto <$> drop 2 vs) [fmap auto <$> drop 1 $ L.scan (ma 0.99) vs]

cost' :: forall a. (Scalar a ~ Double, Mode a, Fractional a) => [a] -> a
cost' (m:c:_) = cost_ [m] [c] (auto <$> drop 2 vs) [fmap auto <$> drop 1 $ L.scan (ma 0.99) vs]

costR'' :: forall a. (Mode a, Fractional (Scalar a), Fractional a) => [Scalar a] -> [a] -> a
costR'' vs' (m:c:_) = cost_ [m] [c] (auto <$> drop 2 vs') [fmap auto <$> drop 1 $ L.scan (ma 0.99) vs']

converge :: (Ord a, Num a) => a -> Int -> [[a]] -> Maybe [a]
converge _ _ [] = Nothing
converge epsilon n (x:xs) = Just $ go x xs (0::Int)
  where
    go x [] _ = x
    go x (x':xs) i
        | dist x x' < epsilon = x'
        | i >= n = x'
        | otherwise = go x' xs (i+1)
    dist a b = L.fold L.sum $ P.abs <$> zipWith (P.-) a b

untilConverged' :: (Ord a, Num a) => a -> Int -> [[a]] -> [[a]]
untilConverged' _ _ [] = []
untilConverged' epsilon n (x:xs) = go x xs (0::Int) []
  where
    go _ [] _ res = res
    go x0 (x':xs0) i res
        | dist x0 x' < epsilon = reverse res
        | i >= n = reverse res
        | otherwise = go x' xs0 (i+1) (x':res)
    dist a b = L.fold L.sum $ P.abs <$> zipWith (P.-) a b

until :: (a -> a -> Bool) -> Int -> [a] -> [a]
until _ _ [] = []
until p n (x:xs) = go x xs (0::Int) []
  where
    go _ [] _ res = res
    go x0 (x':xs0) i res
        | p x0 x' = reverse res
        | i >= n = reverse res
        | otherwise = go x' xs0 (i+1) (x':res)

untilConverged :: (Ord a, Num a) => a -> Int -> [[a]] -> [[a]]
untilConverged _ _ [] = []
untilConverged epsilon n xs = until
  (\a b -> L.fold L.sum (P.abs <$> zipWith (P.-) a b) < epsilon)
  n
  xs

grid :: forall b. (Fractional b, Enum b) => Range b -> b -> [b]
grid (Range (x,x')) steps = (\a -> x P.+ (x' P.- x) P./ steps P.* a) <$> [0..steps]

locs :: Range Double -> Range Double -> Double -> [(Double, Double)]
locs rx ry steps = [(x, y) | x <- grid rx steps, y <- grid ry steps] :: [(Double,Double)]

dir ::
    (forall s. (Reifies s Tape) => [Reverse s Double] -> Reverse s Double) ->
    Double ->
    (Double, Double) ->
    V2 Double
dir f step (x, y) =
    - r2 ((\[x,y] -> (x,y)) $
          gradWith (\x x' -> x + (x' - x) * step) f [x,y])

locs0 :: Range Double -> Range Double -> Double -> [V2 Double]
locs0 rx ry steps = [V2 x y | x <- grid rx steps, y <- grid ry steps]

gradF ::
    (forall s. (Reifies s Tape) => [Reverse s Double] -> Reverse s Double) ->
    Double ->
    V2 Double ->
    V2 Double
gradF f step (V2 x y) =
    - r2 ((\[x',y'] -> (x',y')) $
          gradWith (\x0 x1 -> x0 + (x1 - x0) * step) f [x,y])

arrowData ::
    (forall s. (Reifies s Tape) => [Reverse s Double] -> Reverse s Double) ->
    [V2 Double] ->
    Double ->
    [V4 Double]
arrowData f pos step = zipWith (\(V2 x y) (V2 z w) -> V4 x y z w) pos (gradF f step <$> pos)


chartArrow :: Chart' a
chartArrow = arrows [def] (V4 (Range (-0.5,0.5)) (Range (-0.5,0.5)) (Range (-0.5,0.5)) (Range (-0.5,0.5))) [d] <> axes (chartAspect .~ asquare $ chartRange .~ Just (rangeR2s [pos]) $ def)
  where
    d = zipWith (\(V2 x y) (V2 z w) -> V4 x y z w) pos (gradF costR' step <$> pos)
    pos = locs0 (Range (-0.004, 0.001)) (Range (-0.005, 0.005)) 10
    step = 0.01

extrap :: (Double, [Double]) -> (Double, [Double])
extrap (eta0, x0) = expand eta0 x0
  where
    (res0,_) = grad' costR' x0
    contract eta x
        | res' < res0 = (eta, x')
        | otherwise = contract (eta/2) x'
      where
        x' :: [Double]
        x' = x Main..-. ((eta *) <$> g)
        (_,g) = grad' costR' x
        (res',_) = grad' costR' x'
    expand eta x
        | res' < res0 = expand (eta*2) x'
        | otherwise = contract (eta/2) x
      where
        x' :: [Double]
        x' = x Main..-. ((eta *) <$> g)
        (_,g) = grad' costR' x
        (res',_) = grad' costR' x'
