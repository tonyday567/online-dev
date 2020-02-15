{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


{-

Following:

https://www.tweag.io/posts/2019-09-20-monad-bayes-1.html | Tweag I/O - Probabilistic Programming with monad‑bayes, Part 1: First Steps
https://www.tweag.io/posts/2019-11-08-monad-bayes-2.html | Tweag I/O - Probabilistic Programming with monad‑bayes, Part 2: Linear Regression
https://www.tweag.io/posts/2019-10-25-mcmc-intro1.html | Tweag I/O - Markov chain Monte Carlo (MCMC) Sampling, Part 1: The Basics
https://github.com/adscib/monad-bayes/tree/master/models | monad-bayes/models at master · adscib/monad-bayes

-}

module Main where

import Data.Yahoo
import Protolude
import Control.Lens
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Inference.SMC as SMC
import Control.Monad.Bayes.Inference.RMSMC as RMSMC
import Control.Monad.Bayes.Sequential
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Traced.Static (Traced)
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Class
import Data.List
import qualified Data.Map.Strict as Map
import Chart
import Chart.Bar
import Numeric.Log

model1 :: MonadSample m => m Bool
model1 = uniformD [False, True]

fillD xs = foldl' (\m k -> Map.insertWith (+) k 1 m) Map.empty xs


model2 :: MonadInfer m => m Bool
model2 = do
    b <- uniformD [False, True]
    score (if b then 1.0 else 0.0)
    return b

model3 :: MonadInfer m => m (Double, Double)
model3 = do
    b <- uniform (-1) 1
    m <- uniform (-1) 1
    condition $ (b-m) > 0
    return (b, m)

model4 :: MonadInfer m => m (Double, Double)
model4 = do
    b <- uniform (-1) 1
    m <- uniform (-1) 1
    condition $ (b-m) > 0
    condition $ (b+m) > 0
    return (b, m)

pixelChart :: (Text, Text, Text) -> Rect Double -> [Chart Double] ->
  ChartSvg Double
pixelChart (title, xt, yt) r cs =
  hud
    ( defaultHudConfig
        & #hudTitles .~ [defaultTitle title, (defaultTitle xt :: Title Double) & #place .~ PlaceBottom & #style . #size .~ 0.06, (defaultTitle yt :: Title Double) & #place .~ PlaceLeft & #style . #size .~ 0.06]
    )
    r
    cs

-- pairModel :: MonadInfer m => (Text, Text, Text) -> Int -> m (Double, Double) -> IO ()
pairModel fn ts nsamples r p model = do
  modelsamples <- sampleIOfixed $ prior $ mh nsamples model
  let (Rect x z y w) = r
  let gx = grid OuterPos (Range x z) p
  let gy = grid OuterPos (Range y w) p
  let c = foldl' (\x a -> Map.insertWith (+) (bimap (cutI gx) (cutI gy) a) 1 x) Map.empty
          modelsamples
  let ps = (\(r', c') -> Chart (RectA (RectStyle 0 black 0 c' 1)) [SpotRect r']) <$> pixelate (\(Point x y) -> fromMaybe 0 $ Map.lookup (bimap (cutI gx) (cutI gy) (x,y)) c) r (Point 20 20) white blue
  writeChartSvgWith fn defaultChartSvgStyle (\x -> pixelChart ts x ps)

pairModel' fn ts r p f = do
  let (Rect x z y w) = r
  let gx = grid OuterPos (Range x z) p
  let gy = grid OuterPos (Range y w) p
  let ps = (\(r', c') -> Chart (RectA (RectStyle 0 black 0 c' 1)) [SpotRect r']) <$> pixelate f r (Point p p) white blue
  writeChartSvgWith fn defaultChartSvgStyle (\x -> pixelChart ts x ps)

paramsModel fn (t1, t2, t3) r p f params = do
  let (Rect x z y w) = r
  let gx = grid OuterPos (Range x z) p
  let gy = grid OuterPos (Range y w) p
  let ps p' = (\(r', c') -> Chart (RectA (RectStyle 0 black 0 c' 1)) [SpotRect r']) <$> pixelate (f p') r (Point p p) white blue
  writeChartSvgWith fn defaultChartSvgStyle $
    (\r ->
        hud
        ( mempty
          & #hudTitles .~ [defaultTitle t1, (defaultTitle t2 :: Title Double) & #place .~ PlaceBottom & #style . #size .~ 0.06, (defaultTitle t3 :: Title Double) & #place .~ PlaceLeft & #style . #size .~ 0.06]
        )
        r
        (stack 3 0.05 $ (ps <$> params)))

data Params
  = Params
      { slope :: Double,
        intercept :: Double,
        noiseStd :: Double
      }
  deriving (Eq, Show)

likelihood :: Params -> Point Double -> Log Double
likelihood (Params m b nStd) (Point x y) = normalPdf (m*x + b) nStd y

params0 = Params 2.3 (-1.2) 2.0

samplingDistribution' :: Params -> Point Double -> Double
samplingDistribution' p = exp . ln . likelihood p

priorParams :: MonadSample m => m Params
priorParams = do
  intercept <- uniform (-5) 5
  slope <- uniform (-5) 5
  noise <- uniform 1 3
  return $ Params slope intercept noise

-- likelihoodDist :: MonadInfer m => Params -> m Data
likelihoodDist params r = do
  let (Rect x' z y' w) = r
  x <- uniform x' z
  y <- uniform y' w
  score $ likelihood params (Point x y)
  return $ Point x y

scatterChart :: Text -> [Point Double] -> Rect Double ->
  ChartSvg Double
scatterChart title xs r =
  hud
    ( defaultHudConfig
        & #hudTitles .~ [defaultTitle title]
    )
    r
    [chart']
  where
    chart' = Chart (GlyphA defaultGlyphStyle) (SpotPoint <$> xs)

-- uniform2D :: MonadSample m => m Data
uniform2D (Rect x' z y' w) = do
  x <- uniform x' z
  y <- uniform y' w
  return $ Point x y

-- postParams :: MonadInfer m => m Params -> [Data] -> m Params
postParams pr obs = do
  param <- pr
  forM_ obs (\point -> score (likelihood param point))
  return param

-- predDist :: MonadInfer m => m Params -> m Data
predDist paramDist r = do
  params <- paramDist
  point <- likelihoodDist params r
  return point

main :: IO ()
main = do
  let nsamples = 1000
  samples <- sampleIOfixed $ replicateM nsamples model1
  pure ()
  let d = Map.toList $ fillD samples
  let b = BarData [snd <$> d] (Just $ show . fst <$> d) Nothing
  writeChartSvgWith "other/model1.svg" defaultChartSvgStyle $ (\x -> barChart x (defaultBarOptions []) b)
  samples <- sampleIOfixed $ prior $ mh nsamples model2
  let d = Map.toList $ fillD samples
  let b = BarData [snd <$> d] (Just $ show . fst <$> d) Nothing
  writeChartSvgWith "other/model2.svg" defaultChartSvgStyle $ (\x -> barChart x (defaultBarOptions []) b)
  pairModel "other/model3.svg" ("model3", "b", "m") nsamples (Rect (-1) 1 (-1) 1) 20 model3
  pairModel "other/model4.svg" ("model4", "b", "m") nsamples (Rect (-1) 1 (-1) 1) 20 model4
  -- part 2
  pairModel' "other/model5.svg" ("z", "x", "y") (Rect (-10) 10 (-10) 10) 20 (samplingDistribution' params0)
  params <- sampleIOfixed $ replicateM 9 priorParams
  paramsModel "other/ps.svg" ("z", "x", "y") (Rect (-10) 10 (-10) 10) 20
    samplingDistribution' params

  pointsMCMC <- sampleIOfixed $ prior . mh 1000 $ likelihoodDist params0 (Rect (-10) 10 (-10) 10)
  writeChartSvgWith "other/mcmc1.svg" defaultChartSvgStyle $ scatterChart "mcmc" pointsMCMC

  uniformSamples <- sampleIOfixed $ replicateM 2000 $ uniform2D (Rect (-10) 10 (-10) 10)
  let desiredProb = (samplingDistribution' params0) <$> uniformSamples

  uniform0max <- sampleIOfixed $ replicateM 2000 $ uniform 0 (maximum desiredProb)
  let points3 = [p | (p, u, l) <- zip3 uniformSamples uniform0max desiredProb, u<l]
  writeChartSvgWith "other/mcmc3.svg" defaultChartSvgStyle $ scatterChart "mcmc3" points3

  modelsamples <- sampleIOfixed $ prior . mh 1000 $ postParams priorParams points3
  writeChartSvgWith "other/modelsamples.svg" defaultChartSvgStyle $ scatterChart "modelsamples" ((\(Params a b _) -> Point a b) <$> modelsamples)

  pts <- sampleIOfixed $ prior . mh 40000 $ predDist (postParams priorParams points3) (Rect (-10) 10 (-10) 10)
  let predPoints = take (length pts - 100) pts
  writeChartSvgWith "other/pred.svg" defaultChartSvgStyle $ scatterChart "predPoints" predPoints

  pure ()
