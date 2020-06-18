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

module Run.Random
  ( RandomConfig(..),
    defaultRandomConfig,
    RandomSets(..),
    makeRandomSets,
    repRandomConfig,
    randomCharts,
  )
where

import Chart
import Control.Lens hiding ((:>), (<&>), Unwrapped, Wrapped)
import Control.Monad
import Data.Generics.Labels ()
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as Text
import NumHask.Prelude hiding (fold, (<<*>>), asum)
import Data.Mealy
import Data.Random
import Data.List ((!!))
import Web.Page hiding (StateT(..), State, state, get, bool, runState)
import System.Random.MWC
import Data.Vector (Vector)
import Run.Types

data RandomConfig =
  RandomConfig
  { seed :: [Word32],
    n :: Int,
    nSets :: Int,
    corr :: Double
  } deriving (Eq, Show, Generic)

defaultRandomConfig :: RandomConfig
defaultRandomConfig = RandomConfig [42] 10000 3 0.8

data RandomSets =
  RandomSets
  { rs :: [[Double]],
    rsp :: [(Double, Double)]
  } deriving (Eq, Show, Generic)

makeRandomSets :: RandomConfig -> IO RandomSets
makeRandomSets c = do
  g <- initialize ((fromList $ c ^. #seed) :: Vector Word32)
  rs <- sequence $ replicate (c ^. #nSets) (rvs g (c ^. #n))
  rsp <- rvsp g  (c ^. #n) (c ^. #corr)
  pure $ RandomSets rs rsp

-- | representation of configuration
repRandomConfig :: (Monad m) => RandomConfig -> SharedRep m RandomConfig
repRandomConfig cfg = bimap hmap RandomConfig seed' <<*>> n' <<*>> nsets' <<*>> corr'
  where
    seed' = either (const (view #seed cfg)) id <$>
      readTextbox (Just "seed") (view #seed cfg)
    n' = either (const (view #n cfg)) id <$>
      readTextbox (Just "n") (view #n cfg)
    nsets' = either (const (view #nSets cfg)) id <$>
      readTextbox (Just "nsets") (view #nSets cfg)
    corr' = either (const (view #corr cfg)) id <$>
      readTextbox (Just "n") (view #corr cfg)
    hmap a b c d = a <> b <> c <> d

randomCharts :: SvgOptions -> RandomSets -> Map.Map Text Text
randomCharts svgo rs = Map.fromList
  [ ("walk0",
     renderHudOptionsChart
      svgo
      (titlesHud "random walk" "n" "accumulated value")
      []
      (scanChart (const asum) [1] 0 ((rs ^. #rs) !! 0))
    )
  , ("walks",
     renderHudOptionsChart
      svgo
      (titlesHud "random walks" "n" "accumulated value" &
           #hudLegend .~ Just
      ( defaultLegendOptions
          & #ltext . #size .~ 0.2
          & #lplace .~ PlaceAbsolute (Point 0.3 (-0.3))
          & #legendFrame .~ Just (RectStyle 0.02 (palette !! 5) white),
        zipWith
          (\a r -> (LineA a, ("rw: " <>) . Text.pack . show $ r))
          ls
          (take (length $ rs ^. #rs) [(0::Int)..])
      ))
      []
      (zipWith (\l xs -> Chart (LineA l) (xs)) ls ((zipWith SP [0..] . scan asum) <$> ((rs ^. #rs))))
      ),
    ("correlated walks",
     renderHudOptionsChart
      svgo
      (titlesHud "correlated random walks" "n" "accumulated value" &
           #hudLegend .~ Just
      ( defaultLegendOptions
          & #ltext . #size .~ 0.2
          & #lplace .~ PlaceAbsolute (Point 0.3 (-0.3))
          & #legendFrame .~ Just (RectStyle 0.02 (palette !! 5) white),
        zipWith
          (\a r -> (LineA a, ("rw: " <>) . Text.pack . show $ r))
          ls
          (take (length $ rs ^. #rs) [(0::Int)..])
      ))
      []
      (zipWith (\l xs -> Chart (LineA l) (xs)) ls ((zipWith SP [0..] . scan asum) <$> transpose (((\(x,y) -> [x,y])) <$> (rs ^. #rsp))))
      )
    ]

