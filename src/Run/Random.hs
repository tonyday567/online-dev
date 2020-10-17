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
  ( RandomConfig (..),
    defaultRandomConfig,
    RandomSets (..),
    makeRandomSets,
    repRandomConfig,
    randomChartsNames,
    randomCharts,
  )
where

import Chart
import Chart.Mealy hiding (taker)
import Chart.Various
import Control.Lens hiding ((:>), (<&>), Unwrapped, Wrapped)
import Control.Monad
import Data.Generics.Labels ()
import qualified Data.HashMap.Strict as HashMap
import Data.List ((!!))
import Data.Maybe
import Data.Mealy
import Data.Mealy.Simulate
import Data.Vector (Vector)
import NumHask.Prelude hiding (asum, fold)
import System.Random.MWC
import Web.Rep

data RandomConfig
  = RandomConfig
      { seed :: [Word32],
        n :: Int,
        nSets :: Int,
        corr :: Double
      }
  deriving (Eq, Show, Generic)

defaultRandomConfig :: RandomConfig
defaultRandomConfig = RandomConfig [42] 10000 3 0.8

data RandomSets
  = RandomSets
      { rs :: [[Double]],
        rsp :: [(Double, Double)]
      }
  deriving (Eq, Show, Generic)

makeRandomSets :: RandomConfig -> IO RandomSets
makeRandomSets c = do
  g <- initialize ((fromList $ c ^. #seed) :: Vector Word32)
  rs <- replicateM (c ^. #nSets) (rvs g (c ^. #n))
  rsp <- rvsp g (c ^. #n) (c ^. #corr)
  pure $ RandomSets rs rsp

-- | representation of configuration
repRandomConfig :: (Monad m) => RandomConfig -> SharedRep m RandomConfig
repRandomConfig cfg = bimap hmap RandomConfig seed' <<*>> n' <<*>> nsets' <<*>> corr'
  where
    seed' =
      either (const (view #seed cfg)) id
        <$> readTextbox (Just "seed") (view #seed cfg)
    n' =
      either (const (view #n cfg)) id
        <$> readTextbox (Just "n") (view #n cfg)
    nsets' =
      either (const (view #nSets cfg)) id
        <$> readTextbox (Just "nsets") (view #nSets cfg)
    corr' =
      either (const (view #corr cfg)) id
        <$> readTextbox (Just "n") (view #corr cfg)
    hmap a b c d = a <> b <> c <> d

randomChartsNames :: [Text]
randomChartsNames = ["walk0", "walks", "correlated walks"]

randomCharts :: RandomSets -> HashMap.HashMap Text (HudOptions, [Chart Double])
randomCharts rs =
  HashMap.fromList
    [ ( "walk0",
        ( titlesHud "random walk" "n" "accumulated value",
          scanChart (const asum) [1] 0 ((rs ^. #rs) !! 0)
        )
      ),
      ( "walks",
        ( titlesHud "random walks" "n" "accumulated value"
            & #hudLegend ?~ leg,
          zipWith
            (\l xs -> Chart (LineA l) xs)
            (stdLines 0.005)
            (fmap PointXY . xify . scan asum <$> (rs ^. #rs))
        )
      ),
      ( "correlated walks",
        ( titlesHud "correlated random walks" "n" "accumulated value"
            & #hudLegend ?~ leg,
          zipWith
            (\l xs -> Chart (LineA l) xs)
            (stdLines 0.005)
            ( fmap PointXY . xify . scan asum
                <$> transpose ((\(x, y) -> [x, y]) <$> (rs ^. #rsp))
            )
        )
      )
    ]
  where
    leg =
      lineLegend
        0.01
        ((("rw: " :: Text) <>) . show <$> (rs ^. #rs))
        palette1
