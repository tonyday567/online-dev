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

module Run.Types where

import Chart
import Data.Generics.Labels ()
import NumHask.Prelude

data RunConfig =
  RunConfig
  { nAll :: Int
  , n :: Int
  , rates :: [Double]
  , versus :: (Double, Double)
  , qs :: [Double]
  , qsRate :: Double
  , foreRate :: (Double, Double)
  , histGrain :: Int
  , histRange :: Range Double
  } deriving (Eq, Show, Generic)

defaultRunConfig :: RunConfig
defaultRunConfig = RunConfig 1000 500 [0.95, 0.99] (0.95,0.95) ((0.1 *) <$> [1 .. 9]) 0.99 (0.99,0.99) 20 (Range (-0.03) 0.03)

data RunStyleConfig = RunStyleConfig {runName :: Text, chartSize :: Point Double} deriving (Eq, Show, Generic)

defaultRunStyleConfig :: RunStyleConfig
defaultRunStyleConfig = RunStyleConfig "default" (Point 300 200)
