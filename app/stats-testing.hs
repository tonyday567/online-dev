{-
Experimenting with the Stats module
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Chart hiding (one, zero)
import Control.Category ((>>>))
import qualified Control.Foldl as L
import qualified Control.Scanl as SL
import Control.Lens hiding ((:>), Unwrapped, Wrapped, Empty)
import Data.Generics.Labels ()
import Data.Simulate
import NumHask.Array.Dynamic hiding (append)
import NumHask.Prelude hiding (replace, state, StateT, State, get, runStateT, runState, L1, fold)
import Readme.Lhs
import System.Random.MWC
import Control.Lens hiding (Wrapped, Unwrapped, Empty)
import Data.Attoparsec.Text (parseOnly, decimal)
import qualified Data.Attoparsec.Text as A
import Lucid
import Network.Wai (rawPathInfo)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Options.Generic
import Web.Page
import Web.Page.Examples
import Web.Scotty (scotty, middleware)
import qualified Box
import qualified Data.Text as Text
import Control.Monad
import Data.Maybe
import Control.Monad.Trans.State.Strict
import qualified Control.Monad.Trans.State.Lazy as StateL
import Data.Sequence (ViewR(..), Seq(..))
import Data.Profunctor.Strong
import Data.Fold hiding (M)
import Data.Foldable (foldl1)
import Data.List (scanl1, (!!), head, elemIndex, elem)
import Run.Types
import Run.Stats
import Run.Random
import Run.History
import Data.Mealy
import Options.Generic
import Data.Attoparsec.Text hiding (scan)
import qualified Data.Map.Strict as Map

data Opts =
  MakeCharts |
  ServeCharts
  deriving (Generic, Show)

instance ParseRecord Opts

data ServeType =
  ServeStats RandomConfig StatsConfig [Text] SvgOptions |
  ServeRandoms RandomConfig [Text] SvgOptions |
  ServeModel1 RandomConfig Model1 Double [Text] SvgOptions |
  ServeHistory HistoryConfig Model1HistoryConfig SvgOptions
  deriving (Eq, Show, Generic)

repServeType :: (Monad m) => [Text] -> ServeType -> SharedRep m ServeType
repServeType allItems (ServeStats rc nc items svgo) = bimap hmap ServeStats a <<*>> b <<*>> c <<*>> d
  where
    a = repRandomConfig rc
    b = repStatsConfig nc
    c = repItemsSelect items allItems
    d = repSvgOptions svgo
    hmap a b c d =
      accordion_
        "acctick"
        Nothing
        [ ("randoms", a),
          ("testing", b),
          ("items", c),
          ("svg", d)
        ]
repServeType allItems (ServeRandoms rc items svgo) = bimap hmap ServeRandoms a <<*>> b <<*>> c
  where
    a = repRandomConfig rc
    b = repItemsSelect items allItems
    c = repSvgOptions svgo
    hmap a b c =
      accordion_
        "acctick"
        Nothing
        [ ("randoms", a),
          ("items", b),
          ("svg", c)
        ]
repServeType allItems (ServeModel1 rc m1 rate items svgo) =
  bimap hmap ServeModel1 a <<*>> b <<*>> c <<*>> d <<*>> e
  where
    a = repRandomConfig rc
    b = repModel1 m1
    c = either (const rate) id <$>
      readTextbox (Just "model1 beta") rate
    d = repItemsSelect items allItems
    e = repSvgOptions svgo
    hmap a b c d e =
      accordion_
        "acctick"
        Nothing
        [ ("randoms", a),
          ("model1", b <> c),
          ("items", d),
          ("svg", e)
        ]
repServeType allItems (ServeHistory hc m1hc svgo) =
  bimap hmap ServeHistory a <<*>> b <<*>> c
  where
    a = repHistoryConfig allItems hc
    b = repModel1HistoryConfig m1hc
    c = repSvgOptions svgo
    hmap a b c =
      accordion_
        "acctick"
        Nothing
        [ ("history", a),
          ("model1", b),
          ("svg", c)
        ]

main :: IO ()
main = do
  o :: Opts <- getRecord "Chart production"
  putStrLn (show o :: Text)
  case o of
    MakeCharts -> do
      cs <- makeCharts (ServeStats defaultRandomConfig defaultStatsConfig allTestStatsCharts defaultSvgOptions)
      traverse_ (\(fp,c) -> writeFile ("other/" <> unpack fp <> ".svg") c) cs
    ServeCharts ->
      serveRep
        (repChoice 1
         [ ("stats testing", repServeType allTestStatsCharts (ServeStats defaultRandomConfig defaultStatsConfig allTestStatsCharts defaultSvgOptions)),
           ("random testing", repServeType ["walk0", "walks", "correlated walks"] (ServeRandoms defaultRandomConfig ["walk0"] defaultSvgOptions)),
           ("model1", repServeType model1ChartNames (ServeModel1 defaultRandomConfig zeroModel1 0.01 ["ex-model1-compare"] defaultSvgOptions)),
           ("history", repServeType historyChartNames (ServeHistory defaultHistoryConfig defaultModel1HistoryConfig defaultSvgOptions))
         ])
        True
        (fmap (fmap snd) <$> makeCharts)

makeCharts :: ServeType -> IO [(Text, Text)]
makeCharts (ServeStats rcfg ncfg items svgo) = do
  rs <- makeRandomSets rcfg
  pure $ selectItems items (testStatsCharts svgo rs ncfg)
makeCharts (ServeRandoms cfg items svgo) = do
  rs <- makeRandomSets cfg
  pure $ selectItems items (randomCharts svgo rs)
makeCharts (ServeModel1 cfg m1 r items svgo) = do
  rs <- makeRandomSets cfg
  pure $ selectItems items (model1Charts svgo rs m1 r)
makeCharts (ServeHistory cfg m1hc svgo) = do
  xs <- makeReturns cfg
  pure $ selectItems (cfg ^. #hCharts) (historyCharts svgo cfg m1hc xs)

