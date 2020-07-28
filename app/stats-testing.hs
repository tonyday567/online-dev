{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-
Experimenting with the Stats module
-}

import qualified Box
import Chart
import qualified Control.Foldl as L
import Control.Lens hiding ((:>), Empty, Unwrapped, Wrapped)
import Control.Lens hiding (Empty, Unwrapped, Wrapped)
import Control.Category
import qualified Control.Monad.Trans.State.Lazy as StateL
import Control.Monad.Trans.State.Strict
import qualified Control.Scanl as SL
import Data.Attoparsec.Text (decimal, parseOnly)
import qualified Data.Attoparsec.Text as A
import Data.Attoparsec.Text hiding (scan)
import Data.Fold hiding (M)
import Data.Foldable (foldl1)
import Data.Generics.Labels ()
import Data.List ((!!), elem, elemIndex, head, scanl1)
import qualified Data.HashMap.Strict as HashMap
import Data.Mealy
import Data.Profunctor.Strong
import Data.Sequence (Seq (..), ViewR (..))
import Data.Simulate
import qualified Data.Text as Text
import Lucid
import Network.Wai (rawPathInfo)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static ((>->), addBase, noDots, staticPolicy)
import NumHask.Array.Dynamic hiding (append)
import NumHask.Prelude hiding (L1, State, StateT, fold, get, replace, runState, runStateT, state)
import Options.Generic
import Options.Generic
import Run.History
import Run.Random
import Run.Stats
import System.Random.MWC
import Web.Rep
import Web.Rep.Examples
import Web.Scotty (middleware, scotty)
import Data.Time

data Opts
  = MakeCharts
  | ServeCharts
  deriving (Generic, Show)

instance ParseRecord Opts

data ServeType
  = ServeStats RandomConfig StatsConfig [Text] SvgOptions
  | ServeRandoms RandomConfig [Text] SvgOptions
  | ServeModel1 RandomConfig Model1 Double [Text] SvgOptions
  | ServeHistory HistoryConfig Model1HistoryConfig SvgOptions
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
    c =
      either (const rate) id
        <$> readTextbox (Just "model1 beta") rate
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

serveRep' :: SharedRep IO ServeType -> IO ()
serveRep' srep = sharedServer srep defaultSocketConfig defaultSocketPage
  defaultInputCode outputCode
  where
    outputCode ea = do
      case ea of
        Left err -> pure [Append "debug" err]
        Right st -> do
          chartTexts <- makeCharts st
          pure
            [ Replace "output" (mconcat (snd <$> chartTexts))
            ]

makeCharts :: ServeType -> IO [(Text, Text)]
makeCharts (ServeStats rcfg ncfg items svgo) =
  makeRandomCharts rcfg svgo items (testStatsCharts ncfg)
makeCharts (ServeRandoms cfg items svgo) = do
  makeRandomCharts cfg svgo items randomCharts
makeCharts (ServeModel1 cfg m1 _ items svgo) =
  makeRandomCharts cfg svgo items
    (model1Charts m1)
makeCharts (ServeHistory cfg m1hc svgo) =
  makeReturnsCharts cfg svgo (cfg ^. #hCharts) (historyCharts cfg m1hc)

-- make charts that rely on a RandomSet
makeRandomCharts :: RandomConfig -> SvgOptions -> [Text] ->
  (RandomSets -> HashMap.HashMap Text (HudOptions, [Chart Double])) -> IO [(Text, Text)]
makeRandomCharts rcfg svgo items cs = do
  rs <- makeRandomSets rcfg
  pure $ selectItems items
    (HashMap.map (defaultRender svgo) (cs rs))

-- make charts that rely on history
makeReturnsCharts :: HistoryConfig -> SvgOptions -> [Text] ->
  ([(UTCTime, Double)] -> HashMap.HashMap Text (HudOptions, [Chart Double])) -> IO [(Text, Text)]
makeReturnsCharts cfg svgo items cs = do
  rs <- makeReturns cfg
  pure $ selectItems items
    (HashMap.map (defaultRender svgo) (cs rs))

main :: IO ()
main = do
  o :: Opts <- getRecord "Chart production"
  putStrLn (show o :: Text)
  case o of
    MakeCharts -> do
      cs <- makeCharts (ServeStats defaultRandomConfig defaultStatsConfig testStatsChartsNames
                       defaultSvgOptions)
      traverse_ (\(fp, c) -> writeFile ("other/" <> unpack fp <> ".svg") c) cs
    ServeCharts ->
      serveRep'
        ( repChoice
            1
            [ ("stats testing", repServeType testStatsChartsNames
                (ServeStats defaultRandomConfig defaultStatsConfig testStatsChartsNames
                 defaultSvgOptions)),
              ("random testing", repServeType
                randomChartsNames
                (ServeRandoms defaultRandomConfig ["walk0"] defaultSvgOptions)),
              ("model1", repServeType model1ChartsNames
                (ServeModel1 defaultRandomConfig zeroModel1 0.01
                 ["ex-model1-compare"] defaultSvgOptions)),
              ("history", repServeType historyChartNames
                (ServeHistory defaultHistoryConfig defaultModel1HistoryConfig
                 defaultSvgOptions))
            ]
        )
