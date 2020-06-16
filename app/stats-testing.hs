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

import Chart hiding (one, zero, repChoice)
import Control.Category ((>>>))
import qualified Control.Foldl as L
import qualified Control.Scanl as SL
import Control.Lens hiding ((:>), Unwrapped, Wrapped, Empty)
import Data.Generics.Labels ()
import Data.Random
import NumHask.Array.Dynamic hiding (append)
import NumHask.Prelude hiding ((<<*>>), replace, state, StateT, State, get, runStateT, runState, L1, fold)
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
import Web.Page hiding (StateT(..), State, state, get, bool, runState)
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
import Stats
import Options.Generic
import Data.Attoparsec.Text hiding (scan)
import qualified Data.Map.Strict as Map

data Opts =
  MakeCharts |
  ServeCharts
  deriving (Generic, Show)

instance ParseRecord Opts

data ServeType =
  ServeStats RandomConfig StatsConfig [Text] SvgOptions|
  ServeRandoms RandomConfig [Text] SvgOptions
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
           ("random testing", repServeType ["walk0", "walks", "correlated walks"] (ServeRandoms defaultRandomConfig ["walk0"] defaultSvgOptions))
         ])
        (fmap (fmap snd) <$> makeCharts)

makeCharts :: ServeType -> IO [(Text, Text)]
makeCharts (ServeStats rcfg ncfg items svgo) = do
  rs <- makeRandomSets rcfg
  pure $ selectItems items (testStatsCharts svgo rs ncfg)
makeCharts (ServeRandoms cfg items svgo) = do
  rs <- makeRandomSets cfg
  pure $ selectItems items (randomCharts svgo rs)

page :: Bool -> Page
page doDebug =
  mathjaxSvgPage "hasmathjax"
    <> bootstrapPage
    <> bridgePage
    & #htmlHeader .~ title_ "stats.hs testing"
    & #htmlBody
      .~ (
           div_ [class__ "container"]
            ((div_ [class__ "row no-gutters"]
             ( div_ [class__ "col-4", id_ "input"] mempty <>
               div_ [class__ "col-8", id_ "output"] mempty
             ) <>
             bool mempty (div_ [class__ "row", id_ "debug"] mempty) doDebug))
         )

serveRep :: (Show a) => SharedRep IO a -> (a -> IO [Text]) -> IO ()
serveRep mRep makeRep = do
  scotty 3000 $ do
    middleware $ midShared mRep initRep updateRep
    servePageWith "/" (defaultPageConfig "prod") (page True)
      where
        initRep e r =
          void $ oneRep r
            (\(Rep h fa) m -> do
              append e "input" (toText h)
              case snd (fa m) of
                Left err -> append e "debug" err
                Right cfg -> do
                  c <- makeRep cfg
                  replace e "output" (mconcat c))
        updateRep e (Left err) = do
          putStrLn (show err :: Text)
          append e "debug" err
        updateRep e (Right (_, Left err)) = do
          putStrLn (show err :: Text)
          append e "debug" err
        updateRep e (Right (_, Right c)) = do
            putStrLn (show c :: Text)
            t <- makeRep c
            replace e "output" (mconcat t)

repChoice :: (Monad m) => Int -> [(Text, SharedRep m a)] -> SharedRep m a
repChoice initt xs =
  bimap hmap mmap dd
    <<*>> foldr (\x a -> bimap (:) (:) x <<*>> a) (pure []) cs
  where
    ts = fst <$> xs
    cs = snd <$> xs
    dd = dropdownSum takeText id Nothing ts t0
    t0 = ts !! initt
    hmap dd' cs' =
      div_
        ( dd'
            <> mconcat (zipWith (\c t -> subtype c t0 t) cs' ts)
        )
    mmap dd' cs' = maybe (Data.List.head cs') (cs' !!) (elemIndex dd' ts)

subtype :: With a => a -> Text -> Text -> a
subtype h origt t =
  with
    h
    [ class__ "subtype ",
      data_ "sumtype" t,
      style_ ("display:" <> bool "block" "none" (origt /= t))
    ]
