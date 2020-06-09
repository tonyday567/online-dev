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
import Data.Random
import NumHask.Array.Dynamic hiding (append)
import NumHask.Prelude hiding ((<<*>>), replace, state, StateT, State, get, runStateT, runState, L1)
import Readme.Lhs
import System.Random.MWC
import Control.Lens hiding (Wrapped, Unwrapped, Empty)
import Data.Attoparsec.Text (parseOnly, decimal)
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
import Data.Fold
import Data.Foldable (foldl1)
import Data.List (scanl1, (!!))
import Stats
import Options.Generic

-- * chart helpers
ls :: [LineStyle]
ls = fmap (\c -> defaultLineStyle & #color .~ c & #width .~ 0.003) $ drop 1 palette

sChart :: SvgOptions -> (Double -> Mealy a Double) -> [Double] -> Int -> Text -> [a] -> Text
sChart svgOptions m rates d title xs =
  renderHudOptionsChart
    svgOptions
    (defaultHudOptions &
     #hudTitles .~ [ defaultTitle title] &
     #hudLegend .~ Just
      ( defaultLegendOptions
          & #ltext . #size .~ 0.2
          & #lplace .~ PlaceAbsolute (Point 0.3 (-0.3))
          & #legendFrame .~ Just (RectStyle 0.02 (palette !! 5) white),
        zipWith
          (\a r -> (LineA a, ("rate = " <>) . Text.pack . show $ r))
          ls
          rates
      ))
    []
    ( zipWith (\s xs' -> Chart (LineA s) xs') ls $
      (zipWith SP (fromIntegral <$> [d..]) <$> ((\r -> drop d $ scanS (m r) xs) <$> rates))
    )

stdOfMaChart :: SvgOptions -> Double -> [Double] -> [Double] -> Text
stdOfMaChart svgOptions stdr rates xs =
  renderHudOptionsChart
    svgOptions
    (titlesHud "std of ma" "rate" "std of ma / (0.5*r)**0.5")
    []
    ( (: []) $
        Chart
          (LineA defaultLineStyle)
          (zipWith SP rates ((\r -> stdOfMa stdr r xs / ((0.5 * r)**0.5)) <$> rates'))
    )
  where
    rates' = (\r -> bool r 0.00001 (r==0)) <$> rates
    stdOfMa stdr r xs = foldS (std stdr) (fromList $ scanS (ma (1 - r)) xs)
-- | common pattern of chart title, x-axis title and y-axis title
titlesHud :: Text -> Text -> Text -> HudOptions
titlesHud t x y =
  defaultHudOptions
    & #hudTitles
    .~ [ defaultTitle t,
         defaultTitle x & #place .~ PlaceBottom & #style . #size .~ 0.08,
         defaultTitle y & #place .~ PlaceLeft & #style . #size .~ 0.08
       ]

-- | check if beta is recovering the exact simulated dependency via maDependency
-- and doing what is expected.
-- (The cumulative drift in the specific sample is removed).
-- >>> maDependencyCheck 0.1 0.01 xs
-- 9.999999999999876e-2
--
-- beta measure looks like it contains large noise sensitivities
maDependencyCheck :: Double -> Double -> [Double] -> Double
maDependencyCheck b r xs = xsb - x0
  where
    -- constructued time series with a beta between the ma and the series.
    xs' = scanS (maDependency b (ma (1 - r))) xs
    ma' = scanS ((ma (1 - r)) >>> delay [0]) xs'
    -- beta of ma' on simulated series
    xsb = foldS (beta (ma (1 - r))) $ fromList $ drop 1 $ zip ma' xs'
    -- beta measurement if beta of ma was, in reality, zero.
    x0 = foldS (beta (ma (1 - r))) $ fromList $ drop 1 $ zip ma' xs

makeMaDependencyChart :: SvgOptions -> Double -> Double -> Double -> [Double] -> Text
makeMaDependencyChart svgOptions b r rb xs =
  renderHudOptionsChart svgOptions defaultHudOptions [] $
  [ Chart (LineA defaultLineStyle) $ drop 1000 $ zipWith SP [0..] (scanS (beta (ma (1 - rb))) $ fromList $ drop 100 $ zip ma' xs')
  ]
  where
    xs' = scanS (maDependency b (ma (1 - r))) xs
    ma' = scanS ((ma (1 - r)) >>> delay [0]) xs'

-- | various configuration details
data StatsTestingConfig = StatsTestingConfig
  { stN :: Int,
    stRates :: [Double],
    stBeta :: Double,
    stBetas :: [Double],
    stStdMaDecay :: Double,
    stMaDepBeta :: Double,
    stMaDepRate :: Double,
    stMaDepBetaRate :: Double,
    stHeight :: Double
  } deriving (Generic, Eq, Show)

defaultStatsTestingConfig :: StatsTestingConfig
defaultStatsTestingConfig =
  StatsTestingConfig
  10000
  [0, 0.01, 0.02]
  0.1
  [0.001, 0.01, 0.02, 0.05, 0.1]
  1
  0.1
  0.01
  0
  150

-- | representation of configuration
repStatsTestingConfig :: (Monad m) => StatsTestingConfig -> SharedRep m StatsTestingConfig
repStatsTestingConfig cfg = bimap hmap StatsTestingConfig n' <<*>> rates' <<*>> beta' <<*>> betas' <<*>> stdmadecay' <<*>> madepbeta' <<*>> madeprate' <<*>> madepbetarate' <<*>> height'
  where
    n' = either (const (view #stN cfg)) id <$>
      readTextbox (Just "n") (view #stN cfg)
    rates' = either (const []) id <$>
      readTextbox (Just "rates") (view #stRates cfg)
    beta' = either (const (view #stBeta cfg)) id <$>
      readTextbox (Just "test beta") (view #stBeta cfg)
    betas' = either (const []) id <$>
      readTextbox (Just "test betas") (view #stBetas cfg)
    stdmadecay' = either (const 1) id <$>
      readTextbox (Just "std of ma decay rate") (view #stStdMaDecay cfg)
    madepbeta' = either (const 0.1) id <$>
      readTextbox (Just "beta ma dependency") (view #stMaDepBeta cfg)
    madeprate' = either (const 0.01) id <$>
      readTextbox (Just "decay rate of ma dep") (view #stMaDepRate cfg)
    madepbetarate' = either (const 0.001) id <$>
      readTextbox (Just "rate of beta calc for madep") (view #stMaDepBetaRate cfg)
    height' = either (const 150) id <$>
      readTextbox (Just "chart height") (view #stHeight cfg)
    hmap n'' rates'' beta'' betas'' stdmadecay'' madepbeta'' madeprate'' madepbetarate'' height'' = n'' <> rates'' <> beta'' <> betas'' <> stdmadecay'' <> madepbeta'' <> madeprate'' <> madepbetarate'' <> height''

data Opts =
  ServeCharts |
  MakeCharts
  deriving (Generic, Show)

instance ParseRecord Opts

main :: IO ()
main = do
  o :: Opts <- getRecord "Stats.hs testing"
  putStrLn (show o :: Text)
  case o of
    MakeCharts -> do
      cs <- makeCharts defaultStatsTestingConfig
      traverse_ (\(fp,c) -> writeFile ("other/" <> unpack fp <> ".svg") c) cs
    ServeCharts -> serveCharts defaultStatsTestingConfig

makeCharts :: StatsTestingConfig -> IO [(Text, Text)]
makeCharts cfg = do
  g <- create
  xs <- rvs g (view #stN cfg)
  xs1 <- rvs g (view #stN cfg)
  pure $
    [ ("ex-madep", makeMaDependencyChart
        (defaultSvgOptions & #svgHeight .~ view #stHeight cfg)
        (view #stMaDepBeta cfg)
        (view #stMaDepRate cfg)
        (view #stMaDepBetaRate cfg)
        xs),
      ("ex-ma", sChart (defaultSvgOptions & #svgHeight .~ view #stHeight cfg) ma ((1 -) <$> view #stRates cfg) 0 "ma" (xs <> ((1+) . (2*) <$> xs1))),
      ("ex-std", sChart (defaultSvgOptions & #svgHeight .~ view #stHeight cfg) std ((1 -) <$> view #stRates cfg) 0 "std" (xs <> ((1+) . (2*) <$> xs1))),
      ("ex-stdma", stdOfMaChart (defaultSvgOptions & #svgHeight .~ view #stHeight cfg) (view #stStdMaDecay cfg) (view #stRates cfg) xs)
    ]

page :: Bool -> Text -> Page
page doDebug maxWidth =
  mathjaxSvgPage "hasmathjax"
    <> bootstrapPage
    <> bridgePage
    & #htmlHeader .~ title_ "stats.hs testing"
    & #htmlBody
      .~ divClass_
        "container"
        ( divClass_
            "row"
            ( sec "col4" "input"
                <> sec' "col8" "output"
            )
            <> bool mempty (divClass_ "row" (with div_ [id_ "debug"] mempty)) doDebug
        )
  where
    sec d n = divClass_ d (with div_ [id_ n] mempty)
    sec' d n = divClass_ d (with div_ [id_ n, style_ ("max-width: " <> maxWidth <> "px")] mempty)

initChartsRep
  :: Engine
  -> Rep StatsTestingConfig
  -> StateL.StateT (HashMap Text Text) IO ()
initChartsRep e r =
  void $ oneRep r
  (\(Rep h fa) m -> do
      append e "input" (toText h)
      case snd (fa m) of
        Left err -> append e "debug" err
        Right cfg -> do
          c <- makeCharts cfg
          replace e "output" (mconcat (snd <$> c)))

updateChartsRep :: Engine -> Either Text (HashMap Text Text, Either Text StatsTestingConfig) -> IO ()
updateChartsRep e (Left err) = append e "debug" err
updateChartsRep e (Right (_, Left err)) = append e "debug" err
updateChartsRep e (Right (_, Right c)) = do
  t <- makeCharts c
  replace e "output" (mconcat (snd <$> t))

serveCharts :: StatsTestingConfig -> IO ()
serveCharts cfg = do
  scotty 3000 $ do
    middleware $ midShared (repStatsTestingConfig cfg) initChartsRep updateChartsRep
    servePageWith "/" (defaultPageConfig "prod") (page True "450")
