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
import NumHask.Prelude hiding ((<<*>>), replace, state, StateT, State, get, runStateT, runState, L1, fold)
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
import Data.Fold hiding (M)
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
      (zipWith SP (fromIntegral <$> [d..]) <$> ((\r -> drop d $ scan (m r) xs) <$> rates))
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
    stdOfMa stdr r xs = fold (std stdr) (fromList $ scan (ma (1 - r)) xs)
-- | common pattern of chart title, x-axis title and y-axis title
titlesHud :: Text -> Text -> Text -> HudOptions
titlesHud t x y =
  defaultHudOptions
    & #hudTitles
    .~ [ defaultTitle t,
         defaultTitle x & #place .~ PlaceBottom & #style . #size .~ 0.08,
         defaultTitle y & #place .~ PlaceLeft & #style . #size .~ 0.08
       ]

makeMaDependencyChart :: Foldable f => SvgOptions -> Double -> Double -> Double -> f Double -> Text
makeMaDependencyChart svgOptions b r rb xs =
  renderHudOptionsChart svgOptions defaultHudOptions [] $
  [ Chart (LineA defaultLineStyle) $ drop 1000 $ zipWith SP [0..] (scan (beta1 (ma (1 - rb))) $ fromList $ drop 100 $ zip ma' (toList xs'))
  ]
  where
    xs' = scan (depState (\a m -> a + b * m) (ma (1 - r))) (toList xs)
    ma' = scan ((ma (1 - r)) >>> delay [0]) xs'

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
  MakeCharts |
  ServeModel1
  deriving (Generic, Show)

instance ParseRecord Opts

main :: IO ()
main = do
  o :: Opts <- getRecord "Stats.hs testing"
  putStrLn (show o :: Text)
  case o of
    MakeCharts -> do
      cs <- makeTestCharts defaultStatsTestingConfig
      traverse_ (\(fp,c) -> writeFile ("other/" <> unpack fp <> ".svg") c) cs
    ServeCharts -> serveRep defaultStatsTestingConfig repStatsTestingConfig makeTestCharts
    ServeModel1 -> serveRep zeroModel1 repModel1 (makeChartsModel1 defaultStatsTestingConfig)

makeTestCharts :: StatsTestingConfig -> IO [(Text, Text)]
makeTestCharts cfg = do
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

makeChartsModel1 :: StatsTestingConfig -> Model1 -> IO [(Text, Text)]
makeChartsModel1 cfg m1 = do
  g <- create
  xs <- rvs g (view #stN cfg)
  pure $
    [ ("ex-stats",
        show (fold (depModel1 0.99 m1 >>> ((,) <$> ma 0.99 <*> std 0.99)) xs)),
      ("ex-model1",
       sChart (defaultSvgOptions & #svgHeight .~ view #stHeight cfg)
        (\r -> depModel1 r m1 >>> M id (+) id) [0.01] 0 "model1 walk" xs),
      ("ex-orig",
       sChart (defaultSvgOptions & #svgHeight .~ view #stHeight cfg)
        (\_ -> M id (+) id) [0.01] 0 "orig random walk" xs)
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

serveRep :: a -> (a -> SharedRep IO a) -> (a -> IO [(Text, Text)]) -> IO ()
serveRep m mRep makeRep = do
  scotty 3000 $ do
    middleware $ midShared (mRep m) initRep updateRep
    servePageWith "/" (defaultPageConfig "prod") (page True "450")
      where
        initRep e r =
          void $ oneRep r
            (\(Rep h fa) m -> do
              append e "input" (toText h)
              case snd (fa m) of
                Left err -> append e "debug" err
                Right cfg -> do
                  c <- makeRep cfg
                  replace e "output" (mconcat (snd <$> c)))
        updateRep e (Left err) = append e "debug" err
        updateRep e (Right (_, Left err)) = append e "debug" err
        updateRep e (Right (_, Right c)) = do
            t <- makeRep c
            replace e "output" (mconcat (snd <$> t))

-- | representation of model1
repModel1 :: (Monad m) => Model1 -> SharedRep m Model1
repModel1 m1 = bimap hmap Model1 alphaX' <<*>> alphaS' <<*>> betaMa2X' <<*>> betaMa2S' <<*>> betaStd2X' <<*>> betaStd2S'
  where
    alphaX' = either (const (view #alphaX m1)) id <$>
      readTextbox (Just "alphaX") (view #alphaX m1)
    alphaS' = either (const (view #alphaS m1)) id <$>
      readTextbox (Just "alphaS") (view #alphaS m1)
    betaMa2X' = either (const (view #betaMa2X m1)) id <$>
      readTextbox (Just "betaMa2X") (view #betaMa2X m1)
    betaMa2S' = either (const (view #betaMa2S m1)) id <$>
      readTextbox (Just "betaMa2S") (view #betaMa2S m1)
    betaStd2X' = either (const (view #betaStd2X m1)) id <$>
      readTextbox (Just "betaStd2X") (view #betaStd2X m1)
    betaStd2S' = either (const (view #betaStd2S m1)) id <$>
      readTextbox (Just "betaStd2S") (view #betaStd2S m1)
    hmap alphaX'' alphaS'' betaMa2X'' betaMa2S'' betaStd2X'' betaStd2S'' = alphaX'' <> alphaS'' <> betaMa2X'' <> betaMa2S'' <> betaStd2X'' <> betaStd2S''
