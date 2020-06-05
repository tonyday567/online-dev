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
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

import Chart
import qualified Control.Foldl as L
import Control.Lens hiding ((:>), Unwrapped, Wrapped)
import Data.Generics.Labels ()
import Data.Random
import NumHask.Array.Dynamic hiding (append)
import NumHask.Prelude hiding ((<<*>>), replace)
import Stats
import Readme.Lhs
import System.Random.MWC
import Web.Page
import Control.Lens hiding (Wrapped, Unwrapped)
import Data.Attoparsec.Text (parseOnly, decimal)
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

-- correlation of a time series with it's moving average.
-- x = b * (ma r xs) + a

--
-- sqrt(2) ~ (\r -> (r ** 0.5) * (1/(L.fold (std 1) $ drop 1 $ (L.scan (ma (1-r)) rs)))) r for wide r.

stdOfMa :: Double -> [Double] -> Double -> Double
stdOfMa stdr rs r = 1 / L.fold (std stdr) (drop 1 $ L.scan (ma (1 - r)) rs)

stdOfMaChart :: FilePath -> Double -> [Double] -> [Double] -> IO ()
stdOfMaChart fp stdr rates rs =
  writeHudOptionsChart
    fp
    defaultSvgOptions
    (titlesHud "std of ma" "rate" "sqrt (1/r) * std of ma")
    []
    ( (: []) $
        Chart
          (LineA defaultLineStyle)
          (zipWith SP rates ((\r -> sqrt r * stdOfMa stdr rs r) <$> rates))
    )

stdOfMaChart' :: Double -> [Double] -> [Double] -> Text
stdOfMaChart' stdr rates rs =
  renderHudOptionsChart
    defaultSvgOptions
    (titlesHud "std of ma" "rate" "sqrt (1/r) * std of ma")
    []
    ( (: []) $
        Chart
          (LineA defaultLineStyle)
          (zipWith SP rates ((\r -> sqrt r * stdOfMa stdr rs r) <$> rates))
    )


-- | the average beta measure given an autocorrelation of the form r = a + b * ma.
-- The random variance in the beta measure (ie assuming beta = 0) is removed.
avBetaMeasure :: (Subtractive b, Fractional b, Multiplicative b) => [b] -> b -> b -> b
avBetaMeasure rs r b =
  ( \x ->
      x
        - L.fold
          (beta (ma 1))
          (zip ((\r -> drop 1 $ zipWith (\o m -> o + 0 * m) rs (L.scan (ma (1 - r)) rs)) 0.001) rs)
  )
    $ L.fold (beta (ma 1))
    $ zip ((\r -> drop 1 $ zipWith (\o m -> o + b * m) rs (L.scan (ma (1 - r)) rs)) r) rs

-- beta measure ~ (1/r) * original beta, if the beta was set at 0.1
-- this breaks down at less than 0.003, so that long ma's will be unstable.
avBetaMeasureChart :: FilePath -> [Double] -> [Double] -> Double -> IO ()
avBetaMeasureChart fp rates rs b =
  writeHudOptionsChart
    fp
    defaultSvgOptions
    (titlesHud "beta measure check" "rate" "average beta * (1/r)")
    []
    ( (: []) $
        Chart
          (LineA defaultLineStyle)
          (zipWith SP rates ((\r -> avBetaMeasure rs r b / r) <$> rates))
    )

avBetaMeasureChart' :: [Double] -> [Double] -> Double -> Text
avBetaMeasureChart' rates rs b =
  renderHudOptionsChart
    defaultSvgOptions
    (titlesHud "beta measure check" "rate" "average beta * (1/r)")
    []
    ( (: []) $
        Chart
          (LineA defaultLineStyle)
          (zipWith SP rates ((\r -> avBetaMeasure rs r b / r) <$> rates))
    )

betaErrors :: (Divisive c, Subtractive c, Fractional c) => [c] -> [c] -> [c] -> Array c
betaErrors rs rates bs =
  expand
    (\r b -> (1 / (r * b)) * avBetaMeasure rs r b)
    (fromFlatList [length rates] rates)
    (fromFlatList [length bs] bs)

betaErrorScatter :: FilePath -> [Double] -> [Double] -> [Double] -> IO ()
betaErrorScatter fp rs rates bs =
  writeHudOptionsChart
    fp
    defaultSvgOptions
    (titlesHud "beta measure error" "rates" "betas" & hudOptions)
    h0
    c0
  where
    f1 :: (Point Double -> Double) = \(Point x y) ->
      NumHask.Array.Dynamic.index
        (betaErrors rs rates bs)
        [fromIntegral (floor x :: Integer), fromIntegral (floor y :: Integer)]
    (c0, h0) = pixelfl f1 pixelOptions (defaultPixelLegendOptions "")
    pixelOptions =
      defaultPixelOptions
        & #poGrain .~ Point (fromIntegral $ length rates) (fromIntegral $ length bs)
        & #poRange .~ Rect 0 (fromIntegral $ length rates) 0 (fromIntegral $ length bs)
    hudOptions =
      #hudAxes
        .~ [ defaultAxisOptions
               & #atick . #tstyle .~ TickLabels (show <$> rates),
             defaultAxisOptions
               & #place .~ PlaceLeft
               & #atick . #tstyle .~ TickLabels (show <$> bs)
           ]

betaErrorScatter' :: [Double] -> [Double] -> [Double] -> Text
betaErrorScatter' rs rates bs =
  renderHudOptionsChart
    defaultSvgOptions
    (titlesHud "beta measure error" "rates" "betas" & hudOptions)
    h0
    c0
  where
    f1 :: (Point Double -> Double) = \(Point x y) ->
      NumHask.Array.Dynamic.index
        (betaErrors rs rates bs)
        [fromIntegral (floor x :: Integer), fromIntegral (floor y :: Integer)]
    (c0, h0) = pixelfl f1 pixelOptions (defaultPixelLegendOptions "")
    pixelOptions =
      defaultPixelOptions
        & #poGrain .~ Point (fromIntegral $ length rates) (fromIntegral $ length bs)
        & #poRange .~ Rect 0 (fromIntegral $ length rates) 0 (fromIntegral $ length bs)
    hudOptions =
      #hudAxes
        .~ [ defaultAxisOptions
               & #atick . #tstyle .~ TickLabels (show <$> rates),
             defaultAxisOptions
               & #place .~ PlaceLeft
               & #atick . #tstyle .~ TickLabels (show <$> bs)
           ]

doctype :: Text
doctype = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"

defaultRenderList :: [Double] -> Text
defaultRenderList = renderHudOptionsChart defaultSvgOptions mempty [] . (:[]) . Chart (LineA defaultLineStyle) . zipWith SP [0..]

canned :: IO ()
canned = do
  let n = 10000
  let rates = [0.001, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2]
  let bs = [0.001, 0.01, 0.02, 0.05, 0.1]
  g <- create
  rs <- rvs g n
  stdOfMaChart "other/stdofma.svg" 1 rates rs
  avBetaMeasureChart "other/avbetameasure.svg" rates rs 0.1
  betaErrorScatter "other/betascatter.svg" rs rates bs
  _ <-
    runOutput
      ("app/random.lhs", GitHubMarkdown)
      ("random.html", Html)
      $ pure ()
  pure ()

-- | common pattern of chart title, x-axis title and y-axis title
titlesHud :: Text -> Text -> Text -> HudOptions
titlesHud t x y =
  defaultHudOptions
    & #hudTitles
    .~ [ defaultTitle t,
         defaultTitle x & #place .~ PlaceBottom & #style . #size .~ 0.08,
         defaultTitle y & #place .~ PlaceLeft & #style . #size .~ 0.08
       ]

data RandomConfig = RandomConfig
  { rN :: Int,
    rRates :: [Double],
    rBeta :: Double,
    rBetas :: [Double],
    rStdMaDecay :: Double
  } deriving (Generic, Eq, Show)

defaultRandomConfig :: RandomConfig
defaultRandomConfig = RandomConfig 10000 [0.001, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2] 0.1 [0.001, 0.01, 0.02, 0.05, 0.1] 1

repRandomConfig :: (Monad m) => RandomConfig -> SharedRep m RandomConfig
repRandomConfig cfg = bimap hmap RandomConfig n' <<*>> rates' <<*>> beta' <<*>> betas' <<*>> stdmadecay'
  where
    n' = either (const (view #rN cfg)) id <$>
      readTextbox (Just "n") (view #rN cfg)
    rates' = either (const []) id <$>
      readTextbox (Just "rates") (view #rRates cfg)
    beta' = either (const (view #rBeta cfg)) id <$>
      readTextbox (Just "test beta") (view #rBeta cfg)
    betas' = either (const []) id <$>
      readTextbox (Just "test betas") (view #rBetas cfg)
    stdmadecay' = either (const 1) id <$>
      readTextbox (Just "std of ma decay rate") (view #rStdMaDecay cfg)
    hmap n'' rates'' beta'' betas'' stdmadecay'' = n'' <> rates'' <> beta'' <> betas'' <> stdmadecay''

testPage :: Text -> Text -> [(Text, Html ())] -> Page
testPage title mid sections =
  bootstrapPage <>
  bridgePage &
  #htmlHeader .~ title_ "simPage" &
  #htmlBody .~ divClass_ "container" (mconcat
    [ divClass_ "row" (h1_ (toHtml title))
    , divClass_ "row" (h2_ ("middleware: " <> toHtml mid))
    , divClass_ "row" $ mconcat $ (\(t,h) -> divClass_ "col" (h2_ (toHtml t) <> with div_ [id_ t] h)) <$> sections
    ])

mid1 :: SharedRep IO RandomConfig -> Application -> Application
mid1 sr = midShared sr initRep updateCharts

initRep
  :: Engine
  -> Rep RandomConfig
  -> StateT (HashMap Text Text) IO ()
initRep e r =
  void $ oneRep r
  (\(Rep h fa) m -> do
      append e "input" (toText h)
      case snd (fa m) of
        Left _ -> pure ()
        Right cfg -> do
          c <- chart1 cfg
          replace e "output" c)

chart1 :: RandomConfig -> IO Text
chart1 cfg = do
  g <- create
  rs <- rvs g (view #rN cfg)
  pure $ stdOfMaChart' (view #rStdMaDecay cfg) (view #rRates cfg) rs

charts' :: RandomConfig -> IO [Text]
charts' cfg = do
  g <- create
  rs <- rvs g (view #rN cfg)
  pure $
    [ stdOfMaChart' (view #rStdMaDecay cfg) (view #rRates cfg) rs,
      avBetaMeasureChart' (view #rRates cfg) rs (view #rBeta cfg),
      betaErrorScatter' rs (view #rRates cfg) (view #rBetas cfg)
    ]

results :: (a -> Text) -> Engine -> a -> IO ()
results r e x = replace e "output" (r x)

logResults :: (a -> Text) -> Engine -> Either Text a -> IO ()
logResults _ e (Left err) = append e "log" (err <> "<br>")
logResults r e (Right x) = results r e x

chartDisplay :: (a -> Text) -> Engine -> Either Text a -> IO ()
chartDisplay _ e (Left err) = append e "log" (err <> "<br>")
chartDisplay r e (Right x) = results r e x

initChartRender ::
  Engine ->
  Rep RandomConfig ->
  StateT (HashMap Text Text) IO ()
initChartRender e r =
  void $
    oneRep
      r
      ( \(Rep h fa) m -> do
          append e "input" (toText h)
          c <- either (pure mempty) chart1 (snd (fa m))
          replace e "output" c)

updateChart :: Engine -> Either Text (HashMap Text Text, Either Text RandomConfig) -> IO ()
updateChart e (Left _) = pure ()
updateChart e (Right (_, Left _)) = pure ()
updateChart e (Right (_, Right c)) = do
  t <- chart1 c
  replace e "output" t

updateCharts :: Engine -> Either Text (HashMap Text Text, Either Text RandomConfig) -> IO ()
updateCharts e (Left _) = pure ()
updateCharts e (Right (_, Left _)) = pure ()
updateCharts e (Right (_, Right c)) = do
  t <- charts' c
  replace e "output" (mconcat t)

main :: IO ()
main = do
  let cfg = defaultRandomConfig
  scotty 3000 $ do
    middleware $ mid1 (repRandomConfig cfg)
    servePageWith "/" (defaultPageConfig "prod")
      (testPage "prod" "simulation"
       [ ("input", mempty)
       , ("output",
            toHtml (show cfg :: Text))
       ])

