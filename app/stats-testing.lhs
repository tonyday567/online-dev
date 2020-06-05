``` {.output .header}
```
Testing Stats module

> {-# LANGUAGE ConstraintKinds #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE DuplicateRecordFields #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MonoLocalBinds #-}
> {-# LANGUAGE OverloadedLabels #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE GADTs #-}
> {-# OPTIONS_GHC -Wall #-}
> {-# OPTIONS_GHC -Wno-name-shadowing #-}
> {-# OPTIONS_GHC -Wno-type-defaults #-}
> {-# OPTIONS_GHC -Wno-unused-imports #-}
> {-# OPTIONS_GHC -Wno-orphans #-}
> 
> import Chart hiding (one, zero)
> import Control.Category ((>>>))
> import qualified Control.Foldl as L
> import qualified Control.Scanl as SL
> import Control.Lens hiding ((:>), Unwrapped, Wrapped, Empty)
> import Data.Generics.Labels ()
> import Data.Random
> import NumHask.Array.Dynamic hiding (append)
> import NumHask.Prelude hiding ((<<*>>), replace, state, StateT, State, get, runStateT, runState, L1)
> import Readme.Lhs
> import System.Random.MWC
> import Control.Lens hiding (Wrapped, Unwrapped, Empty)
> import Data.Attoparsec.Text (parseOnly, decimal)
> import Lucid
> import Network.Wai (rawPathInfo)
> import Network.Wai.Middleware.RequestLogger (logStdoutDev)
> import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
> import Options.Generic
> import Web.Page hiding (StateT(..), State, state, get, bool, runState)
> import Web.Page.Examples
> import Web.Scotty (scotty, middleware)
> import qualified Box
> import qualified Data.Text as Text
> import Control.Monad
> import Data.Maybe
> import Control.Monad.Trans.State.Strict
> import qualified Control.Monad.Trans.State.Lazy as StateL
> import Data.Sequence (ViewR(..), Seq(..))
> import Data.Profunctor.Strong
> import Data.Fold
> import Data.Foldable (foldl1)
> import Data.List (scanl1)
> import Stats
> 
> -- * experiments
> ls :: [LineStyle]
> ls = fmap (\c -> defaultLineStyle & #color .~ c & #width .~ 0.003) palette
> 
> sChart :: (Double -> L1 a Double) -> [Double] -> Int -> Text -> [a] -> Text
> sChart m rates d title xs =
>   renderHudOptionsChart
>     defaultSvgOptions
>     (titlesHud title "n" "measure" & #hudLegend
>     .~ Just
>       ( defaultLegendOptions
>           & #ltext . #size .~ 0.3
>           & #lplace .~ PlaceBottom,
>         zipWith
>           (\a r -> (LineA a, ("rate = " <>) . Text.pack . show $ r))
>           (drop 1 ls)
>           rates
>       ))
>     []
>     ( zipWith (\s xs' -> Chart (LineA s) xs') ls $
>       (zipWith SP (fromIntegral <$> [d..]) <$> ((\r -> drop d $ scanL1 (m r) xs) <$> rates))
>     )
> 

The standard deviation of the moving average seems to be proprtional to r**0.5, and equal to around (0.5*r)**0.5.

This seems to break down for low r (below 0.01 say), probably due to startup issues.

``` {.output .stdofma}
```

> 
> -- >>> xs <- rs 10000
> -- >>> stdOfMa 1 0.01 xs
> -- 7.318309056744661e-2
> --
> stdOfMa :: Double -> Double -> [Double] -> Double
> stdOfMa stdr r xs = foldL1 (std stdr) (fromList $ scanL1 (ma (1 - r)) xs)
> 
> stdOfMaChart :: Double -> [Double] -> [Double] -> Text
> stdOfMaChart stdr rates xs =
>   renderHudOptionsChart
>     defaultSvgOptions
>     (titlesHud "std of ma" "rate" "std of ma / (0.5*r)**0.5")
>     []
>     ( (: []) $
>         Chart
>           (LineA defaultLineStyle)
>           (zipWith SP rates ((\r -> stdOfMa stdr r xs / ((0.5 * r)**0.5)) <$> rates))
>     )
> 
> 
> -- | common pattern of chart title, x-axis title and y-axis title
> titlesHud :: Text -> Text -> Text -> HudOptions
> titlesHud t x y =
>   defaultHudOptions
>     & #hudTitles
>     .~ [ defaultTitle t,
>          defaultTitle x & #place .~ PlaceBottom & #style . #size .~ 0.08,
>          defaultTitle y & #place .~ PlaceLeft & #style . #size .~ 0.08
>        ]
> 
> -- | check if beta is recovering a simulated dependency on historical ma,
> -- and doing what is expected.
> -- (The cumulative drift in the specific sample is removed).
> -- >>> s1Check 0.1 0.01 xs
> -- 9.999999999999876e-2
> --
> -- beta measure looks like it contains large noise sensitivities
> s1Check :: Double -> Double -> [Double] -> Double
> s1Check b r xs = xsb - x0
>   where
>     -- constructued time series with a beta between the ma and the series.
>     xs' = scanL1 (onlineDep b (ma (1 - r))) xs
>     ma' = scanL1 (ma (1 - r)) xs'
>     -- beta of ma' on simulated series
>     xsb = foldL1 (beta (ma (1 - r))) $ fromList $ drop 1 $ zip ma' xs'
>     -- beta measurement if beta of ma was, in reality, zero.
>     x0 = foldL1 (beta (ma (1 - r))) $ fromList $ drop 1 $ zip ma' xs
> 
> data SimCrossConfig = SimCrossConfig
>   { rN :: Int,
>     rRates :: [Double],
>     rBeta :: Double,
>     rBetas :: [Double],
>     rStdMaDecay :: Double
>   } deriving (Generic, Eq, Show)
> 
> defaultSimCrossConfig :: SimCrossConfig
> defaultSimCrossConfig =
>   SimCrossConfig
>   10000
>   [0.001, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2]
>   0.1
>   [0.001, 0.01, 0.02, 0.05, 0.1]
>   1
> 
> main :: IO ()
> main = do
>   let n = view #rN defaultSimCrossConfig
>   let rates = view #rRates defaultSimCrossConfig
>   -- let bs = view #rBetas defaultSimCrossConfig
>   g <- create
>   xs <- rvs g n
>   _ <-
>     runOutput
>       ("app/stats-testing.lhs", LHS)
>       ("other/stats-testing.html", Html)
>       $ do
>         output "stdofma" (RawHtml $ stdOfMaChart 1 rates xs)
>         -- output "avbeta" (RawHtml $ avBetaChart 1 rates bs)
>         -- output "betaerrors" (RawHtml $ betaErrorsChart bs rates xs)
>         output "header"
>           (RawHtml $ mconcat $
>            [ "<link rel=\"stylesheet\" type=\"text/css\" href=\"./lhs.css\"  />",
>              "<script src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-svg.js\"></script>"
>            ])
>   pure ()
> 
> 

-- | a constant fold
mconst :: a -> L1 a a
mconst a = L1 (\() _ -> ()) () (const a)


> 
> 
> 


data T1 = T1
  { t1Ma :: Double,
    t1Std :: Double,
    t1Orig :: Double
  } deriving (Eq, Show, Generic)

t1 :: Double -> SL.Scan Double T1
t1 r = SL.prescan (T1 <$> (ma r) <*> (std r)) <*> id

f1 :: Double -> T1 -> Double
f1 b (T1 m _ o) = o + b * m

s1 :: Double -> Double -> SL.Scan Double Double
s1 b r = SL.Scan step begin
  where
    step a = do
      m <- get
      maStep (a + b * m)
    begin = Averager 0 0

> 
> s0 :: Double -> SL.Scan Double Double
> s0 r = SL.Scan step begin
>   where
>     step a = state (step' a)
>     begin = Averager (0.0, 0.0)
>     g = (r*)
>     step' a (Averager (s, c)) = let (s',c') = (g $ s + a, g $ c + 1) in (s'/c', Averager (s',c'))
> 
> s1 :: Double -> Double -> SL.Scan Double Double
> s1 b r = SL.Scan step begin
>   where
>     step a = state (step' a)
>     begin = Averager (0.0, 0.0)
>     g = (r*)
>     step' a (Averager (s, c)) =
>       let a' = a + b * (s `div0` c) in
>       let (s',c') = (g $ s + a', g $ c + 1) in (a', Averager (s',c'))
>     div0 a b = bool (a/b) 0 (b==0)
> 

-- model of linear beta relationship betweeen a historical statistic and a time series
onlineDep :: Double -> SL.Scan Double Double -> SL.Scan Double Double
onlineDep b (SL.Scan step'' begin) = SL.Scan step begin
  where
    step a = do
      s' <- get
      let a' = a + b * s'
      undefined -- step'' a'

> 
> instance Costrong SL.Scan where
>   unfirst (SL.Scan step begin) =
>     SL.Scan
>     (\a -> state $ \s -> let ((b,d),s') = runState (step (a,d)) s in (b,s'))
>     begin
> 
> -- | a constant scan
> sconst :: a -> SL.Scan a a
> sconst a = SL.Scan (const (pure a)) ()
> 
> -- | delays values by 1
> delay ::
>   -- | initial statistical value
>   a ->
>   SL.Scan a a
> delay x0 = SL.Scan (\a -> state (\s -> (s,a))) x0
> 
> -- | delays values by n steps
> --
> -- delayN [0] == delay 0
> --
> -- delayN [] == id
> --
> -- delayN [1,2] = delay 2 . delay 1
> --
> delayN ::
>   -- | initial statistical values, delay equals length
>   [a] ->
>   SL.Scan a a
> delayN x0 = SL.Scan (state . step) (fromList x0)
>   where
>     step :: a -> Seq a -> (a, Seq a)
>     step a Empty = (a, Empty)
>     step a (x' :<| xs) = (x', xs |> a)
> 
> -- model of linear beta relationship betweeen a historical statistic and a time series
> onlineDep' :: (Double -> Double) -> Double -> Double -> SL.Scan Double Double
> onlineDep' f r b = SL.Scan step begin
>   where
>     step a = state (step' a)
>     begin = Averager (0.0, 0.0)
>     step' a (Averager (s, c)) =
>       let a' = a + b * (s `div0` c) in
>       let (s',c') = ((* (1-r)) (s + f a'), (* (1-r)) (c + 1)) in (a', Averager (s',c'))
>     div0 a b = bool (a/b) 0 (b==0)
> 
> onlineL1 :: (Divisive b, Additive b) => (a -> b) -> (b -> b) -> L1 a b
> onlineL1 f g = L1 extract step intract
>   where
>     intract a = Averager (f a, one)
>     step (Averager (s, c)) a = Averager (g $ s + f a, g $ c + one)
>     extract (Averager (s, c)) = s / c
> 
> onlineL :: (Divisive b, Additive b) => (a -> b) -> (b -> b) -> L a b
> onlineL f g = L extract step begin
>   where
>     begin = Averager (zero,zero)
>     step (Averager (s, c)) a = Averager (g $ s + f a, g $ c + one)
>     extract (Averager (s, c)) = s / c
> 
> maL1 :: (Divisive a, Additive a) => a -> L1 a a
> maL1 r = onlineL1 id (*r)
> 
> data S a b = forall x. S { extract :: x -> b, step :: x -> a -> x, intract :: a -> x}
> 
> instance Functor (S a) where
>   fmap f (S e s i) = S (f . e) s i
> 
> instance Profunctor S where
>   dimap f g (S e s i) = S (g . e) (\a -> s a . f) (i . f)
> 
> onlineS :: (Divisive b, Additive b, Eq b) => (a -> b) -> (b -> b) -> S a b
> onlineS f g = S extract step intract
>   where
>     intract a = Averager (f a, one)
>     step (Averager (s,c)) a = let (Averager (s',c')) = intract a in
>       Averager (g $ s + s', g $ c + c')
>     extract (Averager (s, c)) = bool (s/c) zero (c==zero)
> 
> maS :: (Divisive a, Additive a, Eq a) => a -> S a a
> maS r = onlineS id (*r)
> 

onlineDepS :: (Double -> Double) -> Double -> Double -> S Double Double
onlineDepS f r b = S extract step intract
  where
    intract a = (a, Averager (f a, one))
    step (_, m@(Averager (s,c))) a =
      let (a', Averager (s',c')) = intract (a + b * extract' m) in
      (a', Averager ((r*) $ s + s', (r*) $ c + c'))
    extract (a', _) = a'
    extract' (Averager (s, c)) = bool (s/c) zero (c==zero)

> 
> 
> 
> 

``` {.output .avbeta}
```

``` {.output .betaerrors}
```

> 
> -- | check if beta is recovering a simulated dependency on historical ma,
> -- and doing what is expected.
> -- (The cumulative drift in the specific sample is removed).
> -- >>> betaCheck 0.1 0.01 xs
> -- 0.10000000000000142
> betaCheck :: Double -> Double -> [Double] -> Double
> betaCheck b r xs = xsb - x0
>   where
>     -- ma of underlying stochastic variable
>     ma' = scanL1 (ma (1 - r)) xs
>     -- simulated series
>     xs' = zipWith (\e m -> e + b * m) xs ma'
>     -- beta of ma' on simulated series
>     xsb = foldL1 (beta (ma (1 - r))) $ drop 1 $ zip ma' xs'
>     -- beta measurement if beta of ma was, in reality, zero.
>     x0 =  foldL1 (beta (ma (1 - r))) $ drop 1 $ zip ma' xs
> 
> -- | relationship between historical std and std
> betaStdCheck :: Double -> Double -> [Double] -> Double
> betaStdCheck b r xs = xsb
>   where
>     -- std of underlying stochastic
>     std' = scanL1 (std (1 - r)) xs
>     -- simulated series
>     xs' = zipWith (\o m -> o * (1 + (1 - m) * b)) xs std'
>     -- beta of std' on squared simulated series
>     xsb = foldL1 (beta (ma (1 - r))) $ drop 1 $ zip std' xs'
> 

avMaBetaChart :: Double -> [Double] -> [Double] -> Text
avMaBetaChart b rates xs =
  renderHudOptionsChart
    defaultSvgOptions
    (titlesHud "beta ma check" "rate" "average beta")
    []
    ( (: []) $
        Chart
          (LineA defaultLineStyle)
          (zipWith SP rates ((\r -> maBetaCheck b r xs / r) <$> rates))
    )

> 

> avBetaChart :: Double -> [Double] -> [Double] -> Text
> avBetaChart b rates xs =
>   renderHudOptionsChart
>     defaultSvgOptions
>     (titlesHud "beta measure check" "rate" "average beta * (1/r)")
>     []
>     ( (: []) $
>         Chart
>           (LineA defaultLineStyle)
>           (zipWith SP rates ((\r -> betaCheck b r xs / r) <$> rates))
>     )
> 
> betaErrors :: [Double] -> [Double] -> [Double] -> Array Double
> betaErrors bs rates xs =
>   expand
>     (\r b -> (1 / (r * b)) * betaCheck b r xs)
>     (fromFlatList [length rates] rates)
>     (fromFlatList [length bs] bs)
> 
> betaErrorsChart :: [Double] -> [Double] -> [Double] -> Text
> betaErrorsChart bs rates xs =
>   renderHudOptionsChart
>     defaultSvgOptions
>     (titlesHud "beta measure error" "rates" "betas" & hudOptions)
>     h0
>     c0
>   where
>     f1 :: (Point Double -> Double) = \(Point x y) ->
>       NumHask.Array.Dynamic.index
>         (betaErrors bs rates xs)
>         [fromIntegral (floor x :: Integer), fromIntegral (floor y :: Integer)]
>     (c0, h0) = pixelfl f1 pixelOptions (defaultPixelLegendOptions "")
>     pixelOptions =
>       defaultPixelOptions
>         & #poGrain .~ Point (fromIntegral $ length rates) (fromIntegral $ length bs)
>         & #poRange .~ Rect 0 (fromIntegral $ length rates) 0 (fromIntegral $ length bs)
>     hudOptions =
>       #hudAxes
>         .~ [ defaultAxisOptions
>                & #atick . #tstyle .~ TickLabels (show <$> rates),
>              defaultAxisOptions
>                & #place .~ PlaceLeft
>                & #atick . #tstyle .~ TickLabels (show <$> bs)
>            ]
> 
> -- | the average beta measure given an autocorrelation of the form r = a + b * ma.
> -- (The random variance in the specific beta measurement (ie assuming beta = 0) is removed.)
> avStdBeta :: Double -> Double -> [Double] -> Double
> avStdBeta b r xs =
>   ( \x ->
>       x
>         - foldL1
>           (beta (ma 1))
>           (zip ((\r -> drop 1 $ zipWith (\o m -> o + 0 * m) xs (scanL1 (ma (1 - r)) xs)) 0.001) xs)
>   )
>     $ foldL1 (beta (ma 1))
>     $ zip ((\r -> drop 1 $ zipWith (\o m -> o * (1 + b*m)) xs (scanL1 (std (1 - r)) xs)) r) xs
> 
> -- | the average beta measure given an autocorrelation of the form r = a + b * ma.
> -- (The random variance in the specific beta measurement (ie assuming beta = 0) is removed.)
> avStdBeta' :: Double -> Double -> [Double] -> Double
> avStdBeta' b r xs =
>   foldL1 (beta (ma 1))
>     $ zip (drop 1 $ zipWith (\o m -> o + b*(m - 1)) xs (scanL1 (std (1 - r)) xs)) xs
> 
> 
> -- representation of configuration
> repSimCrossConfig :: (Monad m) => SimCrossConfig -> SharedRep m SimCrossConfig
> repSimCrossConfig cfg = bimap hmap SimCrossConfig n' <<*>> rates' <<*>> beta' <<*>> betas' <<*>> stdmadecay'
>   where
>     n' = either (const (view #rN cfg)) id <$>
>       readTextbox (Just "n") (view #rN cfg)
>     rates' = either (const []) id <$>
>       readTextbox (Just "rates") (view #rRates cfg)
>     beta' = either (const (view #rBeta cfg)) id <$>
>       readTextbox (Just "test beta") (view #rBeta cfg)
>     betas' = either (const []) id <$>
>       readTextbox (Just "test betas") (view #rBetas cfg)
>     stdmadecay' = either (const 1) id <$>
>       readTextbox (Just "std of ma decay rate") (view #rStdMaDecay cfg)
>     hmap n'' rates'' beta'' betas'' stdmadecay'' = n'' <> rates'' <> beta'' <> betas'' <> stdmadecay''
> 
> 
> 
> makeCharts :: SimCrossConfig -> IO [Text]
> makeCharts cfg = do
>   g <- create
>   xs <- rvs g (view #rN cfg)
>   pure $
>     [ stdOfMaChart (view #rStdMaDecay cfg) (view #rRates cfg) xs,
>       avBetaChart (view #rBeta cfg) (view #rRates cfg) xs,
>       betaErrorsChart (view #rBetas cfg) (view #rRates cfg) xs
>     ]
> 
> page :: Text -> [(Text, Html ())] -> Page
> page title sections =
>   bootstrapPage <>
>   bridgePage &
>   #htmlBody .~ divClass_ "container" (mconcat
>     [ divClass_ "row" (h1_ (toHtml title))
>     , divClass_ "row" $ mconcat $ (\(t,h) -> divClass_ "col" (h2_ (toHtml t) <> with div_ [id_ t] h)) <$> sections
>     ])
> 
> initChartsRep
>   :: Engine
>   -> Rep SimCrossConfig
>   -> StateL.StateT (HashMap Text Text) IO ()
> initChartsRep e r =
>   void $ oneRep r
>   (\(Rep h fa) m -> do
>       append e "input" (toText h)
>       case snd (fa m) of
>         Left err -> append e "debug" err
>         Right cfg -> do
>           c <- makeCharts cfg
>           replace e "output" (mconcat c))
> 
> updateChartsRep :: Engine -> Either Text (HashMap Text Text, Either Text SimCrossConfig) -> IO ()
> updateChartsRep e (Left err) = append e "debug" err
> updateChartsRep e (Right (_, Left err)) = append e "debug" err
> updateChartsRep e (Right (_, Right c)) = do
>   t <- makeCharts c
>   replace e "output" (mconcat t)
> 
> serveCharts :: IO ()
> serveCharts = do
>   let cfg = defaultSimCrossConfig
>   scotty 3000 $ do
>     middleware $ midShared (repSimCrossConfig cfg) initChartsRep updateChartsRep
>     servePageWith "/" (defaultPageConfig "prod")
>       (page "simulation of cross-dependent moments"
>        [ ("input", mempty)
>        , ("output", mempty)
>        , ("debug", mempty)
>        ])
> 

