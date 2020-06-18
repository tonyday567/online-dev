{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Online statistics for ordered data (such as time-series data), modelled as [mealy machines](https://en.wikipedia.org/wiki/Mealy_machine)
module Data.Mealy
  ( -- * Types
    Mealy (..),
    pattern M,
    scan,
    fold,
    Averager (..),
    pattern A,
    av,
    av_,
    online,

    -- * Statistics
    -- $setup
    ma,
    absma,
    sqma,
    std,
    cov,
    corrGauss,
    corr,
    beta1,
    alpha1,
    reg1,
    beta,
    alpha,
    reg,
    asum,
    aconst,
    delay1,
    delay,
    depState,
    Model1 (..),
    zeroModel1,
    depModel1,

    -- * conversion
    fromFoldl,
  )
where

import qualified Box
import Chart hiding (one, zero, shape, singleton)
import Control.Category ((>>>), Category)
import Control.Lens hiding ((:>), Empty, Unwrapped, Wrapped, index)
import Control.Monad
import qualified Control.Monad.Trans.State.Lazy as StateL
import Control.Monad.Trans.State.Strict
import qualified Control.Scanl as SL
import Data.Attoparsec.Text (decimal, parseOnly)
import Data.Fold hiding (M)
import Data.Foldable (foldl1)
import Data.Generics.Labels ()
import Data.List (scanl1)
import Data.Maybe
import Data.Profunctor.Strong
import Data.Random
import Data.Sequence (Seq (..), ViewR (..))
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified NumHask.Array.Dynamic as D
import NumHask.Array.Fixed (with)
import qualified NumHask.Array.Fixed as F
import NumHask.Array.Shape (HasShape)
import qualified NumHask.Array.HMatrix as HM
import NumHask.Prelude hiding ((<<*>>), L1, State, StateT, get, replace, runState, runStateT, state, fold, asum)
import Options.Generic
import Readme.Lhs
import System.Random.MWC
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Data as LA
import Data.Functor.Rep
import qualified Control.Foldl as L

{- $setup
Generate some random variates for the examples.

xs0, xs1 & xs2 are samples from N(0,1)

xsp is a pair of N(0,1)s with a correlation of 0.8

>>> :set -XDataKinds
>>> import Data.List
>>> import Data.Random
>>> g <- create
>>> xs0 <- rvs g 10000
>>> xs1 <- rvs g 10000
>>> xs2 <- rvs g 10000
>>> xsp <- rvsp g 10000 0.8

-}

{- | A 'Mealy' is a triple of functions

* (c -> b) __extract__ Convert state to the output type.
* (c -> a -> c) __step__ Update state given prior state and (new) input.
* (a -> c) __inject__ Convert an input into the state type.

The type is a newtype wrapper around 'L1' in 'Data.Fold'.

__inject__ is necessary to kick off state on a 'fold' or 'scan', rather than a state existing prior to the fold or scan (this is a Moore machine or 'M1' in 'Data.Fold').

> scan (M e s i) (x : xs) = e <$> scanl' s (i x) xs

-}
newtype Mealy a b = Mealy {l1 :: L1 a b}
  deriving (Profunctor, Category) via (L1)
  deriving (Functor, Applicative) via (L1 a)

-- | Pattern for a 'Mealy'.
--
-- @M extract step inject@
pattern M :: (c -> b) -> (c -> a -> c) -> (a -> c) -> Mealy a b
pattern M e s i = Mealy (L1 e s i)
{-# COMPLETE M #-}

-- | Fold a list through a 'Mealy'.
--
-- > cosieve == fold
fold :: Mealy a b -> [a] -> b
fold _ [] = panic "on the streets of Birmingham."
fold (M e s i) (x:xs) = e $ foldl' s (i x) xs

-- | Run a list through a 'Mealy' and return a list of values for every step
--
-- > length (scan _ xs) == length xs
scan :: Mealy a b -> [a] -> [b]
scan _ [] = []
scan (M e s i) (x:xs) = fromList (e <$> scanl' s (i x) xs)

-- | Most common statistics are averages, which are some sort of aggregation of values (sum) and some sort of sample size (count).
newtype Averager a b
  = Averager
      { sumCount :: (a, b)
      }
  deriving (Eq, Show)

-- | Pattern for an 'Averager'.
--
-- @A sum count@
pattern A :: a -> b -> Averager a b
pattern A s c = Averager (s, c)
{-# COMPLETE A #-}

instance (Additive a, Additive b) => Semigroup (Averager a b) where
  (<>) (A s c) (A s' c') = A (s + s') (c + c')

-- |
-- > av mempty == nan
instance (Additive a, Additive b) => Monoid (Averager a b) where
  mempty = A zero zero
  mappend = (<>)

-- | extract the average from an 'Averager'
--
-- av gives NaN on zero divide
av :: (Divisive a) => Averager a a -> a
av (A s c) = s / c

-- | substitute a default value on zero-divide
--
-- > av_ (Averager (0,0)) x == x
av_ :: (Eq a, Additive a, Divisive a) => Averager a a -> a -> a
av_ (A s c) def = bool def (s / c) (c == zero)

-- | @online f g@ is a 'Mealy' where f is a transformation of the data and g is a decay function (convergent tozero) applied at each step.
--
-- > online id id == av
online :: (Divisive b, Additive b) => (a -> b) -> (b -> b) -> Mealy a b
online f g = M av step intract
  where
    intract a = A (f a) one
    step (A s c) a =
      let (A s' c') = intract a
       in A (g s + s') (g c + c')

{- | A moving average using a decay rate of r. r=1 represents the simple average, and r=0 represents the latest value.

>>> fold (ma 0) (fromList [1..100])
100.0

>>> fold (ma 1) (fromList [1..100])
50.5

>>> fold (ma 0.99) xs0
-4.292501077490672e-2

A change in the underlying mean at n=10000 in the chart below highlights the trade-off between stability of the statistic and response to non-stationarity.

![ma chart](other/ex-ma.svg)

-}
ma :: (Divisive a, Additive a) => a -> Mealy a a
ma r = online id (* r)
{-# INLINEABLE ma #-}

-- | absolute average
--
-- >>> fold (absma 1) xs0
-- 0.7894201075535578
absma :: (Divisive a, Additive a, Signed a) => a -> Mealy a a
absma r = online abs (* r)
{-# INLINEABLE absma #-}

-- | average square
--
-- > fold (ma r) . fmap (**2) == fold (sqma r)
sqma :: (Divisive a, Additive a) => a -> Mealy a a
sqma r = online (\x -> x * x) (* r)
{-# INLINEABLE sqma #-}

{- | standard deviation

The construction of standard deviation, using the Applicative instance of a 'Mealy':

> (\s ss -> sqrt (ss - s ** (one+one))) <$> ma r <*> sqma r

The average deviation of the numbers 1..1000 is about 1 / sqrt 12 * 1000
<https://en.wikipedia.org/wiki/Uniform_distribution_(continuous)#Standard_uniform>

>>> fold (std 1) [0..1000]
288.9636655359978

The average deviation with a decay of 0.99

>>> fold (std 0.99) [0..1000]
99.28328803163829

>>> fold (std 1) xs0
0.9923523681261158

![std chart](other/ex-std.svg)
-}
std :: (Divisive a, ExpField a) => a -> Mealy a a
std r = (\s ss -> sqrt (ss - s ** (one + one))) <$> ma r <*> sqma r
{-# INLINEABLE std #-}

-- | The covariance of a tuple given an underlying central tendency fold.
--
-- >>> fold (cov (ma 1)) xsp
-- 0.8011368250045314
--
cov :: (Field a) => Mealy a a -> Mealy (a, a) a
cov m =
  (\xy x' y' -> xy - x' * y') <$> lmap (uncurry (*)) m <*> lmap fst m <*> lmap snd m
{-# INLINEABLE cov #-}

-- | correlation of a tuple, specialised to Guassian
--
-- >>> fold (corrGauss 1) xsp
-- 0.8020637696465039
corrGauss :: (ExpField a) => a -> Mealy (a, a) a
corrGauss r =
  (\cov' stdx stdy -> cov' / (stdx * stdy)) <$> cov (ma r)
    <*> lmap fst (std r)
    <*> lmap snd (std r)
{-# INLINEABLE corrGauss #-}

-- | a generalised version of correlation of a tuple
--
-- >>> fold (corr (ma 1) (std 1)) xsp
-- 0.8020637696465039
--
-- > corr (ma r) (std r) == corrGauss r
corr :: (ExpField a) => Mealy a a -> Mealy a a -> Mealy (a, a) a
corr central deviation =
  (\cov' stdx stdy -> cov' / (stdx * stdy)) <$> cov central
    <*> lmap fst deviation
    <*> lmap snd deviation
{-# INLINEABLE corr #-}

{- | The beta in a simple linear regression of an (independent variable, single dependent variable) tuple given an underlying central tendency fold.

This is a generalisation of the classical regression formula, where averages are replaced by 'Mealy' statistics.

\[
\begin{align}
\beta & = \frac{n\sum xy - \sum x \sum y}{n\sum x^2 - (\sum x)^2} \\
     & = \frac{n^2 \overline{xy} - n^2 \bar{x} \bar{y}}{n^2 \overline{x^2} - n^2 \bar{x}^2} \\
     & = \frac{\overline{xy} - \bar{x} \bar{y}}{\overline{x^2} - \bar{x}^2} \\
\end{align}
\]

>>> fold (beta1 (ma 1)) $ zipWith (\x y -> (y, x + y)) xs0 xs1
0.9953875263096014
-}
beta1 :: (ExpField a) => Mealy a a -> Mealy (a, a) a
beta1 m =
  (\xy x' y' x2 -> (xy - x' * y') / (x2 - x' * x')) <$> lmap (uncurry (*)) m
    <*> lmap fst m
    <*> lmap snd m
    <*> lmap (\(x, _) -> x * x) m
{-# INLINEABLE beta1 #-}

{- | The alpha in a simple linear regression of an (independent variable, single dependent variable) tuple given an underlying central tendency fold.

\[
\begin{align}
\alpha & = \frac{\sum y \sum x^2 - \sum x \sum xy}{n\sum x^2 - (\sum x)^2} \\
     & = \frac{n^2 \bar{y} \overline{x^2} - n^2 \bar{x} \overline{xy}}{n^2 \overline{x^2} - n^2 \bar{x}^2} \\
     & = \frac{\bar{y} \overline{x^2} - \bar{x} \overline{xy}}{\overline{x^2} - \bar{x}^2} \\
\end{align}
\]

>>> fold (alpha1 (ma 1)) $ zipWith (\x y -> ((3+y), x + 0.5 * (3 + y))) xs0 xs1
1.1880996822796197e-2

-}
alpha1 :: (ExpField a) => Mealy a a -> Mealy (a, a) a
alpha1 m = (\x b y -> y - b * x) <$> lmap fst m <*> beta1 m <*> lmap snd m
{-# INLINEABLE alpha1 #-}

{- | The (alpha, beta) tuple in a simple linear regression of an (independent variable, single dependent variable) tuple given an underlying central tendency fold.

>>> fold (reg1 (ma 1)) $ zipWith (\x y -> ((3+y), x + 0.5 * (3 + y))) xs0 xs1
(1.1880996822796197e-2,0.49538752630956845)

-}
reg1 :: (ExpField a) => Mealy a a -> Mealy (a, a) (a, a)
reg1 m = (,) <$> alpha1 m <*> beta1 m

data RegressionState (n :: Nat) a =
    RegressionState
    { _xx :: F.Array '[n,n] a,
      _x :: F.Array '[n] a,
      _xy :: F.Array '[n] a,
      _y :: a
    } deriving (Functor)

{- | multiple regression

\[
\begin{align}
{\hat  {{\mathbf  {B}}}}=({\mathbf  {X}}^{{{\rm {T}}}}{\mathbf  {X}})^{{ -1}}{\mathbf  {X}}^{{{\rm {T}}}}{\mathbf  {Y}}
\end{align}
\]

\[
\begin{align}
{\mathbf  {X}}={\begin{bmatrix}{\mathbf  {x}}_{1}^{{{\rm {T}}}}\\{\mathbf  {x}}_{2}^{{{\rm {T}}}}\\\vdots \\{\mathbf  {x}}_{n}^{{{\rm {T}}}}\end{bmatrix}}={\begin{bmatrix}x_{{1,1}}&\cdots &x_{{1,k}}\\x_{{2,1}}&\cdots &x_{{2,k}}\\\vdots &\ddots &\vdots \\x_{{n,1}}&\cdots &x_{{n,k}}\end{bmatrix}}
\end{align}
\]

>>> let ys = zipWith3 (\x y z -> 0.1 * x + 0.5 * y + 1 * z) xs0 xs1 xs2
>>> let zs = zip (zipWith (\x y -> fromList [x,y] :: F.Array '[2] Double) xs1 xs2) ys
>>> fold (beta 0.99) zs
[0.4982692361226971, 1.038192474255091]

-}
beta :: (Field a, LA.Field a, KnownNat n) => a -> Mealy (F.Array '[n] a, a) (F.Array '[n] a)
beta r = M extract step inject
  where
    extract (A (RegressionState xx x xy y) c) =
      liftHM2 (\a b -> (LA.pinv a) LA.<> LA.tr b)
      ((one/c) *. (xx - (F.expand (*) x x)))
      ((xy - (y *. x)) .* (one/c))
    step x (xs, y) = rsOnline r x (inject (xs,y))
    inject (xs, y) =
      A (RegressionState (F.expand (*) xs xs) xs (y *. xs) y) one
{-# INLINEABLE beta #-}

liftHM2 :: (LA.Field a, HasShape s, HasShape s', HasShape s'') => (LA.Matrix a -> LA.Matrix a -> LA.Matrix a) -> F.Array s a -> F.Array s' a -> F.Array s'' a
liftHM2 f a b = HM.toFixed $ HM.Array $ f (HM.unArray . HM.fromFixed $ a) (HM.unArray . HM.fromFixed $ b)

rsOnline :: (Field a, KnownNat n) => a -> Averager (RegressionState n a) a -> Averager (RegressionState n a) a -> Averager (RegressionState n a) a
rsOnline r (A (RegressionState xx x xy y) c) (A (RegressionState xx' x' xy' y') c') =
  A (RegressionState (liftR2 d xx xx') (liftR2 d x x') (liftR2 d xy xy') (d y y')) (d c c')
  where
    d s s' = r * s + s'

-- | alpha in a multiple regression
alpha :: (LA.Field a, ExpField a, KnownNat n) => a -> Mealy (F.Array '[n] a, a) a
alpha r = (\xs b y -> y - sum (liftR2 (*) b xs)) <$> lmap fst (arrayify $ ma r) <*> beta r <*> lmap snd (ma r)
{-# INLINEABLE alpha #-}

arrayify :: (HasShape s) => Mealy a b -> Mealy (F.Array s a) (F.Array s b)
arrayify (M sExtract sStep sInject) = M extract step inject
  where
    extract as = fmap sExtract as
    step xs as = liftR2 sStep xs as
    inject as = fmap sInject as

{- | multiple regression

>>> let ys = zipWith3 (\x y z -> 0.1 * x + 0.5 * y + 1 * z) xs0 xs1 xs2
>>> let zs = zip (zipWith (\x y -> fromList [x,y] :: F.Array '[2] Double) xs1 xs2) ys
>>> fold (reg 0.99) zs
([0.4982692361226971, 1.038192474255091],2.087160803386695e-3)

-}
reg :: (LA.Field a, ExpField a, KnownNat n) => a -> Mealy (F.Array '[n] a, a) (F.Array '[n] a, a)
reg r = (,) <$> beta r <*> alpha r
{-# INLINEABLE reg #-}

-- | accumulated sum
asum :: (Additive a) => Mealy a a
asum = M id (+) id

-- | constant Mealy
aconst :: b -> Mealy a b
aconst a = M (\_ -> a) (\_ _ -> ()) (\_ -> ())

-- | delay input values by 1
delay1 :: a -> Mealy a a
delay1 x0 = M fst (\(_, x) a -> (x, a)) (\a -> (x0, a))


{- | delays values by n steps

delay [0] == delay1 0

delay [] == id

delay [1,2] = delay1 2 . delay1 1

>>> scan (delay [-2,-1]) [0..3]
[-2,-1,0,1]

Autocorrelation example:

> scan (((,) <$> id <*> delay [0]) >>> beta (ma 0.99)) xs0

-}
delay ::
  -- | initial statistical values, delay equals length
  [a] ->
  Mealy a a
delay x0 = M extract step inject
  where
    inject a = (Seq.fromList x0) |> a
    extract :: Seq a -> a
    extract Empty = panic "ACAB"
    extract (x :<| _) = x
    step :: Seq a -> a -> Seq a
    step Empty _ = panic "ACAB"
    step (_ :<| xs) a = xs |> a

{- | Add a state dependency to a series.

Typical regression analytics tend to assume that moments of a distributional assumption are unconditional with respect to prior instantiations of the stochastics being studied.

For time series analytics, a major preoccupation is estimation of the current moments given what has happened in the past.

IID:

\[
\begin{align}
x_{t+1} & = alpha_t^x + s_{t+1}\\
s_{t+1} & = alpha_t^s * N(0,1)
\end{align}
\]

Example: including a linear dependency on moving average history:

\[
\begin{align}
x_{t+1} & = (alpha_t^x + beta_t^{x->x} * ma_t^x) + s_{t+1}\\
s_{t+1} & = alpha_t^s * N(0,1)
\end{align}
\]

>>> let xs' = scan (depState (\a m -> a + 0.1 * m) (ma 0.99)) xs0
>>> let ma' = scan ((ma (1 - 0.01)) >>> delay [0]) xs'
>>> let xsb = fold (beta1 (ma (1 - 0.001))) $ drop 1 $ zip ma' xs'
>>> -- beta measurement if beta of ma was, in reality, zero.
>>> let xsb0 = fold (beta1 (ma (1 - 0.001))) $ drop 1 $ zip ma' xs0
>>> xsb - xsb0
9.999999999999976e-2

This simple model of relationship between a series and it's historical average shows how fragile the evidence can be.

![madep](other/ex-madep.svg)

In unravelling the drivers of this result, the standard deviation of a moving average scan seems well behaved for r > 0.01, but increases substantively for values less than this.  This result seems to occur for wide beta values. For high r, the standard deviation of the moving average seems to be proprtional to r**0.5, and equal to around (0.5*r)**0.5.

> fold (std 1) (scan (ma (1 - 0.01)) xs0)

![stdma](other/ex-stdma.svg)

-}
depState :: (a -> b -> a) -> Mealy a b -> Mealy a a
depState f (M sExtract sStep sInject) = M fst step inject
  where
    inject a = (a, sInject a)
    step (_, m) a = let a' = f a (sExtract m) in (a', sStep m a')

{- | a linear model of state dependencies for the first two moments

\[
\begin{align}
x_{t+1} & = (alpha_t^x + beta_t^{x->x} * ma_t^x + beta_t^{s->x} * std_t^x) + s_{t+1}\\
s_{t+1} & = (alpha_t^s + beta_t^{x->s} * ma_t^x + beta_t^{s->s} * std_t^x) * N(0,1)
\end{align}
\]

-}
data Model1
  = Model1
      { alphaX :: Double,
        alphaS :: Double,
        betaMa2X :: Double,
        betaMa2S :: Double,
        betaStd2X :: Double,
        betaStd2S :: Double
      }
  deriving (Eq, Show, Generic)

zeroModel1 :: Model1
zeroModel1 = Model1 0 0 0 0 0 0

-- | Apply a model1 relationship using a single decay factor.
--
-- >>> :set -XOverloadedLabels
-- >>> fold (depModel1 0.01 (zeroModel1 & #betaMa2X .~ 0.1)) xs0
-- -0.47228537123218206
depModel1 :: Double -> Model1 -> Mealy Double Double
depModel1 r m1 =
  depState fX st
  where
    st = (,) <$> ma (1 - r) <*> std (1 - r)
    fX a (m, s) =
      a
        * ( (1 + m1 ^. #alphaS)
              + (m1 ^. #betaMa2S) * m
              + (m1 ^. #betaStd2S) * (s - 1)
          )
        + m1 ^. #alphaX
        + (m1 ^. #betaMa2X) * m
        + (m1 ^. #betaStd2X) * (s - 1)

fromFoldl :: L.Fold a b -> Mealy a b
fromFoldl (L.Fold step begin e) = M e step (step begin)