{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
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

-- | Online statistics for ordered data (such as time-series data), modelled as [mealy machines](https://en.wikipedia.org/wiki/Mealy_machine)

module Stats
  ( -- * Types
    Mealy(..),
    pattern M,
    scanS,
    foldS,
    Averager(..),
    pattern A,
    avSum,
    avCount,
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
    beta,
    alpha,
    reg,
    maDependency,
    sconst,
    delay1,
    delay,
  ) where

import Chart hiding (one, zero)
import Control.Category ((>>>), Category(..))
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
import Options.Generic
import qualified Box
import qualified Data.Text as Text
import Control.Monad
import Data.Maybe
import Control.Monad.Trans.State.Strict
import qualified Control.Monad.Trans.State.Lazy as StateL
import Data.Sequence (ViewR(..), Seq(..))
import qualified Data.Sequence as Seq
import Data.Profunctor.Strong
import Data.Fold hiding (M)
import Data.Foldable (foldl1)
import Data.List (scanl1)

-- $setup
-- Generate some random variates for the examples.
--
-- xs0 & xs1 are random variates with mean 0 and sd 1
--
-- xsp is a pair of N(0,1)s with a correlation of 0.8
--
-- >>> import Data.Random
-- >>> g <- create
-- >>> xs0 <- rvs g 10000
-- >>> xs1 <- rvs g 10000
-- >>> xsp <- rvsp g 10000 0.8

{- | A Mealy is a triple of functions

 * (c -> b) __extract__ Convert state to the output type.
 * (c -> a -> c) __step__ Update state given prior state and (new) input.
 * (a -> c) __inject__ Convert an input into the state type.

The type is a newtype wrapper around 'L1' in 'Data.Fold'.

-}
newtype Mealy a b = Mealy { l1 :: L1 a b }
  deriving (Profunctor, Category) via (L1)
  deriving (Functor, Applicative) via (L1 a)

-- | Pattern for a 'Mealy'.
--
-- @M extract step inject@
pattern M :: (c -> b) -> (c -> a -> c) -> (a -> c) -> Mealy a b
pattern M e s i = Mealy (L1 e s i)
{-# COMPLETE M #-}

-- | Most common statistics are averages.
newtype Averager a = Averager
  { sumCount :: (a, a)
  } deriving (Eq, Show)

-- | Pattern for an 'Averager'.
--
-- A sum count
pattern A :: a -> a -> Averager a
pattern A s c = Averager (s,c)
{-# COMPLETE A #-}

avSum :: Averager a -> a
avSum (A s _) = s

avCount :: Averager a -> a
avCount (A _ c) = c

instance (Additive a) => Semigroup (Averager a) where
  (<>) (A s c) (A s' c') = A (s+s') (c+c')

-- |
-- > av mempty == nan
instance (Additive a) => Monoid (Averager a) where
  mempty = A zero zero
  mappend = (<>)

-- | extract the average from an 'Averager'
--
-- av gives NaN on zero divide
av :: (Divisive a) => Averager a -> a
av (A s c) = s/c

-- | substitute a default value on zero-divide
--
-- > av_ (Averager (0,0)) x == x
av_ :: (Eq a, Additive a, Divisive a) => Averager a -> a -> a
av_ (A s c) def = bool def (s/c) (c==zero)

-- | @online f g@ is a mealy machine where f is a statistic (a transformation of the underlying stream) and g is a function (usually converging towards zero) applied at each iteration of the stream.
--
-- > online id id == av
online :: (Divisive b, Additive b) => (a -> b) -> (b -> b) -> Mealy a b
online f g = M av step intract
  where
    intract a = A (f a) one
    step (A s c) a = let (A s' c') = intract a in
      A (g s + s') (g c + c')

-- | moving average
--
-- >>> foldS (ma 1) (fromList [1..100])
-- 50.5
--
-- >>> foldS (ma 0.99) xs0
-- -4.292501077490672e-2
--
-- A change in the underlying mean at n=10000 in the chart below highlights the trade-off between stability of the statistic and response to non-stationarity.
--
-- ![ma chart](other/ex-ma.svg)
--
ma :: (Divisive a, Additive a) => a -> Mealy a a
ma r = online id (* r)
{-# INLINABLE ma #-}

-- | absolute average
--
-- >>> foldS (absma 1) xs0
-- 0.7894201075535578
absma :: (Divisive a, Additive a, Signed a) => a -> Mealy a a
absma r = online abs (* r)
{-# INLINABLE absma #-}

-- | average square
--
-- > foldS (ma r) . fmap (**2) == foldS (sqma r)
sqma :: (Divisive a, Additive a) => a -> Mealy a a
sqma r = online (\x -> x * x) (* r)
{-# INLINABLE sqma #-}

-- | standard deviation
--
-- The construction of standard deviation, using the Applicative instance of a 'Mealy':
--
-- > (\s ss -> sqrt (ss - s ** (one+one))) <$> ma r <*> sqma r
--
-- The average deviation of the numbers 1..1000 is about 1 / sqrt 12 * 1000
-- <https://en.wikipedia.org/wiki/Uniform_distribution_(continuous)#Standard_uniform>
--
-- >>> foldS (std 1) [0..1000]
-- 288.9636655359978
--
-- The average deviation with a decay of 0.99
--
-- >>> foldS (std 0.99) [0..1000]
-- 99.28328803163829
--
-- >>> foldS (std 1) xs0
-- 0.9923523681261158
--
-- ![std chart](other/ex-std.svg)
std :: (Divisive a, ExpField a) => a -> Mealy a a
std r = (\s ss -> sqrt (ss - s ** (one + one))) <$> ma r <*> sqma r
{-# INLINABLE std #-}

-- | the covariance of a tuple
-- given an underlying central tendency fold
--
-- >>> foldS (cov (ma 1)) xsp
-- 0.8097906344821321
cov :: (Field a) => Mealy a a -> Mealy (a, a) a
cov m =
  (\xy x' y' -> xy - x' * y') <$> lmap (uncurry (*)) m <*> lmap fst m <*> lmap snd m
{-# INLINABLE cov #-}

-- | correlation of a tuple, specialised to Guassian
--
-- >>> foldS (corrGauss 1) xsp
-- 0.8020245281556085
corrGauss :: (ExpField a) => a -> Mealy (a, a) a
corrGauss r =
  (\cov' stdx stdy -> cov' / (stdx * stdy)) <$> cov (ma r) <*>
  lmap fst (std r) <*>
  lmap snd (std r)
{-# INLINABLE corrGauss #-}

-- | a generalised version of correlation of a tuple
--
-- >>> foldS (corr (ma 1) (std 1)) xsp
-- 0.8020245281556085
--
-- > corr (ma r) (std r) == corrGauss r
corr :: (ExpField a) => Mealy a a -> Mealy a a -> Mealy (a, a) a
corr central deviation =
  (\cov' stdx stdy -> cov' / (stdx * stdy)) <$> cov central <*>
  lmap fst deviation <*>
  lmap snd deviation
{-# INLINABLE corr #-}

{- | The beta in a simple linear regression of a (independent variable, dependent variable) tuple given an underlying central tendency fold.

This is a generalisation of the classical regression formulae, where averages are replaced by mealy statistics.

\[
\begin{align}
\beta & = \frac{n\sum xy - \sum x \sum y}{n\sum x^2 - (\sum x)^2} \\
      & = \frac{n^2 \overline{xy} - n^2 \bar{x} \bar{y}}{n^2 \overline{x^2} - n^2 \bar{x}^2} \\
      & = \frac{\overline{xy} - \bar{x} \bar{y}}{\overline{x^2} - \bar{x}^2} \\
\end{align}
\]

>>> foldS (beta (ma 1)) $ zipWith (\x y -> (y, x + y)) xs0 xs1
0.9953875263096014

-}
beta :: (ExpField a) => Mealy a a -> Mealy (a, a) a
beta m =
  (\xy x' y' x2 -> (xy - x' * y') / (x2 - x' * x')) <$> lmap (uncurry (*)) m <*>
  lmap fst m <*>
  lmap snd m <*>
  lmap (\(x, _) -> x * x) m
{-# INLINABLE beta #-}

{- | The alpha in a simple linear regression of a (independent variable, dependent variable) tuple given an underlying central tendency fold.

\[
\begin{align}
\alpha & = \frac{\sum y \sum x^2 - \sum x \sum xy}{n\sum x^2 - (\sum x)^2} \\
      & = \frac{n^2 \bar{y} \overline{x^2} - n^2 \bar{x} \overline{xy}}{n^2 \overline{x^2} - n^2 \bar{x}^2} \\
      & = \frac{\bar{y} \overline{x^2} - \bar{x} \overline{xy}}{\overline{x^2} - \bar{x}^2} \\
\end{align}
\]

>>> foldS (alpha (ma 1)) $ zipWith (\x y -> ((3+y), x + 0.5 * (3 + y))) xs0 xs1
1.1880996822796197e-2

-}
alpha :: (ExpField a) => Mealy a a -> Mealy (a, a) a
alpha m = (\x b y -> y - b * x) <$> lmap fst m <*> beta m <*> lmap snd m
{-# INLINABLE alpha #-}

-- | The (alpha, beta) tuple in a simple linear regression of a (independent variable, dependent variable) tuple given an underlying central tendency fold.
--
-- >>> foldS (reg (ma 1)) $ zipWith (\x y -> ((3+y), x + 0.5 * (3 + y))) xs0 xs1
-- (1.1880996822796197e-2,0.49538752630956845)
reg :: (ExpField a) => Mealy a a -> Mealy (a, a) (a, a)
reg m = (,) <$> alpha m <*> beta m

-- | a constant scan
sconst :: b -> Mealy a b
sconst a = M (\_ -> a) (\_ _ -> ()) (\_ -> ())

-- | delay input values by 1
delay1 :: a -> Mealy a a
delay1 x0 = M fst (\(_,x) a -> (x,a)) (\a -> (x0,a))

-- | delays values by n steps
--
-- delay [0] == delay 0
--
-- delay [] == id
--
-- delay [1,2] = delay1 2 . delay1 1
--
-- >>> scanS (delay [-2,-1]) [0..3]
-- [-2,-1,0,1]
--
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


-- | fold a list through a Mealy statistic and output the final value
--
-- > cosieve == foldS (kind of)
foldS :: Mealy a b -> [a] -> b
foldS _ [] = panic "Fuck Up"
foldS (M e s i) (x:xs) = e $ foldl' s (i x) xs

-- | run a list through a mealy machine and return a list of values for every step
scanS :: Mealy a b -> [a] -> [b]
scanS _ [] = []
scanS (M e s i) (x:xs) = e <$> scanl' s (i x) xs

{- | Converts an iid random variate series to a series with a depenncy on the historical moving average.

Typical regression analytics tend to assume that moments of a distributional assumption are unconditional with respect to prior instantiations of the stochastics being studied.

For time series analytics, a major preoccupation is estimation of the current moments given what has happened in the past.

IID:

\[
\begin{align}
x_{t+1} & = alpha_t^x + s_{t+1}\\
s_{t+1} & = alpha_t^s * N(0,1)
\end{align}
\]

Including a linear dependency on moving average history:

\[
\begin{align}
x_{t+1} & = (alpha_t^x + beta_t^{x->x} * ma_t^x) + s_{t+1}\\
s_{t+1} & = alpha_t^s * N(0,1)
\end{align}
\]

>>> let xs' = scanS (maDependency 0.1 (ma (1 - 0.01))) xs0
>>> let ma' = scanS ((ma (1 - 0.01)) >>> delay [0]) xs'
>>> let xsb = foldS (beta (ma (1 - 0.001))) $ fromList $ drop 1 $ zip ma' xs'
>>> -- beta measurement if beta of ma was, in reality, zero.
>>> let xsb0 = foldS (beta (ma (1 - 0.001))) $ fromList $ drop 1 $ zip ma' xs0
>>> xsb - xsb0
9.999999999999976e-2

This simple model of relationship between a series and it's historical average shows how fragile the evidence can be.

![madep](ex-madep.svg)

In unravelling the drivers of this result, the standard deviation of a moving average scan seems well behaved for r > 0.01, but increases substantively for values less than this.  This result seems to occur for wide beta values. For high r, the standard deviation of the moving average seems to be proprtional to r**0.5, and equal to around (0.5*r)**0.5.

> foldS (std 1) (scanS (ma (1 - 0.01)) xs0)

![stdma](ex-stdma.svg)

-}
maDependency :: Double -> Mealy Double Double -> Mealy Double Double
maDependency b (M sExtract sStep sIntract) = M extract step intract
  where
    intract a = (a, sIntract a)
    step (_, m) a =
      let a' = a + b * sExtract m in
      (a', sStep m a')
    extract (a', _) = a'

{-
ToDo:

\[
\begin{align}
x_{t+1} & = (alpha_t^x + beta_t^{x->x} * ma_t^x + beta_t^{s->x} * std_t^x) + s_{t+1}\\
s_{t+1} & = (alpha_t^s + beta_t^{x->s} * ma_t^x + beta_t^{s->s} * std_t^x) * N(0,1)
\end{align}
\]

-}

{-




-}
