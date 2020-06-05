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
  ( L1(..),
    scanL1,
    foldL1,
    Averager(..),
    avSum,
    avCount,
    av,
    av_,
    online,
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
    onlineDep,
  ) where

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
import Options.Generic
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
import Data.List (scanl1)

-- $setup
-- >>> import Data.Random
-- >>> g <- create
-- >>> xs0 <- rvs g 10000
-- >>> xs1 <- rvs g 10000
-- >>> xsp <- rvsp g 10000 0.8

newtype Mealy a b = Mealy { l1 :: L1 a b }
  deriving Profunctor via (L1 a b)

-- | Most common statistics are averages.
newtype Averager a = Averager
  { sumCount :: (a, a)
  } deriving (Eq, Show)

avSum :: Averager a -> a
avSum (Averager (s,_)) = s

avCount :: Averager a -> a
avCount (Averager (_,c)) = c

instance (Additive a) => Semigroup (Averager a) where
  (<>) (Averager (s,c)) (Averager (s',c')) = Averager (s+s', c+c')

-- |
-- > av mempty == nan
instance (Additive a) => Monoid (Averager a) where
  mempty = Averager (zero,zero)
  mappend = (<>)

-- | extract the average from an 'Averager'
--
-- av gives NaN on zero divide
av :: (Divisive a) => Averager a -> a
av (Averager (s,c)) = s/c

-- | substitute a default value on zero-divide
--
-- > av_ (Averager (0,0)) x == x
av_ :: (Eq a, Additive a, Divisive a) => Averager a -> a -> a
av_ (Averager (s,c)) def = bool def (s/c) (c==zero)

-- | @online f g@ is a mealy machine where f is a statistic (a transformation of the underlying stream) and g is a function (usually converging towards zero) applied at each iteration of the stream.
--
-- > online id id == av
online :: (Divisive b, Additive b) => (a -> b) -> (b -> b) -> L1 a b
online f g = L1 av step intract
  where
    intract a = Averager (f a, one)
    step (Averager (s,c)) a = let (Averager (s',c')) = intract a in
      Averager (g s + s', g c + c')

-- | moving average
--
-- >>> foldL1 (ma 1) (fromList [1..100])
-- 50.5
--
-- >>> foldL1 (ma 0.99) xs0
-- -4.292501077490672e-2
ma :: (Divisive a, Additive a) => a -> L1 a a
ma r = online id (* r)
{-# INLINABLE ma #-}

-- | absolute average
--
-- >>> foldL1 (absma 1) xs0
-- 0.7894201075535578
absma :: (Divisive a, Additive a, Signed a) => a -> L1 a a
absma r = online abs (* r)
{-# INLINABLE absma #-}

-- | average square
-- > foldL1 (ma r) . fmap (**2) == foldL1 (sqma r)
sqma :: (Divisive a, Additive a) => a -> L1 a a
sqma r = online (\x -> x * x) (* r)
{-# INLINABLE sqma #-}

-- | standard deviation
--
-- The formulae for standard deviation, expressed in online terminology, highlights how this statistic is composed of averages:
--
-- > (\s ss -> sqrt (ss - s ** (one+one))) <$> ma r <*> sqma r
--
-- The average deviation of the numbers 1..1000 is about 1 / sqrt 12 * 1000 (see <<https://en.wikipedia.org/wiki/Uniform_distribution_(continuous)#Standard_uniform wiki>>)
--
-- >>> foldL1 (std 1) $ fromList [0..1000]
-- 288.9636655359978
--
-- The average deviation with a decay of 0.99
--
-- >>> foldL1 (std 0.99) $ fromList [0..1000]
-- 99.28328803163829
--
-- >>> foldL1 (std 1) xs0
-- 0.9923523681261158
--
std :: (Divisive a, ExpField a) => a -> L1 a a
std r = (\s ss -> sqrt (ss - s ** (one + one))) <$> ma r <*> sqma r
{-# INLINABLE std #-}

-- | the covariance of a tuple
-- given an underlying central tendency fold
--
-- >>> foldL1 (cov (ma 1)) xsp
-- 0.8097906344821321
cov :: (Field a) => L1 a a -> L1 (a, a) a
cov m =
  (\xy x' y' -> xy - x' * y') <$> lmap (uncurry (*)) m <*> lmap fst m <*> lmap snd m
{-# INLINABLE cov #-}

-- | correlation of a tuple, specialised to Guassian
--
-- >>> foldL1 (corrGauss 1) xsp
-- 0.8020245281556085
corrGauss :: (ExpField a) => a -> L1 (a, a) a
corrGauss r =
  (\cov' stdx stdy -> cov' / (stdx * stdy)) <$> cov (ma r) <*>
  lmap fst (std r) <*>
  lmap snd (std r)
{-# INLINABLE corrGauss #-}

-- | a generalised version of correlation of a tuple
--
-- >>> foldL1 (corr (ma 1) (std 1)) xsp
-- 0.8020245281556085
--
-- > corr (ma r) (std r) == corrGauss r
corr :: (ExpField a) => L1 a a -> L1 a a -> L1 (a, a) a
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

>>> foldL1 (beta (ma 1)) $ fromList $ zipWith (\x y -> (y, x + y)) (toList xs0) (toList xs1)
0.9953875263096014

-}

beta :: (ExpField a) => L1 a a -> L1 (a, a) a
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

-}
alpha :: (ExpField a) => L1 a a -> L1 (a, a) a
alpha m = (\x b y -> y - b * x) <$> lmap fst m <*> beta m <*> lmap snd m
{-# INLINABLE alpha #-}

-- | with an online fold for the central tendency, return an (alpha, beta) regression values tuple.
reg :: (ExpField a) => L1 a a -> L1 (a, a) (a, a)
reg m = (,) <$> alpha m <*> beta m

-- | fold a list through a mealy machine and output the final value
-- > cosieve == foldL1
foldL1 :: L1 a b -> [a] -> b
foldL1 _ [] = panic "Fuck Up"
foldL1 (L1 e s i) (x:xs) = e $ foldl' s (i x) xs

-- | run a list through a mealy machine and return a list of values for every step
scanL1 :: L1 a b -> [a] -> [b]
scanL1 _ [] = []
scanL1 (L1 e s i) (x:xs) = e <$> scanl' s (i x) xs

{- | representation of a dependency between a time series and its own historical statistic

Typical regression analytics tend to assume that moments of a distributional assumption are unconditional with respect to prior instantiations of the stochastics being studied.

For time series analytics, a major preoccupation is estimation of the current moments given what has happened in the past.

The classical regression statistics can work just as well for moments that are time varying.

alpha
---



Unconditional regression estimates
---

\[
\begin{align}
x_{t+1} & = alpha_t^x + s_{t+1}\\
s_{t+1} & = alpha_t^s * N(0,1)
\end{align}
\[

Including a dependency on moving average history.
---

\[
\begin{align}
x_{t+1} & = (alpha_t^x + beta_t^{x->x} * ma_t^x) + s_{t+1}\\
s_{t+1} & = alpha_t^s * N(0,1)
\end{align}
\]

sqrt(2) ~ (\r -> (r ** 0.5) * (1/(L.fold (std 1) $ drop 1 $ (L.scan (ma (1-r)) rs)))) r for wide r.


Dependencies across moments
---

\[
\begin{align}
x_{t+1} & = (alpha_t^x + beta_t^{x->x} * ma_t^x + beta_t^{s->x} * std_t^x) + s_{t+1}\\
s_{t+1} & = (alpha_t^s + beta_t^{x->s} * ma_t^x + beta_t^{s->s} * std_t^x) * N(0,1)
\end{align}
\]

-}
onlineDep :: Double -> L1 Double Double -> L1 Double Double
onlineDep b (L1 sExtract sStep sIntract) = L1 extract step intract
  where
    intract a = (a, sIntract a)
    step (_, m) a =
      let (a', m') = intract (a + b * sExtract m) in
      (a', sStep m' a')
    extract (a', _) = a'

