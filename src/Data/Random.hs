{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Random where

import NumHask.Prelude
import Online
import Control.Monad.Primitive (PrimState)
import System.Random.MWC
import System.Random.MWC.Probability hiding (beta)
import qualified Control.Foldl as L

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> gen <- create
-- >>> let n = 3
-- >>> let eq' a b = all nearZero $ zipWith (-) a b
-- >>> let eq'p a b = all (\x -> x) $ zipWith (\(x0,x1) (y0,y1) -> nearZero (x0-y0) && nearZero (x1-y1)) a b
--

-- | rvs creates a list of standard normal random variates.
-- >>> t <- rvs gen n
-- >>> t `eq'` [-0.8077385934202513,-1.3423948150518445,-0.4900206084002882]
-- True
--
-- > rs <- rvs gen 10000
-- > L.fold (ma 1) rs
-- 1.986088417850516e-3
-- L.fold (std 1) rs
-- 0.9923523681261158
--
rvs :: Gen (PrimState IO) -> Int -> IO [Double]
rvs gen n = samples n standardNormal gen


-- | rvsPair generates a list of correlated random variate tuples
-- | 
-- >>> t <- rvsp gen 3 0.8
-- >>> t `eq'p` [(-0.8077385934202513,-1.4591410449385904),(-1.3423948150518445,-0.6046212701237168),(-0.4900206084002882,0.923007518547542)]
-- True
--
-- > L.fold ((,) <$> L.premap fst (ma 1) <*> L.premap snd (ma 1)) ps
-- (6.431292910481302e-3,8.551782977112597e-3)
-- > L.fold ((,) <$> L.premap fst (std 1) <*> L.premap snd (std 1)) ps
-- (1.0115105678335707,1.0026831116148713)
--
-- > L.fold (corr (ma 1) (std 1)) ps
-- 0.7990745094807482
--
rvsp :: Gen (PrimState IO) -> Int -> Double -> IO [(Double,Double)]
rvsp gen n c = do
  s0 <- rvs gen n
  s1 <- rvs gen n
  let s1' = zipWith (\x y -> c * x + sqrt (1 - c * c) * y) s0 s1
  pure $ zip s0 s1'

{-

writeHudOptionsChart "scratch.svg" defaultSvgOptions defaultHudOptions [] $ (:[]) $ Chart (LineA defaultLineStyle) ((\r -> SP r (L.fold (beta (ma 1)) $ (zip ((\r -> drop 1 $ zipWith (\o m -> o + 0.2 * (r ** -1) * m) rs (L.scan (ma (1-r)) rs)) r) rs))) <$> [0.001, 0.002, 0.005, 0.01,0.02,0.03,0.05,0.1,0.2,0.5])

writeHudOptionsChart "scratch.svg" defaultSvgOptions defaultHudOptions [] $ (:[]) $ Chart (LineA defaultLineStyle) ((\r -> SP r ((\x -> x - ((L.fold (beta (ma 1)) $ (zip ((\r -> drop 1 $ zipWith (\o m -> o + 0 * (r ** -1) * m) rs (L.scan (ma (1-r)) rs)) 0.001) rs)))) $ L.fold (beta (ma 1)) $ (zip ((\r -> drop 1 $ zipWith (\o m -> o + 0.01 * (r ** -1) * m) rs (L.scan (ma (1-r)) rs)) r) rs))) <$> [0.001, 0.002, 0.005, 0.01,0.02,0.03,0.05,0.1,0.2])

1.0237976903632238e-2
λ> (\r -> ((\x -> x - ((L.fold (beta (ma 1)) $ (zip ((\r -> drop 1 $ zipWith (\o m -> o + 0 * (r ** -1) * m) rs (L.scan (ma (1-r)) rs)) 0.001) rs)))) $ L.fold (beta (ma 1)) $ (zip ((\r -> drop 1 $ zipWith (\o m -> o + 0.01 * (r ** -1) * m) rs (L.scan (ma (1-r)) rs)) r) rs))) 0.05

let rsc = (\r -> ((\r -> drop 1 $ zipWith (\o m -> o + 0.1 * (r ** -1) * m) rs (L.scan (ma (1-r)) rs)) r)) 0.05

(L.fold (beta (ma 1)) $ drop 1 $ zip rsc (L.scan (ma 0.95) rsc))

-}



-- > rs <- rvs gen 10000
-- > let xma = zip rs (L.scan (ma 0.95) rs)


-- | with an online fold for the central tendency, return an (alpha, beta) regression values tuple.
reg :: (Fractional a) => L.Fold a a -> L.Fold (a,a) (a,a)
reg m = (,) <$> alpha m <*> beta m


-- y = b * x + a



{-
-- | traverse the same fold over a list
foldMapL :: L.Fold a b -> L.Fold [a] [b]
foldMapL (L.Fold step begin extract) = L.Fold fstep fbegin fextract
  where
    fbegin = Nothing
    fstep Nothing as = Just $ step begin <$> as
    fstep (Just xs) as = Just $ zipWith step xs as
    fextract Nothing = []
    fextract (Just xs) = extract <$> xs

-- | a constant L.Fold
fconst :: b -> L.Fold a b
fconst a = L.Fold (\ _ _ -> ()) () (const a)

-- | apply a list of Folds to a Foldable
foldList :: [L.Fold a b] -> L.Fold a [b]
foldList = foldr (\f -> (<*>) ((:) <$> f)) (fconst [])

-}


{-
-- | the classical abstract historio-dependent model of a stream
data DepMoments a = DepMoments
    { betas :: Stream (Of [a]) IO ()
    , xxs :: [L.Fold a a]
    , rv :: Stream (Of a) IO ()
    , fx :: [(Int, Int)]
    , fstd :: [(Int, Int)]
    } deriving (Generic)

-- defModel :: [Double] -> IO (DepMoments Double)
defModel :: Gen L.RealWorld -> [Double] -> DepMoments Double
defModel g bs = DepMoments (S.repeat bs) [mconst 1, ma 0.9, std 0.9, mconst 1, ma 0.9, std 0.9] (rvStd g) (P.zip [0..2] [0..2]) (P.zip [3..5] [3..5])

-- | create a stream from a DepMoments model
-- >>> toList_ $ depMo (DepMoments (S.repeat [0,0,0,1,0,0]) [mconst 1, ma 0.9, std 0.9, mconst 1, ma 0.9, std 0.9] (each [0..5]) (P.zip [0..2] [0..2]) (P.zip [3..5] [3..5]))
-- [0.0,1.0,2.0,3.0,4.0,5.0]
--
-- mean momentum
-- >>> toList_ $ depMo (DepMoments (S.repeat [0,1,0,1,0,0]) [mconst 1, ma 0.9, std 0.9, mconst 1, ma 0.9, std 0.9] (each [0..5]) (P.zip [0..2] [0..2]) (P.zip [3..5] [3..5]))
-- [0.0,1.0,2.3690036900369003,3.8432683919744113,5.369929916241361,6.931240249360273]
--
depMo ::
    DepMoments Double ->
    Stream (Of Double) IO ()
depMo d =
    rv d &
    echo &
    second (foldList (xxs d)) &
    S.zipWith (\bs (rv0,xs) ->
               L.fold L.sum (P.fmap (\(bi, xi) -> bs!!bi * xs!!xi) (fx d)) +
               rv0 * L.fold L.sum (P.fmap (\(bi, xi) -> bs!!bi * xs!!xi) (fstd d)))
    (betas d)

-}


