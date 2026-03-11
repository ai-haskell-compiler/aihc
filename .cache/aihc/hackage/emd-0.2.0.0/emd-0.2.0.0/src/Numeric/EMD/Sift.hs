{-# LANGUAGE BangPatterns                             #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE LambdaCase                               #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE RecordWildCards                          #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TypeApplications                         #-}
{-# LANGUAGE TypeInType                               #-}
{-# LANGUAGE TypeOperators                            #-}
{-# OPTIONS_GHC -Wno-orphans                          #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

-- |
-- Module      : Numeric.EMD.Sift
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Tools for creating your own custom sift stopping conditions.
--
-- @since 0.2.0.0
module Numeric.EMD.Sift (
    Sifter(..), SiftResult(..), SingleSift(..), SM
  -- * Sifters
  , defaultSifter
  , siftStdDev
  , siftTimes
  , siftEnergyDiff
  , siftSCond
  , siftAnd
  , siftOr
  -- ** Make Sifters
  , envMean
  , energyDiff
  , normalizeProj
  , siftCauchy
  , siftPairs
  , siftProj
  , siftPairs_
  , siftProj_
  -- * Internal
  , sift, envelopes, rms
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Conduino
import           Data.Conduino.Internal
import           Data.Default.Class
import           Data.Finite
import           Data.Sequence                (Seq(..))
import           GHC.TypeNats
import           Numeric.EMD.Internal
import           Numeric.EMD.Internal.Extrema
import           Numeric.EMD.Internal.Spline
import qualified Data.Conduino.Combinators    as C
import qualified Data.Map                     as M
import qualified Data.Vector.Generic          as VG
import qualified Data.Vector.Generic.Sized    as SVG

-- | @since 0.1.3.0
instance (VG.Vector v a, Fractional a, Ord a) => Default (Sifter v n a) where
    def = defaultSifter

-- | Default 'Sifter'
--
-- @
-- defaultSifter = 'siftStdDev' 0.3 `siftOr` 'siftTimes' 50
-- @
--
-- R package uses @'siftTimes' 20@, Matlab uses no limit
defaultSifter :: (VG.Vector v a, Fractional a, Ord a) => Sifter v n a
defaultSifter = siftStdDev 0.3 `siftOr` siftTimes 50

-- | Cheng, Yu, Yang suggest pairing together an energy difference
-- threshold with a threshold for mean envelope RMS.  This is a convenience
-- function to construct that pairing.
siftEnergyDiff
    :: (VG.Vector v a, KnownNat n, Floating a, Ord a)
    => a                -- ^ Threshold for Energy Difference
    -> a                -- ^ Threshold for mean envelope RMS
    -> Sifter v n a
siftEnergyDiff s t = siftProj energyDiff s
           `siftAnd` siftProj envMean t


-- | The result of a sifting operation.  Each sift either yields
-- a residual, or a new IMF.
data SiftResult v n a = SRResidual !(SVG.Vector v n a)
                      | SRIMF      !(SVG.Vector v n a) !Int   -- ^ number of sifting iterations

-- | Create a sifter that stops after a given fixed number of sifts.
--
-- Useful to use alongside 'siftOr' to set an "upper limit" on the number
-- of sifts.
siftTimes :: Int -> Sifter v n a
siftTimes n = Sifter $ C.drop (n - 1) >> void awaitSurely

-- | Create a sifter that stops when some projection on 'SingleSift' is
-- smaller than a given threshold.
siftProj
    :: Ord b
    => (SingleSift v n a -> SM v n a b)     -- ^ projection
    -> b                                    -- ^ threshold
    -> Sifter v n a
siftProj p t = siftProj_ $ fmap (<= t) . p

-- | Create a sifter that stops based on some predicate on the initial
-- vector and 'SingleSift' being 'True'.
siftProj_ :: (SingleSift v n a -> SM v n a Bool) -> Sifter v n a
siftProj_ p = Sifter go
  where
    go = do
      v <- awaitSurely
      r <- lift $ p v
      unless r go

-- | Create a sifter that stops when some projection on two consecutive
-- 'SingleSift's is smaller than a given threshold.
siftPairs
    :: Ord b
    => (SingleSift v n a -> SingleSift v n a -> SM v n a b)
    -> b
    -> Sifter v n a
siftPairs p t = siftPairs_ $ \x y -> (<= t) <$> p x y

-- | Create a sifter that stops based on some predicate on two consecutive
-- 'SingleSift's being 'True'.
siftPairs_
    :: (SingleSift v n a -> SingleSift v n a -> SM v n a Bool)
    -> Sifter v n a
siftPairs_ p = Sifter $ go =<< awaitSurely
  where
    go s = do
      s' <- awaitSurely
      r  <- lift $ p s s'
      unless r (go s')

-- | Sift based on the "standard deviation test", outlined in original
-- paper.
siftStdDev
    :: forall v n a. (VG.Vector v a, Fractional a, Ord a)
    => a                -- ^ minimal threshold
    -> Sifter v n a
siftStdDev = siftPairs $ \(SingleSift v _ _) (SingleSift v' _ _) -> pure $
    SVG.sum (SVG.zipWith (\x x' -> (x-x')^(2::Int) / (x^(2::Int) + eps)) v v')
  where
    eps = 0.0000001

-- | General class of "cauchy-like" sifters: Given a projection function
-- from a 'SingleSift', stop as soon as successive projections become
-- smaller than a given threshold, propertionally.
--
-- Given \(f(x_t)\), stop when:
--
-- \[
--   \frac{(f(x_t) - f(x_{t-1}))^2}{f^2(x_{t-1})} < \delta
-- \]
siftCauchy
    :: (Fractional b, Ord b)
    => (SingleSift v n a -> b)      -- ^ Projection function
    -> b                            -- ^ Threshold \(\delta\)
    -> Sifter v n a
siftCauchy p = siftPairs $ \s s' ->
  let ps  = p s
      ps' = p s'
      δ   = ps' - ps
  in  pure $ (δ * δ) / (ps * ps)

-- | Sift based on the "S-parameter" condition: Stop after a streak @n@ of
-- almost-same numbers of zero crossings and turning points.
siftSCond
    :: (VG.Vector v a, KnownNat n, Fractional a, Ord a)
    => Int                          -- ^ Streak @n@ to stop on
    -> Sifter v (n + 1) a
siftSCond n = Sifter $ C.map (crossCount . ssResult)
                    .| C.consecutive n
                    .| C.concatMap pick
                    .| C.dropWhile notGood
  where
    pick Empty      = Nothing
    pick (xs :|> x) = (xs, x) <$ guard (length xs == (n - 1))
    notGood (xs, x) = all ((<= 1) . abs . subtract x) xs
    crossCount xs = M.size mins + M.size maxs + crosses
      where
        (mins, maxs) = extrema xs
        crosses = fst . flip execState (0, Nothing) . flip SVG.mapM_ xs $ \x -> modify $ \(!i, !y) ->
          let xPos = x > 0
              i'   = case y of
                       Nothing -> i
                       Just y'
                         | xPos == y' -> i
                         | otherwise  -> i + 1
          in  (i', Just xPos)

-- | Combine two sifters in "or" fashion: The final sifter will complete
-- when /either/ sifter completes.
siftOr :: Sifter v n a -> Sifter v n a -> Sifter v n a
siftOr (Sifter p) (Sifter q) = Sifter $ altSink p q
infixr 2 `siftOr`

-- | Combine two sifters in "and" fashion: The final sifter will complete
-- when /both/ sifters complete.
siftAnd :: Sifter v n a -> Sifter v n a -> Sifter v n a
siftAnd (Sifter p) (Sifter q) = Sifter $ zipSink (id <$ p) q
infixr 3 `siftAnd`

-- | Project the root mean square of the mean of the maximum and minimum
-- envelopes.
envMean
    :: (VG.Vector v a, KnownNat n, Floating a)
    => SingleSift v n a
    -> SM v n a a
envMean SingleSift{..} = pure $
    rms $ SVG.zipWith (\x y -> (x + y) / 2) ssMinEnv ssMaxEnv

-- | Project the /square root/ of the "Energy difference".
energyDiff
    :: (VG.Vector v a, Floating a)
    => SingleSift v n a
    -> SM v n a a
energyDiff SingleSift{..} = do
    v0 <- ask
    pure . sqrt . abs . SVG.sum
         $ SVG.zipWith (\x c -> c * (x - c)) v0 ssResult

-- | Given a "projection function" (like 'envMean' or 'energyDiff'),
-- re-scale the result based on the RMS of the original signal.
normalizeProj
    :: (VG.Vector v a, KnownNat n, Floating a)
    => (SingleSift v n a -> SM v n a a)
    -> (SingleSift v n a -> SM v n a a)
normalizeProj f ss = do
    v0 <- asks rms
    r  <- f ss
    pure $ r / v0

-- | Get the root mean square of a vector
rms :: (VG.Vector v a, KnownNat n, Floating a) => SVG.Vector v n a -> a
rms xs = sqrt $ SVG.foldl' (\s x -> s + x*x) 0 xs / fromIntegral (SVG.length xs)


-- | Iterated sifting process, used to produce either an IMF or a residual.
sift
    :: forall v n a. (VG.Vector v a, KnownNat n, Floating a, Ord a)
    => EMDOpts v (n + 1) a
    -> SVG.Vector v (n + 1) a
    -> SiftResult v (n + 1) a
sift EO{..} v0 = case execStateT (runPipe sifterPipe) (0, v0) of
    Left  v        -> SRResidual v
    Right (!i, !v) -> SRIMF v i
  where
    sifterPipe = C.repeatM go
              .| hoistPipe
                    (pure . (`runReader` v0))
                    (sPipe eoSifter)
    go = StateT $ \(!i, !v) ->
      case sift' eoSplineEnd eoBoundaryHandler v of
        Nothing                -> Left v
        Just ss@SingleSift{..} -> Right (ss, (i + 1, ssResult))

-- | Single sift
sift'
    :: (VG.Vector v a, KnownNat n, Fractional a, Ord a)
    => SplineEnd a
    -> Maybe BoundaryHandler
    -> SVG.Vector v (n + 1) a
    -> Maybe (SingleSift v (n + 1) a)
sift' se bh v = do
    (mins, maxs) <- envelopes se bh v
    pure SingleSift
      { ssResult = SVG.zipWith3 (\x mi ma -> x - (mi + ma)/2) v mins maxs
      , ssMinEnv = mins
      , ssMaxEnv = maxs
      }

-- | Returns cubic splines of local minimums and maximums.  Returns
-- 'Nothing' if there are not enough local minimum or maximums to create
-- the splines.
envelopes
    :: (VG.Vector v a, KnownNat n, Fractional a, Ord a)
    => SplineEnd a
    -> Maybe BoundaryHandler
    -> SVG.Vector v (n + 1) a
    -> Maybe (SVG.Vector v (n + 1) a, SVG.Vector v (n + 1) a)
envelopes se bh xs = do
    when (bh == Just BHClamp) $ do
      guard (M.size mins > 1)
      guard (M.size maxs > 1)
    (,) <$> splineAgainst se emin mins
        <*> splineAgainst se emax maxs
  where
    -- minMax = M.fromList [(minBound, SVG.head xs), (maxBound, SVG.last xs)]
    (mins,maxs) = extrema xs
    (emin,emax) = case bh of
      Nothing  -> mempty
      Just bh' -> extendExtrema xs bh' (mins,maxs)
    --   | isJust bh = (mins `M.union` minMax, maxs `M.union` minMax)
    --   | otherwise = (mins, maxs)

extendExtrema
    :: forall v n a. (VG.Vector v a, KnownNat n)
    => SVG.Vector v (n + 1) a
    -> BoundaryHandler
    -> (M.Map (Finite (n + 1)) a, M.Map (Finite (n + 1)) a)
    -> (M.Map Int a, M.Map Int a)
    -- (M.Map (Finite (n + 1)) a, M.Map (Finite (n + 1)) a)
extendExtrema xs = \case
    BHClamp     -> const (firstLast, firstLast)
    BHSymmetric -> \(mins, maxs) ->
      let addFirst = case (flippedMin, flippedMax) of
              (Nothing      , Nothing      ) -> mempty
              -- first point is local maximum
              (Just (_,mn)  , Nothing      ) -> (mn        , firstPoint)
              -- first point is local minimum
              (Nothing      , Just (_,mx)  ) -> (firstPoint, mx        )
              (Just (mni,mn), Just (mxi,mx))
                | mni < mxi                  -> (mn        , firstPoint)
                | otherwise                  -> (firstPoint, mx        )
            where
              flippedMin = flip fmap (M.lookupMin mins) $ \(minIx, minVal) ->
                (minIx, M.singleton (negate (fromIntegral minIx)) minVal)
              flippedMax = flip fmap (M.lookupMin maxs) $ \(maxIx, maxVal) ->
                (maxIx, M.singleton (negate (fromIntegral maxIx)) maxVal)
          addLast = case (flippedMin, flippedMax) of
              (Nothing      , Nothing      ) -> mempty
              -- last point is local maximum
              (Just (_,mn)  , Nothing      ) -> (mn        , lastPoint )
              -- last point is local minimum
              (Nothing      , Just (_,mx)  ) -> (lastPoint , mx        )
              (Just (mni,mn), Just (mxi,mx))
                | mni > mxi                  -> (mn        , lastPoint )
                | otherwise                  -> (lastPoint , mx        )
            where
              flippedMin = flip fmap (M.lookupMax mins) $ \(minIx, minVal) ->
                (minIx, M.singleton (extendSym (fromIntegral minIx)) minVal)
              flippedMax = flip fmap (M.lookupMax maxs) $ \(maxIx, maxVal) ->
                (maxIx, M.singleton (extendSym (fromIntegral maxIx)) maxVal)
      in  addFirst `mappend` addLast
  where
    lastIx = fromIntegral $ maxBound @(Finite n)
    firstPoint = M.singleton 0 (SVG.head xs)
    lastPoint  = M.singleton lastIx (SVG.last xs)
    firstLast  = firstPoint `mappend` lastPoint
    extendSym i = 2 * lastIx - i

-- | Build a splined vector against a map of control points.
splineAgainst
    :: (VG.Vector v a, KnownNat n, Fractional a, Ord a)
    => SplineEnd a
    -> M.Map Int a              -- ^ extensions
    -> M.Map (Finite n) a
    -> Maybe (SVG.Vector v n a)
splineAgainst se ext = fmap go
                     . makeSpline se
                     . mappend (M.mapKeysMonotonic fromIntegral ext)
                     . M.mapKeysMonotonic fromIntegral
  where
    go spline = SVG.generate (sampleSpline spline . fromIntegral)
