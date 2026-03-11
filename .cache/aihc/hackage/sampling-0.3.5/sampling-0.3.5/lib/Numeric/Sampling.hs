{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Numeric.Sampling (
    -- * Without replacement
    sample
  , sampleIO

    -- * With replacement
  , resample
  , resampleIO

    -- * Unequal probability, without replacement
  , psample
  , psampleIO

    -- * Unequal probability, with replacement
  , presample
  , presampleIO

    -- * Re-exported
  , module System.Random.MWC
  ) where

import qualified Control.Foldl as F
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Foldable as Foldable
#if __GLASGOW_HASKELL__ < 710
import Data.Foldable (Foldable)
#endif
import Data.Function (on)
import Data.List (sortBy)
#if __GLASGOW_HASKELL__ < 800
import Data.Monoid
#endif
import qualified Data.Sequence as S
import qualified Data.Vector as V (toList)
import Numeric.Sampling.Internal
import System.Random.MWC

-- | (/O(n)/) Sample uniformly, without replacement.
--
--   Returns Nothing if the desired sample size is larger than the collection
--   being sampled from.
sample
  :: (PrimMonad m, Foldable f)
  => Int -> f a -> Gen (PrimState m) -> m (Maybe [a])
sample n xs gen
  | n < 0     = return Nothing
  | otherwise = do
      collected <- F.foldM (randomN n gen) xs
      return $ fmap V.toList collected
{-# INLINABLE sample #-}

-- | (/O(n)/) 'sample' specialized to IO.
sampleIO :: Foldable f => Int -> f a -> IO (Maybe [a])
sampleIO n xs = withSystemRandom . asGenIO $ sample n xs
{-# INLINABLE sampleIO #-}

-- | (/O(n log n)/) Sample uniformly with replacement (bootstrap).
resample
  :: (PrimMonad m, Foldable f)
  => Int -> f a -> Gen (PrimState m) -> m [a]
resample n xs = presample n weighted where
  weight   = recip (F.fold F.genericLength xs)
  weighted = zip (repeat weight) (Foldable.toList xs)
{-# INLINABLE resample #-}

-- | (/O(n log n)/) 'resample' specialized to IO.
resampleIO :: Foldable f => Int -> f a -> IO [a]
resampleIO n xs = withSystemRandom . asGenIO $ resample n xs
{-# INLINABLE resampleIO #-}

-- | (/O(n log n)/) Unequal probability sampling.
--
--   Returns Nothing if the desired sample size is larger than the collection
--   being sampled from.
psample
  :: (PrimMonad m, Foldable f)
  => Int -> f (Double, a) -> Gen (PrimState m) -> m (Maybe [a])
psample n weighted gen = do
    let sorted = sortProbs weighted
    computeSample n sorted gen
  where
    computeSample
      :: PrimMonad m
      => Int -> [(Double, a)] -> Gen (PrimState m) -> m (Maybe [a])
    computeSample size xs g = go 1 [] size (S.fromList xs) where
      go !mass !acc j vs
        | j <  0    = return Nothing
        | j <= 0    = return (Just acc)
        | otherwise = do
            z <- fmap (* mass) (uniform g)

            let cumulative = S.drop 1 $ S.scanl (\s (pr, _) -> s + pr) 0 vs
                midx       = S.findIndexL (>= z) cumulative

                idx = case midx of
                  Nothing -> error "psample: no index found"
                  Just x  -> x

                (p, val) = S.index vs idx
                (l, r)   = S.splitAt idx vs
                deleted = l <> S.drop 1 r

            go (mass - p) (val:acc) (pred j) deleted
{-# INLINABLE psample #-}

-- | (/O(n log n)/) 'psample' specialized to IO.
psampleIO :: Foldable f => Int -> f (Double, a) -> IO (Maybe [a])
psampleIO n weighted = withSystemRandom . asGenIO $ psample n weighted
{-# INLINABLE psampleIO #-}

-- | (/O(n log n)/) Unequal probability resampling.
presample
  :: (PrimMonad m, Foldable f)
  => Int -> f (Double, a) -> Gen (PrimState m) -> m [a]
presample n weighted gen
    | n <= 0    = return []
    | otherwise = do
        let (bprobs, vals) = unzip $ sortProbs weighted
            probs          = drop 1 (F.scan F.sum bprobs)
            cumulative     = zip probs vals
        computeSample n cumulative gen
  where
    computeSample
      :: PrimMonad m => Int -> [(Double, a)] -> Gen (PrimState m) -> m [a]
    computeSample size xs g = go [] size where
      go !acc s
        | s <= 0    = return acc
        | otherwise = do
            z <- uniform g
            case F.fold (F.find ((>= z) . fst)) xs of
              Just (_, val) -> go (val:acc) (pred s)
              Nothing       -> return acc
{-# INLINABLE presample #-}

-- | (/O(n log n)/) 'presample' specialized to IO.
presampleIO :: (Foldable f) => Int -> f (Double, a) -> IO [a]
presampleIO n weighted = withSystemRandom . asGenIO $ presample n weighted
{-# INLINABLE presampleIO #-}

sortProbs :: (Foldable f, Ord a) => f (a, b) -> [(a, b)]
sortProbs = sortBy (flip compare `on` fst) . Foldable.toList
{-# INLINABLE sortProbs #-}

