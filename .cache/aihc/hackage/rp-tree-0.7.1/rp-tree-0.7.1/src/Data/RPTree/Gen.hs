{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# language LambdaCase #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# options_ghc -Wno-unused-imports #-}
module Data.RPTree.Gen where

import Control.Monad (replicateM, foldM)

-- containers
import qualified Data.IntMap as IM (IntMap, insert, toList)
-- splitmix-distribitions
import System.Random.SplitMix.Distributions (Gen, GenT, uniformR, stdUniform, bernoulli, exponential, normal, discrete, categorical)
-- transformers
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT(..), get, put, runStateT, evalStateT, State, runState, evalState)

import qualified Data.Vector.Generic as VG (Vector(..), unfoldrM, length, replicateM, (!))
import qualified Data.Vector.Unboxed as VU (Vector, Unbox, fromList)

import Data.RPTree.Internal (RPTree(..), RPT(..), SVector(..), fromListSv, DVector(..), fromListDv)


-- | Sample without replacement with a single pass over the data
--
-- implements Algorithm L for reservoir sampling
--
-- Li, Kim-Hung (4 December 1994). "Reservoir-Sampling Algorithms of Time Complexity O(n(1+log(N/n)))". ACM Transactions on Mathematical Software. 20 (4): 481–493. doi:10.1145/198429.198435
sampleWOR :: (Monad m, Foldable t) =>
             Int -- ^ sample size
          -> t a
          -> GenT m [a]
sampleWOR k xs = do
  (_, res) <- flip runStateT z $ foldM insf 0 xs
  pure $ map snd $ IM.toList (rsReservoir res)
  where
    z = RSPartial mempty
    insf i x = do
      st <- get
      case st of
        RSPartial acc -> do
          w <- lift $ genW k
          s <- lift $ genS w
          let
            acc' = IM.insert i x acc
            ila = i + s + 1
            st'
              | i >= k = RSFull acc' ila w
              | otherwise = RSPartial acc'
          put st'
          pure (succ i)
        RSFull acc ila0 w0 -> do
          case i `compare` ila0 of
            EQ -> do
              w <- lift $ genW k
              s <- lift $ genS w0
              let
                ila = i + s + 1
              acc' <- lift $ replaceInBuffer k acc x
              let
                w' = w0 * w
              put (RSFull acc' ila w')
              pure (succ i)
            _ -> pure (succ i)

data ResS a = RSPartial { rsReservoir :: IM.IntMap a }
            | RSFull {
                rsReservoir :: IM.IntMap a -- ^ reservoir
                , rsfLookAh :: !Int -- ^ lookahead index
                , rsfW :: !Double -- ^ W
                } deriving (Eq, Show)

genW :: (Monad m) => Int -> GenT m Double
genW k = do
  u <- stdUniform
  pure $ exp (log u / fromIntegral k)

genS :: (Monad m) => Double -> GenT m Int
genS w = do
  u <- stdUniform
  pure $ floor (log u / log (1 - w))

-- | Replaces a value at a random position within the buffer
replaceInBuffer :: (Monad m) =>
                   Int
                -> IM.IntMap a
                -> a
                -> GenT m (IM.IntMap a)
replaceInBuffer k imm y = do
  u <- stdUniform
  let ix = floor (fromIntegral k * u)
  pure $ IM.insert ix y imm







-- mixtures

mixtureN :: Monad m => [(Double, GenT m b)] -> GenT m b
mixtureN pgs = go
  where
    (ps, gs) = unzip pgs
    go = do
      miix <- categorical ps
      case miix of
        Nothing -> gs !! 0
        Just i  -> do
          let p = gs !! i
          p


circle2d :: (Monad m) => Double -> GenT m (DVector Double)
circle2d r = go
  where
    go = do
      x <- uniformR (- r) r
      y <- uniformR (- r) r
      if x**2 + y**2 <= r
        then pure $ fromListDv [x, y]
        else go

normalSparse2 :: Monad m => Double -> Int -> GenT m (SVector Double)
normalSparse2 pnz d = do
  b <- bernoulli 0.5
  if b
    then sparse pnz d (normal 0 0.5)
    else sparse pnz d (normal 2 0.5)

normalDense2 :: Monad m => Int -> GenT m (DVector Double)
normalDense2 d = do
  b <- bernoulli 0.5
  if b
    then dense d (normal 0 0.5)
    else dense d (normal 2 0.5)

normal2 :: (Monad m) => GenT m (DVector Double)
normal2 = do
  b <- bernoulli 0.5
  if b
    then dense 2 $ normal 0 0.5
    else dense 2 $ normal 2 0.5


-- | Generate a sparse random vector with a given nonzero density and components sampled from the supplied random generator
sparse :: (Monad m, VU.Unbox a) =>
          Double -- ^ nonzero density
       -> Int -- ^ vector dimension
       -> GenT m a -- ^ random generator of vector components
       -> GenT m (SVector a)
sparse p sz rand = SV sz <$> sparseVG p sz rand

-- | Generate a dense random vector with components sampled from the supplied random generator
dense :: (Monad m, VG.Vector VU.Vector a) =>
         Int -- ^ vector dimension
      -> GenT m a -- ^ random generator of vector components
      -> GenT m (DVector a)
dense sz rand = DV <$> denseVG sz rand



-- | Sample a dense random vector
denseVG :: (VG.Vector v a, Monad m) =>
           Int -- ^ vector dimension
        -> m a
        -> m (v a)
denseVG sz rand = VG.unfoldrM mkf 0
  where
    mkf i
      | i >= sz = pure Nothing
      | otherwise = do
          x <- rand
          pure $ Just (x, succ i)

-- | Sample a sparse random vector
sparseVG :: (Monad m, VG.Vector v (Int, a)) =>
            Double -- ^ nonzero density
         -> Int  -- ^ vector dimension
         -> GenT m a
         -> GenT m (v (Int, a))
sparseVG p sz rand = VG.unfoldrM mkf 0
  where
    mkf i
      | i >= sz = pure Nothing
      | otherwise = do
          flag <- bernoulli p
          if flag
            then
            do
              x <- rand
              pure $ Just ((i, x), succ i)
            else
              mkf (succ i)
