-- |
-- Module:      Data.Vector.Rotcev
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- A wrapper for an arbitrary 'V.Vector' with O(1) 'reverse'.
-- Instead of creating a copy, it just flips a flag, which inverts indexing.
-- Imagine it as a vector with a switch between little-endianness and big-endianness.

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Vector.Rotcev
  ( Rotcev(..)
  , reverse
  , unRotcev
  , MRotcev(..)
  , mreverse
  ) where

import Prelude hiding (reverse)
import Data.Function
import Data.Vector.Fusion.Util (Box(..))
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV

-- | Wrapper for immutable vectors, equipped with a 'V.Vector' instance.
--
-- >>> Forward  (Data.Vector.fromList [0..100]) Data.Vector.Generic.! 10
-- 10
-- >>> Backward (Data.Vector.fromList [0..100]) Data.Vector.Generic.! 10
-- 90
data Rotcev v a
  = Forward  !(v a)
  -- ^ Behaves as an original vector in respect to 'V.Vector' operations.
  | Backward !(v a)
  -- ^ Behaves as a reversed vector in respect to 'V.Vector' operations.
  deriving (Eq, Ord, Show)

fromRotcev :: Rotcev v a -> v a
fromRotcev = \case
  Forward  v -> v
  Backward v -> v
{-# INLINE fromRotcev #-}

-- | Reverse an immutable vector in O(1) time and space.
--
-- >>> vec = Data.Vector.Generic.fromList [0..100] :: Rotcev Data.Vector.Vector Int
-- >>> reverse vec Data.Vector.Generic.! 10
-- 90
reverse :: Rotcev v a -> Rotcev v a
reverse = \case
  Forward  v -> Backward v
  Backward v -> Forward  v
{-# INLINE reverse #-}

-- | Unwrap 'Rotcev', extracting an underlying vector.
-- This takes O(1) for 'Forward', but full O(n) time for 'Backward' case,
-- so it would rather be avoided in intermediate computations.
-- Instead leverage opportunities, provided by generic 'V.Vector'
-- and 'MV.MVector' instances.
unRotcev :: V.Vector v a => Rotcev v a -> v a
unRotcev = \case
  Forward v  -> v
  Backward v -> V.reverse v
{-# INLINE unRotcev #-}

-- | Wrapper for mutable vectors, equipped with a 'MV.MVector' instance.
data MRotcev v s a
  = MForward  !(V.Mutable v s a)
  -- ^ Behaves as an original vector in respect to 'MV.MVector' operations.
  | MBackward !(V.Mutable v s a)
  -- ^ Behaves as a reversed vector in respect to 'MV.MVector' operations.

fromMRotcev :: MRotcev v s a -> V.Mutable v s a
fromMRotcev = \case
  MForward  v -> v
  MBackward v -> v
{-# INLINE fromMRotcev #-}

-- | Reverse a mutable vector in O(1) time and space.
mreverse :: MRotcev v s a -> MRotcev v s a
mreverse = \case
  MForward  v -> MBackward v
  MBackward v -> MForward  v
{-# INLINE mreverse #-}

type instance V.Mutable (Rotcev v) = MRotcev v

instance MV.MVector (V.Mutable v) a => MV.MVector (MRotcev v) a where
  basicLength = MV.basicLength . fromMRotcev
  basicUnsafeSlice off len = \case
    MForward  v -> MForward  $ MV.basicUnsafeSlice off len v
    MBackward v -> MBackward $ MV.basicUnsafeSlice (MV.basicLength v - off - len) len v
  basicOverlaps = MV.basicOverlaps `on` fromMRotcev
  basicUnsafeNew = fmap MForward . MV.basicUnsafeNew
  basicInitialize = MV.basicInitialize . fromMRotcev
  basicUnsafeReplicate = (fmap MForward .) . MV.basicUnsafeReplicate
  basicUnsafeRead = \case
    MForward  v -> MV.basicUnsafeRead v
    MBackward v -> MV.basicUnsafeRead v . ((MV.basicLength v - 1) -)
  basicUnsafeWrite = \case
    MForward  v -> MV.basicUnsafeWrite v
    MBackward v -> MV.basicUnsafeWrite v . ((MV.basicLength v - 1) -)
  basicClear = MV.basicClear . fromMRotcev
  basicSet = MV.basicSet . fromMRotcev

  basicUnsafeCopy !dst' !src' = case dst' of
    MForward{} -> case src' of
      MForward{}  -> MV.basicUnsafeCopy dst src
      MBackward{} -> do_copy 0
    MBackward{} -> case src' of
      MForward{}  -> do_copy 0
      MBackward{} -> MV.basicUnsafeCopy dst src
    where
      dst = fromMRotcev dst'
      src = fromMRotcev src'
      !n = MV.basicLength src
      do_copy i
        | i < n = do
          x <- MV.basicUnsafeRead src i
          MV.basicUnsafeWrite dst (n - 1 - i) x
          do_copy (i + 1)
        | otherwise = pure ()

  basicUnsafeMove !dst !src
    | MV.basicOverlaps dst src = do
      srcCopy' <- MV.basicUnsafeNew (MV.basicLength src)
      let srcCopy = case dst of
            MForward{}  -> MForward srcCopy'
            MBackward{} -> case src of
              MForward{}  -> MForward srcCopy'
              MBackward{} -> MBackward srcCopy'
      MV.basicUnsafeCopy srcCopy src
      MV.basicUnsafeCopy dst srcCopy
    | otherwise = MV.basicUnsafeCopy dst src

  basicUnsafeGrow (MForward v) by = do
    let n = MV.basicLength v
    v' <- MV.basicUnsafeNew (n + by)
    MV.basicUnsafeCopy (MV.basicUnsafeSlice 0 n v') v
    pure $ MForward v'
  basicUnsafeGrow (MBackward v) by = do
    let n = MV.basicLength v
    v' <- MV.basicUnsafeNew (n + by)
    MV.basicUnsafeCopy (MV.basicUnsafeSlice by n v') v
    pure $ MBackward v'

  {-# INLINE basicLength           #-}
  {-# INLINE basicUnsafeSlice      #-}
  {-# INLINE basicOverlaps         #-}
  {-# INLINE basicUnsafeNew        #-}
  {-# INLINE basicInitialize       #-}
  {-# INLINE basicUnsafeReplicate  #-}
  {-# INLINE basicUnsafeRead       #-}
  {-# INLINE basicUnsafeWrite      #-}
  {-# INLINE basicUnsafeCopy       #-}
  {-# INLINE basicUnsafeMove       #-}
  {-# INLINE basicUnsafeGrow       #-}

instance V.Vector v a => V.Vector (Rotcev v) a where
  basicUnsafeFreeze = \case
    MForward  v -> Forward  <$> V.basicUnsafeFreeze v
    MBackward v -> Backward <$> V.basicUnsafeFreeze v
  basicUnsafeThaw = \case
    Forward  v -> MForward  <$> V.basicUnsafeThaw v
    Backward v -> MBackward <$> V.basicUnsafeThaw v
  basicLength = V.basicLength . fromRotcev
  basicUnsafeSlice off len = \case
    Forward  v -> Forward  $ V.basicUnsafeSlice off len v
    Backward v -> Backward $ V.basicUnsafeSlice (V.basicLength v - off - len) len v
  basicUnsafeIndexM = \case
    Forward  v -> V.basicUnsafeIndexM v
    Backward v -> V.basicUnsafeIndexM v . ((V.basicLength v - 1) -)

  basicUnsafeCopy !dst' !src' = case dst' of
    MForward{} -> case src' of
      Forward{}  -> V.basicUnsafeCopy dst src
      Backward{} -> do_copy 0
    MBackward{} -> case src' of
      Forward{}  -> do_copy 0
      Backward{} -> V.basicUnsafeCopy dst src
    where
      dst = fromMRotcev dst'
      src = fromRotcev  src'
      !n = V.basicLength src

      do_copy i
        | i < n = case V.basicUnsafeIndexM src i of
          Box x -> do
            MV.basicUnsafeWrite dst (n - 1 - i) x
            do_copy (i + 1)
        | otherwise = pure ()

  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw   #-}
  {-# INLINE basicLength       #-}
  {-# INLINE basicUnsafeSlice  #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE basicUnsafeCopy   #-}
