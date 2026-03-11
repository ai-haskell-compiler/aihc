-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.PQueue.Prio.Min.Mutable
  ( -- * Documentation
    MMinPQueue,

    -- * Construction
    empty,
    singleton,
    insert,

    -- * Query
    deleteMin,

    -- * List conversion
    fromAscList,
  )
where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Vector qualified as Vector
import Data.Vector.Mutable (MVector)
import Data.Vector.Mutable qualified as MVector

data MMinPQueue s k a
  = MMinPQueue
  { vectorRef :: STRef s (MVector s (k, a)),
    sizeRef :: STRef s Int
  }

empty :: ST s (MMinPQueue s k a)
empty = do
  vec <- MVector.unsafeNew 1
  vecRef <- newSTRef vec
  sizeRef <- newSTRef 0
  pure $
    MMinPQueue
      { vectorRef = vecRef,
        sizeRef = sizeRef
      }

singleton :: k -> a -> ST s (MMinPQueue s k a)
singleton k v = do
  vec <- MVector.replicate 1 (k, v)
  vecRef <- newSTRef vec
  sizeRef <- newSTRef 1
  pure $
    MMinPQueue
      { vectorRef = vecRef,
        sizeRef = sizeRef
      }

resizeIfNeeded :: MMinPQueue s k a -> ST s ()
resizeIfNeeded pqueue = do
  vec <- readSTRef pqueue.vectorRef
  size <- readSTRef pqueue.sizeRef
  when (MVector.length vec == size) $ do
    vec' <- MVector.unsafeGrow vec size
    writeSTRef pqueue.vectorRef vec'

insert :: (Ord k) => k -> a -> MMinPQueue s k a -> ST s ()
insert k v pqueue = do
  resizeIfNeeded pqueue

  n <- readSTRef pqueue.sizeRef
  vec <- readSTRef pqueue.vectorRef
  MVector.unsafeWrite vec n (k, v)
  writeSTRef pqueue.sizeRef (n + 1)

  bubbleUp vec n

bubbleUp :: (Ord k) => MVector s (k, a) -> Int -> ST s ()
bubbleUp vector = go
  where
    go i =
      if i == 0
        then pure ()
        else do
          let parentIndex = (i - 1) `quot` 2
          parentVal <- MVector.unsafeRead vector parentIndex
          currentVal <- MVector.unsafeRead vector i
          when (fst parentVal > fst currentVal) $ do
            MVector.unsafeSwap vector parentIndex i
            go parentIndex

deleteMin :: (Ord k) => MMinPQueue s k a -> ST s (Maybe (k, a))
deleteMin pqueue = do
  n <- readSTRef pqueue.sizeRef
  if n == 0
    then pure Nothing
    else do
      vector <- readSTRef pqueue.vectorRef
      minNode <- MVector.unsafeRead vector 0
      MVector.unsafeSwap vector 0 (n - 1)
      writeSTRef pqueue.sizeRef (n - 1)
      bubbleDown vector 0 (n - 1)
      pure (Just minNode)

bubbleDown :: (Ord k) => MVector s (k, a) -> Int -> Int -> ST s ()
bubbleDown vector i n = go i
  where
    go j = do
      case compare n (2 * j + 2) of
        LT -> pure ()
        EQ -> do
          x <- MVector.unsafeRead vector j
          x' <- MVector.unsafeRead vector (2 * j + 1)
          let j' = 2 * j + 1
          when (fst x > fst x') $ do
            MVector.unsafeWrite vector j' x
            MVector.unsafeWrite vector j x'
            go j'
        GT -> do
          x <- MVector.unsafeRead vector j
          xleft <- MVector.unsafeRead vector (2 * j + 1)
          xright <- MVector.unsafeRead vector (2 * j + 2)
          let (x', j') =
                if fst xleft <= fst xright
                  then (xleft, 2 * j + 1)
                  else (xright, 2 * j + 2)
          when (fst x > fst x') $ do
            MVector.unsafeWrite vector j' x
            MVector.unsafeWrite vector j x'
            go j'

fromAscList :: forall s k a. [(k, a)] -> ST s (MMinPQueue s k a)
fromAscList xs = do
  vector <- Vector.thaw (Vector.fromList xs)
  vectorRef <- newSTRef vector
  sizeRef <- newSTRef (length xs)
  pure $
    MMinPQueue
      { vectorRef = vectorRef,
        sizeRef = sizeRef
      }
