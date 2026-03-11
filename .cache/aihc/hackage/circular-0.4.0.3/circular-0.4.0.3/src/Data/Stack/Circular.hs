{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Data.Stack.Circular
-- Description :  Circular stacks of fixed size
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jun 18 10:00:28 2020.
--
-- Construction of mutable circular stacks is done with 'replicate' and subsequent
-- 'push'es, or with 'fromVector'.
--
-- When denoting the asymptotic runtime of functions, @n@ refers to the circular
-- stack size.
module Data.Stack.Circular
  ( -- * Mutable circular stacks
    MStack,

    -- ** Construction and conversion
    replicate,
    fromVector,
    fromVectorWithIndex,
    toVector,
    take,

    -- ** Accessors
    size,
    get,
    pop,
    push,

    -- ** Monadic folds
    foldM,
    foldKM,

    -- * Immutable circular stacks
    Stack,
    thaw,
    freeze,
  )
where

import Control.Monad.Primitive
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VM
import Prelude hiding (foldl, product, replicate, sum, take)

-- | Mutable circular stacks with fixed size are just mutable vectors with a
-- pointer to the last element.
data MStack v s a = MStack
  { _mVector :: VG.Mutable v s a,
    _mIndex :: !Int
  }

-- | A circular stack of given size with the same element replicated.
--
-- Call 'error' if the maximum size is zero or negative.
--
-- O(n).
replicate :: (VG.Vector v a, PrimMonad m) => Int -> a -> m (MStack v (PrimState m) a)
replicate n x
  | n <= 0 = error "replicate: maximum size must be one or larger"
  | otherwise = do
      v <- VM.replicate n x
      return $ MStack v 0

-- | Convert a vector to a circular stack with size being equal to the length of
-- the vector. The first element of the vector is the oldest element of the
-- stack, the last element of the vector is the youngest element of the stack.
--
-- The vector must be non-empty.
--
-- O(n).
fromVector :: (VG.Vector v a, PrimMonad m) => v a -> m (MStack v (PrimState m) a)
fromVector v
  | n == 0 = error "fromVector: empty vector"
  | otherwise = do
      mv <- VG.thaw v
      return $ MStack mv (n - 1)
  where
    n = VG.length v

-- | Convert a vector to a circular stack with size being equal to the length of
-- the vector. The element of the vector at the given index is the youngest
-- element of the stack, the next element of the vector is the oldest element of
-- the stack.
--
-- The vector must be non-empty.
--
-- O(n).
fromVectorWithIndex :: (VG.Vector v a, PrimMonad m) => Int -> v a -> m (MStack v (PrimState m) a)
fromVectorWithIndex i v = do
  ms <- fromVector v
  return $ ms {_mIndex = i}

-- | Convert a circular stack to a vector. The first element of the returned
-- vector is the oldest element of the stack, the last element of the returned
-- vector is the youngest element of the stack.
--
-- O(n).
toVector :: (VG.Vector v a, PrimMonad m) => MStack v (PrimState m) a -> m (v a)
toVector (MStack v i) = do
  l <- VG.freeze $ VM.unsafeDrop i' v
  r <- VG.freeze $ VM.unsafeTake i' v
  return $ l VG.++ r
  where
    i' = i + 1

-- | Convert the last k elements of a circular stack to a vector. The first
-- element of the returned vector is the oldest element of the stack, the last
-- element of the returned vector is the youngest element of the stack.
--
-- The size of the stack must be larger than k.
--
-- O(k).
take :: (VG.Vector v a, PrimMonad m) => Int -> MStack v (PrimState m) a -> m (v a)
take k (MStack v i)
  | k < 0 = error "take: negative k"
  | k > n = error "take: circular stack too small"
  | k == 0 = return VG.empty
  -- We know now that k is in [1, n] and check if all k elements can be taken in
  -- one go.
  | i0 >= 0 = VG.freeze $ VM.unsafeSlice i0 k v
  -- Now we now that i0 is negative.
  | otherwise = do
      -- The length of r is i'.
      r <- VG.freeze $ VM.unsafeTake i' v
      -- The length of l has to be k-i'. So we have to drop n-(k-i')=n+i0.
      l <- VG.freeze $ VM.unsafeDrop (n + i0) v
      return $ l VG.++ r
  where
    n = VM.length v
    i' = i + 1
    -- The starting index. Can be negative.
    i0 = i' - k

-- | Size of the stack.
size :: VG.Vector v a => MStack v s a -> Int
size = VM.length . _mVector

-- | Get the last element without changing the stack.
--
-- O(1).
get :: (VG.Vector v a, PrimMonad m) => MStack v (PrimState m) a -> m a
get (MStack v i) = VM.unsafeRead v i
{-# INLINE get #-}

-- Select the previous older element without changing the stack.
previous :: VG.Vector v a => MStack v s a -> MStack v s a
previous (MStack v i) = MStack v i'
  where
    j = i - 1
    i' = if j < 0 then VM.length v - 1 else j

-- | Pop the youngest element from the stack and put the focus on the previous
-- element.
--
-- Be careful:
--
-- The stack is always full! Popping returns the last element and moves the
-- index to the second-last element, but the element is not truly removed from
-- the stack. It is only put to the end of the queue.
--
-- Hence, `pop` always succeeds, even if there are actually no more elements on
-- the stack (similar to walking backwards in a circle).
--
-- O(1).
pop :: (VG.Vector v a, PrimMonad m) => MStack v (PrimState m) a -> m (a, MStack v (PrimState m) a)
pop x = do
  val <- get x
  return (val, previous x)

-- Replace the youngest element.
put :: (VG.Vector v a, PrimMonad m) => a -> MStack v (PrimState m) a -> m (MStack v (PrimState m) a)
put x (MStack v i) = VM.unsafeWrite v i x >> return (MStack v i)

-- Select the next younger element without changing the stack.
next :: VG.Vector v a => MStack v s a -> MStack v s a
next (MStack v i) = MStack v i'
  where
    i' = (i + 1) `mod` VM.length v

-- | Push an element on the stack.
--
-- O(1).
push :: (VG.Vector v a, PrimMonad m) => a -> MStack v (PrimState m) a -> m (MStack v (PrimState m) a)
push x = put x . next

-- | Monadic fold from young to old over all elements of the stack.
--
-- Please also see the documentation of 'pop'.
--
-- O(n).
foldM :: (VG.Vector v b, PrimMonad m) => (a -> b -> a) -> a -> MStack v (PrimState m) b -> m a
foldM f x s = foldKM n f x s
  where
    n = VM.length $ _mVector s

-- Monadic fold over k elements in a vector.
foldKV ::
  (VM.MVector v b, PrimMonad m) =>
  -- Number of elements to take.
  Int ->
  -- Current index.
  Int ->
  (a -> b -> a) ->
  a ->
  v (PrimState m) b ->
  m a
foldKV 0 _ _ x _ = return x
foldKV k i f x v = do
  x' <- f x <$> VM.unsafeRead v i
  -- Assume that i-1 is non-negative.
  foldKV (k - 1) (i - 1) f x' v

-- | See 'foldM' but only over the @k@ youngest elements on the stack.
--
-- O(k).
foldKM :: (VG.Vector v b, PrimMonad m) => Int -> (a -> b -> a) -> a -> MStack v (PrimState m) b -> m a
foldKM k f x (MStack v i)
  | k < 0 = error "foldKM: k is negative."
  | k > n = error "foldKM: k is larger than the stack size."
  -- We can do the fold in one go.
  | k <= i' = foldKV k i f x v
  -- Or not.
  | otherwise = do
      x' <- foldKV i' i f x v
      -- Continue from the end of the vector.
      foldKV (k - i') (n - 1) f x' v
  where
    n = VM.length v
    i' = i + 1

-- | Immutable circular stack; useful, for example, to save, or restore a
-- mutable circular stack.
data Stack v a = Stack
  { _iStack :: v a,
    _iIndex :: !Int
  }
  deriving (Eq, Read, Show)

-- This (per se useless) top level splice separates the module into two
-- declaration groups. This is required, because the expression slices below
-- (which are now in the second declaration group) can only refer to definitions
-- outside their own declaration group. See
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/template_haskell.html.
$(return [])

instance (FromJSON (v a)) => FromJSON (Stack v a) where
  parseJSON = $(mkParseJSON defaultOptions ''Stack)

instance (ToJSON (v a)) => ToJSON (Stack v a) where
  toJSON = $(mkToJSON defaultOptions ''Stack)
  toEncoding = $(mkToEncoding defaultOptions ''Stack)

-- | Conversion from immutable to mutable circular stack.
--
-- O(n).
thaw :: (VG.Vector v a, PrimMonad m) => Stack v a -> m (MStack v (PrimState m) a)
thaw (Stack v i) = do
  mv <- VG.thaw v
  return $ MStack mv i

-- | Conversion from mutable to immutable circular stack.
--
-- O(n).
freeze :: (VG.Vector v a, PrimMonad m) => MStack v (PrimState m) a -> m (Stack v a)
freeze (MStack mv i) = do
  v <- VG.freeze mv
  return $ Stack v i
