{-# LANGUAGE CPP         #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe        #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-- | Indexed Traversables
module Data.Traversable.WithIndex (
    -- * Indexed Traversables
    TraversableWithIndex(..),
    -- * Indexed Traversable Combinators
    ifor,
    imapM,
    iforM,
    imapAccumR,
    imapAccumL,
    -- * Default implementations
    imapDefault,
    ifoldMapDefault,
) where

import Prelude (Monad (..), flip)

import Control.Applicative           (Applicative (..), WrappedMonad (..))
import Control.Applicative.Backwards (Backwards (..))
import Data.Tuple                    (swap)

import qualified Control.Monad.Trans.State.Lazy as Lazy

import WithIndex
import CoerceCompat

-- | Traverse with an index (and the arguments flipped).
--
-- @
-- 'for' a ≡ 'ifor' a '.' 'const'
-- 'ifor' ≡ 'flip' 'itraverse'
-- @
ifor :: (TraversableWithIndex i t, Applicative f) => t a -> (i -> a -> f b) -> f (t b)
ifor = flip itraverse
{-# INLINE ifor #-}

-- | Map each element of a structure to a monadic action,
-- evaluate these actions from left to right, and collect the results, with access
-- the index.
--
-- When you don't need access to the index 'mapM' is more liberal in what it can accept.
--
-- @
-- 'mapM' ≡ 'imapM' '.' 'const'
-- @
imapM :: (TraversableWithIndex i t, Monad m) => (i -> a -> m b) -> t a -> m (t b)
imapM f = unwrapMonad #. itraverse (WrapMonad #.. f)
{-# INLINE imapM #-}

-- | Map each element of a structure to a monadic action,
-- evaluate these actions from left to right, and collect the results, with access
-- its position (and the arguments flipped).
--
-- @
-- 'forM' a ≡ 'iforM' a '.' 'const'
-- 'iforM' ≡ 'flip' 'imapM'
-- @
iforM :: (TraversableWithIndex i t, Monad m) => t a -> (i -> a -> m b) -> m (t b)
iforM = flip imapM
{-# INLINE iforM #-}

-- | Generalizes 'Data.Traversable.mapAccumR' to add access to the index.
--
-- 'imapAccumR' accumulates state from right to left.
--
-- @
-- 'Data.Traversable.mapAccumR' ≡ 'imapAccumR' '.' 'const'
-- @
imapAccumR :: TraversableWithIndex i t => (i -> s -> a -> (s, b)) -> s -> t a -> (s, t b)
imapAccumR f s0 a = swap (Lazy.runState (forwards (itraverse (\i c -> Backwards (Lazy.state (\s -> swap (f i s c)))) a)) s0)
{-# INLINE imapAccumR #-}

-- | Generalizes 'Data.Traversable.mapAccumL' to add access to the index.
--
-- 'imapAccumL' accumulates state from left to right.
--
-- @
-- 'Data.Traversable.mapAccumL' ≡ 'imapAccumL' '.' 'const'
-- @
imapAccumL :: TraversableWithIndex i t => (i -> s -> a -> (s, b)) -> s -> t a -> (s, t b)
imapAccumL f s0 a = swap (Lazy.runState (itraverse (\i c -> Lazy.state (\s -> swap (f i s c))) a) s0)
{-# INLINE imapAccumL #-}
