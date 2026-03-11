{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- |
-- Module    : Data.Queue.Bounded
-- Copyright : (c) Kadena LLC, 2019
-- License   : BSD3
-- Maintainer: Colin Woodbury <colin@kadena.io>
--
-- This library provides a strict, immutable, thread-safe, single-ended, bounded
-- queue. When the insert limit is reached and a `cons` is attempted, this
-- `BQueue` automatically drops old entries off its end. Thus, writes always
-- succeed and never block.
--
-- This data structure is intended as a "sliding window" over some stream of
-- data, where we wish old entries to be naturally forgotten. Since this is an
-- immutable data structure and not a concurrent queue, we provide instances for
-- the usual useful typeclasses with which one can perform analysis over the
-- entire "window".
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified Data.Queue.Bounded as BQ
-- @

module Data.Queue.Bounded
  ( -- * Type
    BQueue()
    -- * Construction
  , empty, singleton, fromList
    -- * Insertion / Removal
  , cons, uncons
    -- * Extra
  , average
  , reverse
  , take, drop
  ) where

import           Control.DeepSeq (NFData)
import           Data.Foldable (foldl', toList)
import           Data.Ratio ((%))
import           Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)
import           Prelude hiding (drop, reverse, take)
import qualified Prelude as P

---

-- | A single-ended, bounded queue which keeps track of its size.
data BQueue a = BQueue
  { _bqs      :: !(Seq a)
  , _bqsLimit :: {-# UNPACK #-} !Int
  , _bqsSize  :: {-# UNPACK #-} !Int }
  deriving (Eq, Generic, NFData)

instance Show a => Show (BQueue a) where
  show (BQueue q _ _) = show $ toList q

instance Functor BQueue where
  fmap f (BQueue q l s) = BQueue (f <$> q) l s
  {-# INLINE fmap #-}

-- | \(\mathcal{O}(1)\) `length` implementation.
instance Foldable BQueue where
  foldMap f (BQueue q _ _) = foldMap f q
  {-# INLINE foldMap #-}

  length (BQueue _ _ s) = s

instance Traversable BQueue where
  traverse f (BQueue q l s) = (\q' -> BQueue q' l s) <$> traverse f q
  {-# INLINE traverse #-}

instance Semigroup (BQueue a) where
  (BQueue q l s) <> (BQueue q' l' s') = BQueue (q <> q') (l + l') (s + s')
  {-# INLINE (<>) #-}

-- | Given a limit value, yield an empty `BQueue`.
empty :: Int -> BQueue a
empty l = BQueue mempty l 0

-- | Given a limit value and an initial value, yield a singleton `BQueue`.
singleton :: Int -> a -> BQueue a
singleton l a = BQueue (Seq.singleton a) l 1

-- | \(\mathcal{O}(c)\). Naively keeps the first \(c\) values of the input list
-- (as defined by the given limiting `Int` value) and does not attempt any
-- elegant queue-like cycling.
fromList :: Int -> [a] -> BQueue a
fromList n list = BQueue (Seq.fromList list') n $ length list'
  where
    list' = P.take n list

-- | \(\mathcal{O}(1)\).
cons :: a -> BQueue a -> BQueue a
cons a (BQueue Empty l _) = BQueue (Seq.singleton a) l 1
cons a (BQueue q@(rest :|> _) l s)
  | s == l = BQueue (a <| rest) l s
  | otherwise = BQueue (a <| q) l (succ s)

-- | \(\mathcal{O}(1)\).
uncons :: BQueue a -> Maybe (a, BQueue a)
uncons (BQueue Empty _ _)     = Nothing
uncons (BQueue (h :<| t) l s) = Just (h, BQueue t l $ pred s)

-- | \(\mathcal{O}(n)\).
average :: Integral a => BQueue a -> a
average (BQueue q _ s) = floor $ foldl' (+) 0 q % fromIntegral s
{-# INLINE average #-}

-- | \(\mathcal{O}(n)\).
reverse :: BQueue a -> BQueue a
reverse (BQueue q l s) = BQueue (Seq.reverse q) l s

-- | \(\mathcal{O}(\log(\min(i,n-i)))\).
take :: Int -> BQueue a -> BQueue a
take n (BQueue q l s) = BQueue (Seq.take n q) l $ min n s

-- | \(\mathcal{O}(\log(\min(i,n-i)))\).
drop :: Int -> BQueue a -> BQueue a
drop n (BQueue q l s) = BQueue (Seq.drop n q) l $ max 0 (s - n)
