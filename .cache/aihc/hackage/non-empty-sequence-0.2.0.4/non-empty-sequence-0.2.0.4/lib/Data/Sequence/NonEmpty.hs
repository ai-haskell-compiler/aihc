{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}

-- | A 'Seq' that must contain at least one item.
module Data.Sequence.NonEmpty where

import Control.Monad (join, ap)
import Data.Data (Data)
import Data.Semigroup (Semigroup((<>)))
import Data.Sequence (Seq, (<|), ViewL(EmptyL, (:<)), viewl)
import Data.Typeable (Typeable)
import qualified Data.Sequence as Seq

-- | Conceptually this is a 'Seq' that always contains at least one item.
data NonEmptySeq a = NonEmptySeq
  { _fore :: a
  , _aft :: Seq a
  } deriving (Eq, Ord, Show, Data, Typeable, Functor, Foldable, Traversable)

-- | van Laarhoven lens for the first element
fore :: Functor f => (a -> f a) -> NonEmptySeq a -> f (NonEmptySeq a)
fore inj (NonEmptySeq a1 as) = flip NonEmptySeq as <$> inj a1

-- | van Laarhoven lens for the remaining elements
aft :: Functor f => (Seq a -> f (Seq a)) -> NonEmptySeq a -> f (NonEmptySeq a)
aft inj (NonEmptySeq a1 as) = NonEmptySeq a1 <$> inj as

instance Semigroup (NonEmptySeq a) where
  (NonEmptySeq a1 as) <> (NonEmptySeq b1 bs)
    = NonEmptySeq a1 (as <> (b1 <| bs))

instance Monad NonEmptySeq where
  return a = NonEmptySeq a Seq.empty
  NonEmptySeq a as >>= f = NonEmptySeq (_fore r1) rs
    where
      r1 = f a
      rs = _aft r1 `mappend` rss
      rss = join . fmap nonEmptySeqToSeq . fmap f $ as

instance Applicative NonEmptySeq where
  pure = return
  (<*>) = ap

-- | Flattens a 'NonEmptySeq' to a 'Seq'.
nonEmptySeqToSeq :: NonEmptySeq a -> Seq a
nonEmptySeqToSeq (NonEmptySeq a1 as) = a1 <| as

-- | If the 'Seq' has at least one item, create a 'NonEmptySeq'.
seqToNonEmptySeq :: Seq a -> Maybe (NonEmptySeq a)
seqToNonEmptySeq sq = case viewl sq of
  EmptyL -> Nothing
  x :< xs -> Just (NonEmptySeq x xs)

-- | Prepends a 'Seq' to a 'NonEmptySeq'.
prependSeq :: Seq a -> NonEmptySeq a -> NonEmptySeq a
prependSeq sq (NonEmptySeq a as) = case viewl sq of
  EmptyL -> NonEmptySeq a as
  l :< ls -> NonEmptySeq l (ls `mappend` (a <| as))

-- | Appends a 'Seq' to a 'NonEmptySeq'.
appendSeq :: NonEmptySeq a -> Seq a -> NonEmptySeq a
appendSeq (NonEmptySeq a as) sq = NonEmptySeq a (as `mappend` sq)

-- | Place a single item at the head of the 'NonEmptySeq'.
singleton :: a -> NonEmptySeq a
singleton a = NonEmptySeq a Seq.empty
