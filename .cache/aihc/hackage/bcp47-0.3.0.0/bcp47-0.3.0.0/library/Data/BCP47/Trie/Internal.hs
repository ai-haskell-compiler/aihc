{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.BCP47.Trie.Internal
  ( Trie (..)
  , fromList
  , fromNonEmpty
  , singleton
  , union
  , unionWith
  , unionUsing
  , mapMaybe
  , Trie2 (..)
  , Subtags (..)
  , singleton2
  , lookup2
  , match2
  , union2Using
  , fromSubtags
  , mapMaybe2
  )
where

#if MIN_VERSION_base(4,18,0)
import Control.Applicative ((<|>))
#else
import Control.Applicative (liftA2, (<|>))
#endif
import Data.BCP47
import Data.BCP47.Internal.Subtags
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Last (Last, getLast))
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Modifiers (NonEmptyList (getNonEmpty))

-- | A trie mapping 'BCP47' tags to values
newtype Trie a = Trie {unLanguage :: Map ISO639_1 (Trie2 a)}
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Semigroup a => Semigroup (Trie a) where
  x <> y = unionUsing (liftA2 (<>)) x y

instance Arbitrary a => Arbitrary (Trie a) where
  arbitrary = fromNonEmpty . NE.fromList . getNonEmpty <$> arbitrary

-- | Construct a 'Trie' from a list of tag/value pairs.
fromList :: [(BCP47, a)] -> Maybe (Trie a)
fromList = fmap fromNonEmpty . NE.nonEmpty

-- | Construct a 'Trie' from a non empty list of tag/value pairs.
fromNonEmpty :: NonEmpty (BCP47, a) -> Trie a
fromNonEmpty = foldr (union . uncurry singleton) (Trie mempty)

-- | Construct a 'Trie' from a single tag/value pair.
singleton :: BCP47 -> a -> Trie a
singleton tag = Trie . Map.singleton (language tag) . singleton2 tag

-- | A left-biased union of two 'Trie' structures. The left value is prefered
-- when duplicate tags are found.
union :: Trie a -> Trie a -> Trie a
union = unionUsing (<|>)

-- | 'union' with a combining function.
unionWith :: (a -> a -> a) -> Trie a -> Trie a -> Trie a
unionWith f = unionUsing (liftA2 f)

unionUsing :: (Maybe a -> Maybe a -> Maybe a) -> Trie a -> Trie a -> Trie a
unionUsing f (Trie x) (Trie y) = Trie $ Map.unionWith (union2Using f) x y

nullToMaybe :: Map k a -> Maybe (Map k a)
nullToMaybe m = if Map.null m then Nothing else Just m

-- Like `Map.mapMaybe` but returns a `Maybe` because `Trie` should be non-empty
mapMaybe :: (a -> Maybe b) -> Trie a -> Maybe (Trie b)
mapMaybe f (Trie x) = Trie <$> nullToMaybe (Map.mapMaybe (mapMaybe2 f) x)

data Trie2 a = Trie2 (Maybe a) (Map Subtags (Trie2 a))
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Semigroup a => Semigroup (Trie2 a) where
  x <> y = union2Using (liftA2 (<>)) x y

instance Monoid a => Monoid (Trie2 a) where
  mempty = Trie2 mempty mempty

mapMaybe2 :: (a -> Maybe b) -> Trie2 a -> Maybe (Trie2 b)
mapMaybe2 f = go
 where
  go (Trie2 x xs) = case (f =<< x, nullToMaybe $ Map.mapMaybe go xs) of
    (Nothing, Nothing) -> Nothing
    (Just x', Nothing) -> Just $ Trie2 (Just x') mempty
    (x', Just xs') -> Just $ Trie2 x' xs'

singleton2 :: BCP47 -> a -> Trie2 a
singleton2 tag = fromSubtags (toSubtags tag)

fromSubtags :: [Subtags] -> a -> Trie2 a
fromSubtags =
  foldr (\path leaf -> Trie2 Nothing . Map.singleton path . leaf) toVal

toVal :: a -> Trie2 a
toVal x = Trie2 (Just x) mempty

lookup2 :: BCP47 -> Trie2 a -> Maybe a
lookup2 tag = getLast . go (toSubtags tag)
 where
  go :: [Subtags] -> Trie2 a -> Last a
  go [] (Trie2 mVal _) = Last mVal
  go (p : ps) (Trie2 mVal children) =
    Last mVal <> (go ps =<< Last (Map.lookup p children))

match2 :: BCP47 -> Trie2 a -> Maybe a
match2 tag = go (toSubtags tag)
 where
  go :: [Subtags] -> Trie2 a -> Maybe a
  go [] (Trie2 mVal _) = mVal
  go (p : ps) (Trie2 _ children) = go ps =<< Map.lookup p children

union2Using :: (Maybe a -> Maybe a -> Maybe a) -> Trie2 a -> Trie2 a -> Trie2 a
union2Using f (Trie2 x xs) (Trie2 y ys) =
  Trie2 (f x y) (Map.unionWith (union2Using f) xs ys)
