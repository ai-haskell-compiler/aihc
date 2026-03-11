{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
  DerivingVia,
  FlexibleContexts,
  FlexibleInstances,
  PatternSynonyms,
  ScopedTypeVariables,
  StandaloneDeriving,
  TypeApplications,
  TypeFamilies,
  TypeOperators,
  UndecidableInstances #-}
module DiffLoc.Test
  ( (=.=)
  , partialSemiInverse
  , GoodSpan(.., GSN, GSV)
  , FairSpan(.., FSN, FSV)
  ) where

import Data.List (sort)
import Data.Proxy (Proxy(..))
import GHC.TypeNats (KnownNat, natVal)
import Test.QuickCheck
import Test.QuickCheck.HigherOrder

import DiffLoc
import DiffLoc.Unsafe

-- $setup
-- >>> import Data.Maybe
-- >>> import DiffLoc
-- >>> import Test.QuickCheck
-- >>> quickCheck = quickCheckWith stdArgs{maxSuccess=3000}

infix 1 =.=

(=.=) :: (Arbitrary a, Show a, Eq b, Show b) => (a -> b) -> (a -> b) -> Property
f =.= g = property (\x -> f x === g x)

whenJust :: Testable prop => Maybe a -> (a -> prop) -> Property
whenJust Nothing _ = discard
whenJust (Just x) f = property (f x)

partialSemiInverse :: (Arbitrary a, Eq a, Show a) => (a -> Maybe b) -> (b -> Maybe a) -> a -> Property
partialSemiInverse f g x = f x `whenJust` \y -> g y === Just x

instance (Arbitrary p, Arbitrary (Trans p)) => Arbitrary (Interval p) where
  arbitrary = do
    (i, n) <- arbitrary
    pure (i :.. n)
  shrink (i :.. n) = [ i' :.. n' | (i', n') <- shrink (i, n) ]

instance (Arbitrary p, Arbitrary (Trans p)) => Arbitrary (Replace p) where
  arbitrary = do
    (i, n, m) <- arbitrary
    pure (Replace i n m)
  shrink (Replace i n m) =
    [ Replace i' n' m' | (i', n', m') <- shrink (i, n, m) ]

instance (Arbitrary p, Show p, Arbitrary (Trans p), Show (Trans p)) => Constructible (Interval p) where
  type Repr (Interval p) = Interval p
  fromRepr = id

instance (Arbitrary p, Show p, Arbitrary (Trans p), Show (Trans p)) => Constructible (Replace p) where
  type Repr (Replace p) = Replace p
  fromRepr = id

instance (Shift r, Arbitrary r, Show r) => Constructible (ADiff r) where
  type Repr (ADiff r) = [r]
  fromRepr = listToDiff

-- | Generate GoodSpan most of the time, but also some completely arbitrary ones once in a while.
data FairSpan p = FS (Diff p) (Interval p)

-- | A Diff and a non-conflicting Span
data GoodSpan p = GS (Diff p) (Interval p)

deriving instance (Show p, Show (Trans p)) => Show (FairSpan p)
deriving instance (Show p, Show (Trans p)) => Show (GoodSpan p)

pattern FSN :: Diff N -> Interval N -> FairSpan N
pattern FSN d s = FS d s

pattern GSN :: Diff N -> Interval N -> GoodSpan N
pattern GSN d s = GS d s

pattern FSV :: Diff (Colline N N) -> Interval (Colline N N) -> FairSpan (Colline N N)
pattern FSV d s = FS d s

pattern GSV :: Diff (Colline N N) -> Interval (Colline N N) -> GoodSpan (Colline N N)
pattern GSV d s = GS d s

instance (Amor p, Arbitrary p, Arbitrary (Trans p)) => Arbitrary (FairSpan p) where
  arbitrary = frequency [(10, (\(GS d s) -> FS d s) <$> arbitrary), (1, arbitrary)]

instance (Amor p, Arbitrary p, Arbitrary (Trans p), Show p, Show (Trans p)) => Constructible (FairSpan p) where
  type Repr (FairSpan p) = FairSpan p
  fromRepr = id

-- |
-- prop> \(GSN d s) -> isJust (mapDiff d s)
-- prop> \(GSV d s) -> isJust (mapDiff d s)
instance (Amor p, Arbitrary p, Arbitrary (Trans p)) => Arbitrary (GoodSpan p) where
  arbitrary = do
    let pairs (x : y : xs) | x == y = pairs (x : xs)
                           | otherwise = (x, y) : pairs xs
        pairs _ = []
    ts <- scale (* 2) (pairs <$> sort <$> arbitrary) `suchThat` (not . null)
    i <- choose (0, length ts-1)
    let (s, ts') = case splitAt i ts of
          (pre, (x, y) : suf) -> (x :.. (y .-. x), pre ++ suf)
          _ -> error "should not happen"
    d <- listToDiff <$> traverse (\(x, y) -> Replace x (y .-. x) <$> arbitrary) ts'
    pure (GS d s)

instance (Arbitrary l, Arbitrary c) => Arbitrary (Colline l c) where
  arbitrary = Colline <$> arbitrary <*> arbitrary

instance (Arbitrary l, Arbitrary c) => Arbitrary (Vallee l c)  where
  arbitrary = Vallee <$> arbitrary <*> arbitrary

deriving via a instance Arbitrary a => Arbitrary (Plain a)
deriving via (f a) instance Arbitrary (f a) => Arbitrary (f :$: a)

genLowerBound :: (Arbitrary a, Num a, Ord a) => a -> Gen a
genLowerBound n = do
  NonNegative i <- arbitrary
  pure (if n <= n + i then n + i else n)  -- catches overflow

instance (Arbitrary a, Num a, Ord a, KnownNat n) => Arbitrary (IndexFrom n a) where
  arbitrary = indexFrom <$> genLowerBound (fromIntegral (natVal @n Proxy))

instance (Arbitrary a, Num a, Ord a) => Arbitrary (Offset a) where
  arbitrary = offset <$> genLowerBound 0
