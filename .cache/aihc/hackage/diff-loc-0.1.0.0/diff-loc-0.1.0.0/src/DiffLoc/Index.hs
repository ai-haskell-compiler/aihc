{-# LANGUAGE
  AllowAmbiguousTypes,
  DataKinds,
  DerivingStrategies,
  DerivingVia,
  GeneralizedNewtypeDeriving,
  ScopedTypeVariables,
  TypeApplications,
  TypeFamilies #-}

-- | Indices and offsets.
module DiffLoc.Index
  ( -- * One-dimensional indices
    -- ** Unbounded indices
    Plain(..)

    -- ** Indices bounded by an origin
  , IndexFrom()
  , indexFromM
  , indexFromM0
  , indexFromM1
  , fromIndex
  , fromIndex0
  , fromIndex1
  , zeroIndex
  , oneIndex

    -- ** Offsets
  , Offset()
  , offsetM
  , fromOffset
  ) where

import Data.Monoid (Sum(..))
import Data.Proxy (Proxy(..))
import GHC.TypeNats (KnownNat, Nat, natVal)
import Text.Show.Combinators (showCon, (@|))
import DiffLoc.Shift

-- | One-dimensional indices.
newtype Plain a = Plain a
  deriving (Eq, Ord, Show)

instance (Num a, Ord a) => Amor (Plain a) where
  type Trans (Plain a) = Offset a
  Plain y .+ Offset x = Plain (x Prelude.+ y)
  Plain x .-.? Plain y | y <= x = Just (Offset (x - y))
                       | otherwise = Nothing

--

-- | One-dimensional indices with an origin (an initial index).
-- Indices must be greater than the origin, hence the constructor is hidden.
--
-- Use 'indexFromM' to construct indices, with @TypeApplications@ to make the
-- indexing scheme explicit, and 'fromIndex' to destruct them.
--
-- @
-- (origin :: IndexFrom n a) <= i    -- for all i
-- @
newtype IndexFrom (n :: Nat) a = IndexFrom a
  deriving (Eq, Ord)
  deriving Amor via (Plain a)

instance Show a => Show (IndexFrom n a) where
  showsPrec = flip $ \(IndexFrom i) -> showCon "indexFrom" @| i

instance (Num a, Ord a, KnownNat n) => Origin (IndexFrom n a) where
  origin = IndexFrom (knownNum @n)

-- | Reify a 'KnownNat'.
--
-- @
-- knownNum @42 = 42
-- @
knownNum :: forall n a. (KnownNat n, Num a) => a
knownNum = fromIntegral (natVal @n Proxy)

-- | Constructor for 'IndexFrom'.
--
-- See also 'DiffLoc.Unsafe.indexFrom' in "DiffLoc.Unsafe", a variant of 'indexFromM' that
-- throws errors instead of using @Maybe@.
indexFromM :: forall n a. (KnownNat n, Num a, Ord a) => a -> Maybe (IndexFrom n a)
indexFromM i | knownNum @n <= i = Just (IndexFrom i)
             | otherwise = Nothing

-- | 'indexFromM' specialized to 0-indexing.
indexFromM0 :: forall a. (Num a, Ord a) => a -> Maybe (IndexFrom 0 a)
indexFromM0 = indexFromM

-- | 'indexFromM' specialized to 1-indexing.
indexFromM1 :: forall a. (Num a, Ord a) => a -> Maybe (IndexFrom 1 a)
indexFromM1 = indexFromM

-- | Destructor for 'IndexFrom'.
fromIndex :: forall n a. IndexFrom n a -> a
fromIndex (IndexFrom i) = i

-- | 'fromIndex' specialized to 0-indexing.
fromIndex0 :: IndexFrom 0 a -> a
fromIndex0 = fromIndex

-- | 'fromIndex' specialized to 1-indexing.
fromIndex1 :: IndexFrom 1 a -> a
fromIndex1 = fromIndex

-- | Convert from zero-indexing to one-indexing.
oneIndex :: Num a => IndexFrom 0 a -> IndexFrom 1 a
oneIndex (IndexFrom i) = IndexFrom (i Prelude.+ 1)

-- | Convert from one-indexing to zero-indexing.
zeroIndex :: Num a => IndexFrom 1 a -> IndexFrom 0 a
zeroIndex (IndexFrom i) = IndexFrom (i - 1)

-- | Type of nonnegative offsets.
newtype Offset a = Offset a
  deriving (Eq, Ord)
  deriving (Semigroup, Monoid) via (Sum a)

instance Show a => Show (Offset a) where
  showsPrec = flip $ \(Offset i) -> showCon "offset" @| i

-- | Construct a nonnegative 'Offset'.
--
-- See also 'DiffLoc.Unsafe.offset' in "DiffLoc.Unsafe", a variant of 'offsetM' that
-- throws errors instead of using @Maybe@.
offsetM :: (Num a, Ord a) => a -> Maybe (Offset a)
offsetM i | 0 <= i = Just (Offset i)
          | otherwise = Nothing

-- | Unwrap 'Offset'.
fromOffset :: Offset a -> a
fromOffset (Offset i) = i
