{-# LANGUAGE
  DataKinds,
  DerivingVia,
  FlexibleInstances,
  StandaloneDeriving,
  TypeOperators #-}

-- | Basic configurations to get started.
module DiffLoc.Starter
  ( -- * The heavy lifter
    Diff

    -- * Basic index types
  , Z
  , N
  , N'

    -- * Under the hood
  , (:$:)(..)
  ) where

import GHC.TypeNats (KnownNat)
import DiffLoc.Diff
import DiffLoc.Interval
import DiffLoc.Index
import DiffLoc.Shift
import DiffLoc.Unsafe

-- $setup
-- >>> import DiffLoc
-- >>> import DiffLoc.Unsafe

-- | A shorthand for the common use case of 'Diff'.
type Diff p = ADiff (Replace p)

-- | A trick to reduce noise by hiding newtype wrapper constructors.
-- This makes the documentation more palatable.
--
-- >>> show (NoShow (Plain 3) :: Plain :$: Int)
-- "3"
-- >>> show (Colline 4 2 :.. Vallee (offset 3) (offset 3) :: Interval (Colline N N))
-- "Colline 4 2 :.. Vallee (offset 3) (offset 3)"
newtype f :$: x = NoShow (f x)
  deriving (Eq, Ord)
  deriving (Semigroup, Monoid, Amor, Origin) via (f x)

instance Show a => Show (Plain :$: a) where
  show (NoShow (Plain i)) = show i

instance Show a => Show (IndexFrom n :$: a) where
  show (NoShow i) = show (fromIndex i)

instance Show a => Show (Offset :$: a) where
  show (NoShow i) = show (fromOffset i)

instance Num a => Num (Plain :$: a) where
  fromInteger n = NoShow (Plain (fromInteger n))
  (+) = undefined ; (-) = undefined ; (*) = undefined ; abs = undefined ; signum = undefined

instance (Num a, Ord a, KnownNat n) => Num (IndexFrom n :$: a) where
  fromInteger n = NoShow (indexFrom (fromInteger n))
  (+) = undefined ; (-) = undefined ; (*) = undefined ; abs = undefined ; signum = undefined

instance (Num a, Ord a) => Num (Offset :$: a) where
  fromInteger n = NoShow (offset (fromInteger n))
  (+) = undefined ; (-) = undefined ; (*) = undefined ; abs = undefined ; signum = undefined

-- | Integers.
type Z = Plain :$: Int

-- | Natural numbers.
type N = IndexFrom 0 :$: Int

-- | Positive numbers.
type N' = IndexFrom 1 :$: Int
