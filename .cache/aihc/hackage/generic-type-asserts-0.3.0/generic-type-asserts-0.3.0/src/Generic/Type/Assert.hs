{-# LANGUAGE UndecidableInstances #-} -- for IsEnum

-- | Structural assertions on generic type representation.

module Generic.Type.Assert
  ( type GAssertNotVoid
  , type GAssertNotSum, type GAssertSum
  , type GAssertEnum
  ) where

import GHC.Generics
import Generic.Type.Assert.Error
import GHC.TypeError ( Assert )

-- | Type is not void i.e. has at least one constructor.
type GAssertNotVoid a =
    Assert (IsNotVoid (StripD1 (Rep a))) (GAssertErrorVoid a)
type family IsNotVoid a where
    IsNotVoid V1  = False
    IsNotVoid _ = True

-- | Type is not a sum type i.e. has at most one constructor.
--
-- Permits void types.
type GAssertNotSum a =
    Assert (IsNotSum (StripD1 (Rep a))) (GAssertErrorSum a)
type family IsNotSum a where
    IsNotSum (_ :+: _) = False
    IsNotSum _ = True

-- | Type is a sum type i.e. has >=2 constructors.
--
-- Permits void types.
type GAssertSum a =
    Assert (IsSum (StripD1 (Rep a))) (GAssertErrorNotSum a)
type family IsSum a where
    IsSum (C1 _ _) = False
    IsSum _ = True

-- | Type has only empty constructors.
--
-- Permits void types.
type GAssertEnum a =
    Assert (IsEnum (StripD1 (Rep a))) (GAssertErrorNotEnum a)
type family IsEnum a where
    IsEnum V1 = True
    IsEnum (C1 _ a) = ConsIsEmpty a
    IsEnum (l :+: r) = IsEnum l `And` IsEnum r

--------------------------------------------------------------------------------

type family StripD1 a where StripD1 (D1 _ a) = a

type family ConsIsEmpty a where
    ConsIsEmpty U1 = True
    ConsIsEmpty _  = False

-- | Type level boolean AND.
type family And l r where
    And True True = True
    And _    _    = False
