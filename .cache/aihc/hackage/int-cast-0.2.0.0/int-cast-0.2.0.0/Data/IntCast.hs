{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ < 707
#error This code requires GHC 7.7+
#endif

#include "MachDeps.h"
#include "HsBaseConfig.h"

-- |
-- Module:  Data.IntCast
-- Copyright:   © 2014-2018  Herbert Valerio Riedel
-- License:     BSD-style (see the LICENSE file)
--
-- Maintainer:  Herbert Valerio Riedel <hvr@gnu.org>
-- Stability:   experimental
-- Portability: GHC ≥ 7.8
--
-- This module provides for statically or dynamically checked
-- conversions between 'Integral' types.
module Data.IntCast
    ( -- * Conversion functions
      -- ** statically checked
      --
      -- | In the table below each cell denotes which of the three
      -- 'intCast', 'intCastIso' and 'intCastEq' conversion operations
      -- are allowed (i.e. by the type-checker). The rows represent
      -- the domain @a@ while the columns represent the codomain @b@
      -- of the @a->b@-typed conversion functions.
      --
      -- +-----------+-----------------+--------------+----------------------------------------+------------------------------------------+
      -- |           | 'Natural'       | 'Word32'     | 'Word64'                               | 'Int'                                    |
      -- +-----------+-----------------+--------------+----------------------------------------+------------------------------------------+
      -- | 'Word'    | 'intCast'       |              | 'intCast' & 'intCastEq' & 'intCastIso' | 'intCastIso'                             |
      -- +-----------+-----------------+--------------+----------------------------------------+------------------------------------------+
      -- | 'Word16'  | 'intCast'       | 'intCast'    | 'intCast'                              | 'intCast'                                |
      -- +-----------+-----------------+--------------+----------------------------------------+------------------------------------------+
      -- | 'Int64'   |                 |              | 'intCastIso'                           | 'intCast' & 'intCastEq' & 'intCastIso'   |
      -- +-----------+-----------------+--------------+----------------------------------------+------------------------------------------+
      -- | 'Int8'    |                 |              |                                        | 'intCast'                                |
      -- +-----------+-----------------+--------------+----------------------------------------+------------------------------------------+
      --
      -- __Note:__ The table above assumes a 64-bit platform (i.e. where @finiteBitSize (0 :: Word) == 64@).
      intCast
    , intCastIso
    , intCastEq
      -- ** dynamically checked
    , intCastMaybe

      -- * Registering new integer types
      -- |
      --  * For 'intCastMaybe' you need to provide type-class instances of 'Bits'
      --    (and 'Integral').
      --
      --  * For 'intCast', 'intCastIso', and 'intCastEq' simply
      --    declare instances for the 'IntBaseType' type-family (as well
      --    as type-class instances of 'Integral') as described below.
    , IntBaseType
    , IntBaseTypeK(..)

      -- * Type-level predicates
      -- | The following type-level predicates are used by 'intCast',
      -- 'intCastIso', and 'intCastEq' respectively.
    , IsIntSubType
    , IsIntBaseSubType
    , IsIntTypeIso
    , IsIntBaseTypeIso
    , IsIntTypeEq
    , IsIntBaseTypeEq
    ) where

-- Haskell 2010+
import           Data.Bits
import           Data.Int
import           Data.Word
import           Foreign.C.Types

-- non-Haskell 2010
import           GHC.TypeLits
import           Numeric.Natural (Natural)

-- | (Kind) Meta-information about integral types.
--
-- If also a 'Bits' instance is defined, the type-level information
-- provided by 'IntBaseType' ought to match the meta-information that
-- is conveyed by the 'Bits' class' 'isSigned' and 'bitSizeMaybe'
-- methods.
data IntBaseTypeK
     -- | fixed-width \(n\)-bit integers with value range \( \left[ -2^{n-1}, 2^{n-1}-1 \right] \).
     = FixedIntTag Nat
     -- | fixed-width \(n\)-bit integers with value range  \( \left[ 0, 2^{n} \right] \).
     | FixedWordTag Nat
     -- | integers with value range \( \left] -\infty, +\infty \right[ \).
     | BigIntTag
     -- | naturals with value range \( \left[ 0, +\infty \right[ \).
     | BigWordTag

-- | The (open) type family 'IntBaseType' encodes type-level
-- information about the value range of an integral type.
--
-- This module also provides type family instances for the standard
-- Haskell 2010 integral types (including "Foreign.C.Types") as well
-- as the 'Natural' type.
--
-- Here's a simple example for registering a custom type with the
-- "Data.IntCast" facilities:
--
-- @
-- /-- user-implemented unsigned 4-bit integer/
-- data Nibble = …
--
-- /-- declare meta-information/
-- type instance 'IntBaseType' Nibble = 'FixedWordTag' 4
--
-- /-- user-implemented signed 7-bit integer/
-- data MyInt7 = …
--
-- /-- declare meta-information/
-- type instance 'IntBaseType' MyInt7 = 'FixedIntTag' 7
-- @
--
-- The type-level predicate 'IsIntSubType' provides a partial
-- ordering based on the types above. See also 'intCast'.
type family IntBaseType a :: IntBaseTypeK

type instance IntBaseType Integer = 'BigIntTag
type instance IntBaseType Natural = 'BigWordTag

-- Haskell2010 Basic fixed-width Integer Types
type instance IntBaseType Int8   = 'FixedIntTag  8
type instance IntBaseType Int16  = 'FixedIntTag  16
type instance IntBaseType Int32  = 'FixedIntTag  32
type instance IntBaseType Int64  = 'FixedIntTag  64
type instance IntBaseType Word8  = 'FixedWordTag 8
type instance IntBaseType Word16 = 'FixedWordTag 16
type instance IntBaseType Word32 = 'FixedWordTag 32
type instance IntBaseType Word64 = 'FixedWordTag 64

#if defined(WORD_SIZE_IN_BITS)
type instance IntBaseType Int    = {-'-} 'FixedIntTag  WORD_SIZE_IN_BITS
type instance IntBaseType Word   = {-'-} 'FixedWordTag WORD_SIZE_IN_BITS
#else
#error Cannot determine bit-size of 'Int'/'Word' type
#endif

-- Haskell2010 FFI Integer Types
type instance IntBaseType CChar      = IntBaseType HTYPE_CHAR
type instance IntBaseType CInt       = IntBaseType HTYPE_INT
type instance IntBaseType CIntMax    = IntBaseType HTYPE_INTMAX_T
type instance IntBaseType CIntPtr    = IntBaseType HTYPE_INTPTR_T
type instance IntBaseType CLLong     = IntBaseType HTYPE_LONG_LONG
type instance IntBaseType CLong      = IntBaseType HTYPE_LONG
type instance IntBaseType CPtrdiff   = IntBaseType HTYPE_PTRDIFF_T
type instance IntBaseType CSChar     = IntBaseType HTYPE_SIGNED_CHAR
type instance IntBaseType CShort     = IntBaseType HTYPE_SHORT
type instance IntBaseType CSigAtomic = IntBaseType HTYPE_SIG_ATOMIC_T
type instance IntBaseType CSize      = IntBaseType HTYPE_SIZE_T
type instance IntBaseType CUChar     = IntBaseType HTYPE_UNSIGNED_CHAR
type instance IntBaseType CUInt      = IntBaseType HTYPE_UNSIGNED_INT
type instance IntBaseType CUIntMax   = IntBaseType HTYPE_UINTMAX_T
type instance IntBaseType CUIntPtr   = IntBaseType HTYPE_UINTPTR_T
type instance IntBaseType CULLong    = IntBaseType HTYPE_UNSIGNED_LONG_LONG
type instance IntBaseType CULong     = IntBaseType HTYPE_UNSIGNED_LONG
type instance IntBaseType CUShort    = IntBaseType HTYPE_UNSIGNED_SHORT

-- | Closed type family providing the partial order of (improper) subtype-relations
--
-- 'IsIntSubType' provides a more convenient entry point.
type family IsIntBaseSubType a b :: Bool where
    -- this relation is reflexive
    IsIntBaseSubType a                 a                 = 'True

    -- Every integer is a subset of 'Integer'
    IsIntBaseSubType a                 'BigIntTag        = 'True

    -- Even though Haskell2010 doesn't provide naturals, we can use the
    -- tag 'Nat' to denote such entities
    IsIntBaseSubType ('FixedWordTag a) 'BigWordTag       = 'True

    -- sub-type relations between fixed-with types
    IsIntBaseSubType ('FixedIntTag  a) ('FixedIntTag  b) = a   <=? b
    IsIntBaseSubType ('FixedWordTag a) ('FixedWordTag b) = a   <=? b
    IsIntBaseSubType ('FixedWordTag a) ('FixedIntTag  b) = a+1 <=? b

    -- everything else is not a sub-type
    IsIntBaseSubType a                 b                 = 'False

type IsIntSubType a b = IsIntBaseSubType (IntBaseType a) (IntBaseType b)

-- | Closed type family representing an equality-relation on bit-width
--
-- This is a superset of the 'IsIntBaseTypeEq' relation, as it ignores
-- the signedness of fixed-size integers (i.e. 'Int32' is considered
-- equal to 'Word32').
--
-- 'IsIntTypeIso' provides a more convenient entry point.
type family IsIntBaseTypeIso a b :: Bool where
    IsIntBaseTypeIso a                 a                 = 'True
    IsIntBaseTypeIso ('FixedIntTag  n) ('FixedWordTag n) = 'True
    IsIntBaseTypeIso ('FixedWordTag n) ('FixedIntTag  n) = 'True
    IsIntBaseTypeIso a                 b                 = 'False

type IsIntTypeIso a b = IsIntBaseTypeIso (IntBaseType a) (IntBaseType b)

-- | Closed type family representing an equality-relation on the integer base-type.
--
-- 'IsIntBaseTypeEq' provides a more convenient entry point.
type family IsIntBaseTypeEq (a :: IntBaseTypeK) (b :: IntBaseTypeK) :: Bool where
    IsIntBaseTypeEq a a = 'True
    IsIntBaseTypeEq a b = 'False

type IsIntTypeEq a b = IsIntBaseTypeEq (IntBaseType a) (IntBaseType b)

-- Starting w/ GHC 8.4, (==) became a closed type-family, so the
-- following convenience instance isn't possibly anymore
--
-- type instance a == b = IsIntBaseTypeEq a b
--
-- https://github.com/haskell-hvr/int-cast/issues/3

-- | Statically checked integer conversion which satisfies the property
--
--  * @'toInteger' ≡ 'toInteger' . intCast@
--
-- __Note:__ This is just a type-restricted alias of 'fromIntegral' and
-- should therefore lead to the same compiled code as if
-- 'fromIntegral' had been used instead of 'intCast'.
intCast :: (Integral a, Integral b, IsIntSubType a b ~ 'True) => a -> b
intCast = fromIntegral
{-# INLINE intCast #-}

-- | Statically checked integer conversion which satisfies the properties
--
--  * @∀β . 'intCastIso' ('intCastIso' a ∷ β) == a@
--
--  * @'toInteger' ('intCastIso' a) == 'toInteger' b     (__if__ 'toInteger' a == 'toInteger' b)@
--
-- __Note:__ This is just a type-restricted alias of 'fromIntegral' and
-- should therefore lead to the same compiled code as if
-- 'fromIntegral' had been used instead of 'intCastIso'.
intCastIso :: (Integral a, Integral b, IsIntTypeIso a b ~ 'True) => a -> b
intCastIso = fromIntegral
{-# INLINE intCastIso #-}

-- | Version of 'intCast' restricted to casts between types with same value domain.
--
-- 'intCastEq' is the most constrained of the three conversions: The
-- existence of a 'intCastEq' conversion implies the existence of the
-- other two, i.e. 'intCastIso' and 'intCast'.
--
-- __Note:__ This is just a type-restricted alias of 'fromIntegral' and
-- should therefore lead to the same compiled code as if
-- 'fromIntegral' had been used instead of 'intCastIso'.
intCastEq :: (Integral a, Integral b, IsIntTypeEq a b ~ 'True) => a -> b
intCastEq = fromIntegral
{-# INLINE intCastEq #-}

----------------------------------------------------------------------------
----------------------------------------------------------------------------
-- dynamically checked conversion

-- | 'Bits' class based value-level predicate with same semantics as 'IsIntSubType'
isBitSubType :: (Bits a, Bits b) => a -> b -> Bool
isBitSubType _x _y
  -- reflexive
  | xWidth == yWidth, xSigned == ySigned = True

  -- Every integer is a subset of 'Integer'
  | ySigned, Nothing == yWidth           = True
  | not xSigned, not ySigned, Nothing == yWidth = True

  -- sub-type relations between fixed-with types
  | xSigned == ySigned,   Just xW <- xWidth, Just yW <- yWidth  = xW <= yW
  | not xSigned, ySigned, Just xW <- xWidth, Just yW <- yWidth  = xW < yW

  | otherwise = False
  where
    xWidth   = bitSizeMaybe _x
    xSigned  = isSigned     _x

    yWidth   = bitSizeMaybe _y
    ySigned  = isSigned     _y
{-# INLINE isBitSubType #-}

-- | Run-time-checked integer conversion
--
-- This is an optimized version of the following generic code below
--
-- > intCastMaybeRef :: (Integral a, Integral b) => a -> Maybe b
-- > intCastMaybeRef x
-- >   | toInteger x == toInteger y = Just y
-- >   | otherwise                  = Nothing
-- >   where
-- >     y = fromIntegral x
--
-- The code above is rather inefficient as it needs to go via the
-- 'Integer' type. The function 'intCastMaybe', however, is marked @INLINEABLE@ and
-- if both integral types are statically known, GHC will be able
-- optimize the code signficantly (for @-O1@ and better).
--
-- For instance (as of GHC 7.8.1) the following definitions
--
-- > w16_to_i32 = intCastMaybe :: Word16 -> Maybe Int32
-- >
-- > i16_to_w16 = intCastMaybe :: Int16 -> Maybe Word16
--
-- are translated into the following (simplified) /GHC Core/ language
--
-- > w16_to_i32 = \x -> Just (case x of _ { W16# x# -> I32# (word2Int# x#) })
-- >
-- > i16_to_w16 = \x -> case eta of _
-- >   { I16# b1 -> case tagToEnum# (<=# 0 b1) of _
-- >       { False -> Nothing
-- >       ; True -> Just (W16# (narrow16Word# (int2Word# b1)))
-- >       }
-- >   }
--
-- __Note:__ Starting with @base-4.8@, this function has been added to "Data.Bits"
-- under the name 'Data.Bits.toIntegralSized'.
--
intCastMaybe :: (Integral a, Integral b, Bits a, Bits b) => a -> Maybe b
-- the code below relies on GHC optimizing away statically decidable branches
intCastMaybe x
  | maybe True (<= x) yMinBound
  , maybe True (x <=) yMaxBound = Just y
  | otherwise = Nothing
  where
    y       = fromIntegral x

    xWidth  = bitSizeMaybe x
    yWidth  = bitSizeMaybe y

    yMinBound | isBitSubType x y = Nothing
              | isSigned x, not (isSigned y) = Just 0
              | isSigned x, isSigned y, Just yW <- yWidth
                  = Just (negate $ bit (yW-1)) -- N.B. assumes sub-type
              | otherwise                    = Nothing

    yMaxBound | isBitSubType x y = Nothing
              | isSigned x, not (isSigned y), Just xW <- xWidth, Just yW <- yWidth
              , xW <= yW+1 = Nothing -- max-bound beyond a's domain
              | Just yW <- yWidth = if isSigned y then Just (bit (yW-1)-1) else Just (bit yW-1)
              | otherwise = Nothing
{-# INLINEABLE intCastMaybe #-}
