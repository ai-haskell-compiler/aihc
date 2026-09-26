{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Num.BigNat
  ( BigNat (..),
    BigNat#,
    bigNatZero,
    bigNatOne,
    bigNatFromWord#,
    bigNatSize#,
    bigNatIsZero,
    bigNatIsOne,
    bigNatAnd,
    bigNatOr,
    bigNatXor,
  )
where

import GHC.Classes (Eq (..))
import GHC.Prim (ByteArray#, Int#, Word#, compareByteArrays#, eqWord#, indexWordArray#, int2Word#, newByteArray#, quotInt#, realWorld#, sizeofByteArray#, unsafeFreezeByteArray#, writeWordArray#, (==#))
import GHC.Prim.Integer (Integer (..), integerAnd, integerFromMagnitude#, integerOr, integerXor)
import GHC.Types (Bool (..), isTrue#)

-- | The magnitude of an arbitrary-precision number: a canonical,
-- little-endian sequence of 64-bit limbs with no trailing zero limb.  This is
-- the payload that 'GHC.Num.Integer.Integer' carries in @IP@ and @IN@ and
-- that 'GHC.Num.Natural.Natural' carries in @NB@.
type BigNat# = ByteArray#

-- | Lifted wrapper for a 'BigNat#'.
--
-- The magnitude itself is unlifted, so it cannot be stored directly in
-- ordinary lifted data structures or passed to a class method.  This wrapper
-- is the representation packages such as @hashable@ match on.
data BigNat = BN# {unBigNat :: BigNat#}

-- | A magnitude is canonical, so two of them denote the same number exactly
-- when they hold the same bytes.
instance Eq BigNat where
  BN# left == BN# right =
    case sizeofByteArray# left ==# sizeofByteArray# right of
      0# -> False
      _ -> case compareByteArrays# left 0# right 0# (sizeofByteArray# left) of
        0# -> True
        _ -> False

  BN# left /= BN# right =
    case sizeofByteArray# left ==# sizeofByteArray# right of
      0# -> True
      _ -> case compareByteArrays# left 0# right 0# (sizeofByteArray# left) of
        0# -> False
        _ -> True

-- | The magnitude zero, which has no limb.
bigNatZero :: BigNat
bigNatZero =
  case newByteArray# 0# realWorld# of
    (# state, mutable #) ->
      case unsafeFreezeByteArray# mutable state of
        (# _, magnitude #) -> BN# magnitude

-- | The magnitude one.
bigNatOne :: BigNat
bigNatOne = BN# (bigNatFromWord# (int2Word# 1#))

-- | The magnitude of a word: no limb for zero, one limb otherwise.
bigNatFromWord# :: Word# -> BigNat#
bigNatFromWord# word =
  case eqWord# word (int2Word# 0#) of
    1# -> case bigNatZero of BN# zero -> zero
    _ ->
      case newByteArray# 8# realWorld# of
        (# state, mutable #) ->
          case writeWordArray# mutable 0# word state of
            state1 ->
              case unsafeFreezeByteArray# mutable state1 of
                (# _, magnitude #) -> magnitude

-- | The number of limbs.
bigNatSize# :: BigNat# -> Int#
bigNatSize# magnitude = quotInt# (sizeofByteArray# magnitude) 8#

-- | Whether the magnitude is zero.
bigNatIsZero :: BigNat# -> Bool
bigNatIsZero magnitude = isTrue# (bigNatSize# magnitude ==# 0#)

-- | Whether the magnitude is one.
bigNatIsOne :: BigNat# -> Bool
bigNatIsOne magnitude =
  case bigNatSize# magnitude ==# 1# of
    1# -> isTrue# (eqWord# (indexWordArray# magnitude 0#) (int2Word# 1#))
    _ -> False

-- | The bitwise and of two magnitudes.
bigNatAnd :: BigNat# -> BigNat# -> BigNat#
bigNatAnd left right = integerMagnitude# (integerAnd (integerFromMagnitude# 1# left) (integerFromMagnitude# 1# right))

-- | The bitwise or of two magnitudes.
bigNatOr :: BigNat# -> BigNat# -> BigNat#
bigNatOr left right = integerMagnitude# (integerOr (integerFromMagnitude# 1# left) (integerFromMagnitude# 1# right))

-- | The bitwise exclusive or of two magnitudes.
bigNatXor :: BigNat# -> BigNat# -> BigNat#
bigNatXor left right = integerMagnitude# (integerXor (integerFromMagnitude# 1# left) (integerFromMagnitude# 1# right))

-- The bitwise operations reuse the limb code behind 'Integer', so a
-- magnitude goes through a non-negative 'Integer' and comes back out of it.
integerMagnitude# :: Integer -> BigNat#
integerMagnitude# value =
  case value of
    IS small -> bigNatFromWord# (int2Word# small)
    IP magnitude -> magnitude
    IN magnitude -> magnitude
