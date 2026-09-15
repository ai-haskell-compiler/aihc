{-# LANGUAGE MagicHash #-}

module GHC.Num.BigNat
  ( BigNat (..),
    BigNat#,
  )
where

import GHC.Classes (Eq (..))
import GHC.Prim (ByteArray#, compareByteArrays#, sizeofByteArray#, (==#))
import GHC.Types (Bool (..))

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
