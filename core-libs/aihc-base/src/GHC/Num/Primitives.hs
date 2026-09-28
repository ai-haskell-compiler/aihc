{-# LANGUAGE MagicHash #-}

-- | Primitive helpers of @ghc-bignum@.  Only the part that packages use
-- is here.
module GHC.Num.Primitives
  ( Bool#,
    wordSizeInBase#,
  )
where

import GHC.Prim (Int#, Word#, eqWord#, int2Word#, plusWord#)
import GHC.Prim.Integer (integerFromWord#, integerLogBase#)

-- | A boolean as an 'Int#': @1#@ is true and @0#@ is false.
type Bool# = Int#

-- | The number of digits of a word in a base greater than one.  Zero has
-- no digit.
wordSizeInBase# :: Word# -> Word# -> Word#
wordSizeInBase# base value =
  case eqWord# value (int2Word# 0#) of
    1# -> int2Word# 0#
    _ -> plusWord# (integerLogBase# (integerFromWord# 1# base) (integerFromWord# 1# value)) (int2Word# 1#)
