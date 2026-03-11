{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
module GHC.Int.Compat (W.Int8,
    W.Int16,
    W.Int32,
    W.Int64(..),
    Int(..),
#if MIN_VERSION_ghc_prim(0,8,0)
    pattern GHC.Int.Compat.I8#,
    pattern GHC.Int.Compat.I16#,
    pattern GHC.Int.Compat.I32#,
#else
    pattern W.I8#,
    pattern W.I16#,
    pattern W.I32#,
#endif

    uncheckedIShiftL64#, uncheckedIShiftRA64#,
    -- * Equality operators
    -- | See GHC.Classes#matching_overloaded_methods_in_rules
    eqInt, neInt, gtInt, geInt, ltInt, leInt,
    eqInt8, neInt8, gtInt8, geInt8, ltInt8, leInt8,
    eqInt16, neInt16, gtInt16, geInt16, ltInt16, leInt16,
    eqInt32, neInt32, gtInt32, geInt32, ltInt32, leInt32,
    eqInt64, neInt64, gtInt64, geInt64, ltInt64, leInt64) where

import GHC.Int hiding (Int8(..), Int16(..), Int32(..))
import qualified GHC.Int as W

#if MIN_VERSION_ghc_prim(0,8,0)

import GHC.Prim

pattern I8# :: Int# -> W.Int8
pattern I8# x <- (W.I8# (int8ToInt# -> x)) where
  I8# x = W.I8# (intToInt8# x)
{-# COMPLETE I8# #-}

pattern I16# :: Int# -> W.Int16
pattern I16# x <- (W.I16# (int16ToInt# -> x)) where
  I16# x = W.I16# (intToInt16# x)
{-# COMPLETE I16# #-}

pattern I32# :: Int# -> W.Int32
pattern I32# x <- (W.I32# (int32ToInt# -> x)) where
  I32# x = W.I32# (intToInt32# x)
{-# COMPLETE I32# #-}

#endif
