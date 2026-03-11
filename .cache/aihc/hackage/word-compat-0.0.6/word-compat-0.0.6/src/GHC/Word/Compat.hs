{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
module GHC.Word.Compat (W.Word8,
    W.Word16,
    W.Word32,
    W.Word64,
    W.Word(..),
#if MIN_VERSION_ghc_prim(0,8,0)
    pattern GHC.Word.Compat.W8#,
    pattern GHC.Word.Compat.W16#,
    pattern GHC.Word.Compat.W32#,
#else
    pattern W.W8#,
    pattern W.W16#,
    pattern W.W32#,
#endif
#if MIN_VERSION_ghc_prim(0,9,0)
    pattern GHC.Word.Compat.W64#,
#else
    pattern W.W64#,
#endif
        -- * Shifts
    uncheckedShiftL64#,
    uncheckedShiftRL64#,

    -- * Byte swapping
    byteSwap16,
    byteSwap32,
    byteSwap64,

    -- * Bit reversal
#if MIN_VERSION_base(4,14,0)
    bitReverse8,
    bitReverse16,
    bitReverse32,
    bitReverse64,
#endif

    -- * Equality operators
    -- | See GHC.Classes#matching_overloaded_methods_in_rules
    eqWord, neWord, gtWord, geWord, ltWord, leWord,
    eqWord8, neWord8, gtWord8, geWord8, ltWord8, leWord8,
    eqWord16, neWord16, gtWord16, geWord16, ltWord16, leWord16,
    eqWord32, neWord32, gtWord32, geWord32, ltWord32, leWord32,
    eqWord64, neWord64, gtWord64, geWord64, ltWord64, leWord64) where

import GHC.Word hiding (Word8(..), Word16(..), Word32(..), Word64(..))
import qualified GHC.Word as W

#if MIN_VERSION_ghc_prim(0,8,0)

import GHC.Prim

pattern W8# :: Word# -> W.Word8
pattern W8# x <- (W.W8# (word8ToWord# -> x)) where
  W8# x = W.W8# (wordToWord8# x)
{-# COMPLETE W8# #-}

pattern W16# :: Word# -> W.Word16
pattern W16# x <- (W.W16# (word16ToWord# -> x)) where
  W16# x = W.W16# (wordToWord16# x)
{-# COMPLETE W16# #-}

pattern W32# :: Word# -> W.Word32
pattern W32# x <- (W.W32# (word32ToWord# -> x)) where
  W32# x = W.W32# (wordToWord32# x)
{-# COMPLETE W32# #-}

#endif

#if MIN_VERSION_ghc_prim(0,9,0)

pattern W64# :: Word# -> W.Word64
pattern W64# x <- (W.W64# (word64ToWord# -> x)) where
  W64# x = W.W64# (wordToWord64# x)
{-# COMPLETE W64# #-}

#endif
