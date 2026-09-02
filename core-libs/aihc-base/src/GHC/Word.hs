{-# LANGUAGE MagicHash #-}

module GHC.Word
  ( Word (..),
    Word8 (..),
    Word16 (..),
    Word32 (..),
    Word64 (..),
    byteSwap16,
    byteSwap32,
    byteSwap64,
  )
where

import GHC.Prim
  ( Word#,
    Word16#,
    Word32#,
    Word64#,
    Word8#,
    byteSwap16#,
    byteSwap32#,
    byteSwap64#,
    word16ToWord#,
    word32ToWord#,
    wordToWord16#,
    wordToWord32#,
  )

data Word = W# Word#

data Word8 = W8# Word8#

data Word16 = W16# Word16#

data Word32 = W32# Word32#

data Word64 = W64# Word64#

-- | Reverse the byte order of a 16-bit word.
byteSwap16 :: Word16 -> Word16
byteSwap16 (W16# value) = W16# (wordToWord16# (byteSwap16# (word16ToWord# value)))

-- | Reverse the byte order of a 32-bit word.
byteSwap32 :: Word32 -> Word32
byteSwap32 (W32# value) = W32# (wordToWord32# (byteSwap32# (word32ToWord# value)))

-- | Reverse the byte order of a 64-bit word.
byteSwap64 :: Word64 -> Word64
byteSwap64 (W64# value) = W64# (byteSwap64# value)
