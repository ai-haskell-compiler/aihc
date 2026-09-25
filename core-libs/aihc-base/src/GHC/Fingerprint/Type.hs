{-# LANGUAGE MagicHash #-}

module GHC.Fingerprint.Type (Fingerprint (..)) where

import Data.Bits (shiftL, shiftR, (.|.))
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable (..))
import GHC.Internal.Char (Char (C#))
import GHC.Prim
  ( Int#,
    Word#,
    and#,
    chr#,
    eqWord#,
    int2Word#,
    ltWord#,
    plusWord#,
    uncheckedShiftRL#,
    word2Int#,
    word64ToWord#,
  )
import GHC.Word (Word64 (W64#), Word8)
import Prelude

data Fingerprint = Fingerprint {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64

instance Eq Fingerprint where
  Fingerprint firstLeft secondLeft == Fingerprint firstRight secondRight =
    equalWord64 firstLeft firstRight && equalWord64 secondLeft secondRight
  left /= right = not (left == right)

instance Ord Fingerprint where
  compare (Fingerprint firstLeft secondLeft) (Fingerprint firstRight secondRight) =
    case compareWord64 firstLeft firstRight of
      EQ -> compareWord64 secondLeft secondRight
      result -> result
  left < right = compare left right == LT
  left <= right = compare left right /= GT
  left > right = compare left right == GT
  left >= right = compare left right /= LT
  max left right =
    case left > right of
      True -> left
      False -> right
  min left right =
    case left > right of
      True -> right
      False -> left

instance Show Fingerprint where
  show (Fingerprint firstWord secondWord) = hex16 firstWord ++ hex16 secondWord

equalWord64 :: Word64 -> Word64 -> Bool
equalWord64 (W64# left) (W64# right) =
  case eqWord# (word64ToWord# left) (word64ToWord# right) of
    0# -> False
    _ -> True

compareWord64 :: Word64 -> Word64 -> Ordering
compareWord64 (W64# left) (W64# right) = compareWord# (word64ToWord# left) (word64ToWord# right)

compareWord# :: Word# -> Word# -> Ordering
compareWord# left right =
  case eqWord# left right of
    0# ->
      case ltWord# left right of
        0# -> GT
        _ -> LT
    _ -> EQ

hex16 :: Word64 -> String
hex16 (W64# value) =
  [ hexDigit (word64ToWord# value) 60#,
    hexDigit (word64ToWord# value) 56#,
    hexDigit (word64ToWord# value) 52#,
    hexDigit (word64ToWord# value) 48#,
    hexDigit (word64ToWord# value) 44#,
    hexDigit (word64ToWord# value) 40#,
    hexDigit (word64ToWord# value) 36#,
    hexDigit (word64ToWord# value) 32#,
    hexDigit (word64ToWord# value) 28#,
    hexDigit (word64ToWord# value) 24#,
    hexDigit (word64ToWord# value) 20#,
    hexDigit (word64ToWord# value) 16#,
    hexDigit (word64ToWord# value) 12#,
    hexDigit (word64ToWord# value) 8#,
    hexDigit (word64ToWord# value) 4#,
    hexDigit (word64ToWord# value) 0#
  ]

hexDigit :: Word# -> Int# -> Char
hexDigit word shift =
  hexDigitValue (and# (uncheckedShiftRL# word shift) (int2Word# 15#))

hexDigitValue :: Word# -> Char
hexDigitValue digit =
  case ltWord# digit (int2Word# 10#) of
    0# -> C# (chr# (word2Int# (plusWord# digit (int2Word# 87#))))
    _ -> C# (chr# (word2Int# (plusWord# digit (int2Word# 48#))))

-- | A fingerprint is stored as its two words, each in big-endian byte order,
-- as GHC does.
instance Storable Fingerprint where
  sizeOf _ = 16
  alignment _ = 8
  peek address = do
    high <- peekWord64 (castPtr address) 8 0
    low <- peekWord64 (castPtr address `plusPtr` 8) 8 0
    return (Fingerprint high low)
  poke address (Fingerprint high low) = do
    pokeWord64 (castPtr address) 8 high
    pokeWord64 (castPtr address `plusPtr` 8) 8 low

-- | Read a big-endian word of the given number of bytes onto an accumulator.
peekWord64 :: Ptr Word8 -> Int -> Word64 -> IO Word64
peekWord64 _ 0 accumulator = return accumulator
peekWord64 address count accumulator = do
  byte <- peek address
  peekWord64 (address `plusPtr` 1) (count - 1) ((accumulator `shiftL` 8) .|. fromIntegral byte)

-- | Write the given number of low bytes of a word in big-endian order.
pokeWord64 :: Ptr Word8 -> Int -> Word64 -> IO ()
pokeWord64 _ 0 _ = return ()
pokeWord64 address count value = do
  pokeElemOff address (count - 1) (fromIntegral value)
  pokeWord64 address (count - 1) (value `shiftR` 8)
