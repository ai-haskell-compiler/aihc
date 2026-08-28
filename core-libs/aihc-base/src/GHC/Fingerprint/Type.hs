{-# LANGUAGE MagicHash #-}

module GHC.Fingerprint.Type (Fingerprint (..)) where

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
import GHC.Word (Word64 (W64#))
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
