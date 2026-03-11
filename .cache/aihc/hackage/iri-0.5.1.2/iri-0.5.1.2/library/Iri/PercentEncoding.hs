module Iri.PercentEncoding where

import Iri.Prelude

{-# INLINE matchHexByte #-}
matchHexByte :: a -> (Word8 -> a) -> Word8 -> a
matchHexByte failure success x =
  if x >= 48 && x <= 57
    then success (x - 48)
    else
      if x >= 65 && x <= 70
        then success (x - 55)
        else
          if x >= 97 && x <= 102
            then success (x - 87)
            else failure

{-# INLINE matchPercentEncodedBytes #-}
matchPercentEncodedBytes :: a -> (Word8 -> a) -> Word8 -> Word8 -> a
matchPercentEncodedBytes failure success byte1 byte2 =
  matchHexByte failure firstByteSuccess byte1
  where
    firstByteSuccess decodedByte1 =
      matchHexByte failure secondByteSuccess byte2
      where
        secondByteSuccess decodedByte2 =
          success (shiftL decodedByte1 4 .|. decodedByte2)
