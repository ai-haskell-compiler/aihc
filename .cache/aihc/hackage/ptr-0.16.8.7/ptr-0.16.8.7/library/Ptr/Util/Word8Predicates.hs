module Ptr.Util.Word8Predicates where

import Ptr.Prelude

asciiUpperLetter :: Word8 -> Bool
asciiUpperLetter x = x - 65 <= 25

asciiDigit :: Word8 -> Bool
asciiDigit x = x - 48 <= 9
