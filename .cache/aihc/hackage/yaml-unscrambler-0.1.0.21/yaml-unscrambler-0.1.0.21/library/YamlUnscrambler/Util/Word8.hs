module YamlUnscrambler.Util.Word8 where

import YamlUnscrambler.Prelude

lowercaseInAscii :: Word8 -> Word8
lowercaseInAscii a =
  if 65 <= a && a <= 90 || 192 <= a && a <= 214 || 216 <= a && a <= 222
    then a + 32
    else a
