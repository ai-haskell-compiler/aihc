module YamlUnscrambler.Util.ByteString where

import Data.ByteString
import YamlUnscrambler.Prelude hiding (length, map)
import qualified YamlUnscrambler.Util.Word8 as Word8

lowercaseInAscii :: ByteString -> ByteString
lowercaseInAscii =
  map Word8.lowercaseInAscii

{-# NOINLINE lowercaseNullInAscii #-}
lowercaseNullInAscii :: ByteString
lowercaseNullInAscii =
  "null"

saysNullInCiAscii :: ByteString -> Bool
saysNullInCiAscii a =
  length a == 4 && lowercaseInAscii a == lowercaseNullInAscii
