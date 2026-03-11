{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteArrayAndWord8List where

import qualified Data.Primitive.ByteArray
import LawfulConversions.Classes
import LawfulConversions.Prelude
import LawfulConversions.Relations.ByteArrayAndShortByteString ()

instance IsSome Data.Primitive.ByteArray.ByteArray [Word8] where
  to = fromList

instance IsSome [Word8] Data.Primitive.ByteArray.ByteArray where
  to = toList

instance IsMany Data.Primitive.ByteArray.ByteArray [Word8]

instance IsMany [Word8] Data.Primitive.ByteArray.ByteArray

instance Is Data.Primitive.ByteArray.ByteArray [Word8]

instance Is [Word8] Data.Primitive.ByteArray.ByteArray
