{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ShortByteStringAndWord8List where

import qualified Data.ByteString.Short
import LawfulConversions.Classes
import LawfulConversions.Prelude
import LawfulConversions.Relations.ByteArrayAndShortByteString ()

instance IsSome [Word8] Data.ByteString.Short.ShortByteString where
  to = Data.ByteString.Short.unpack

instance IsSome Data.ByteString.Short.ShortByteString [Word8] where
  to = Data.ByteString.Short.pack

instance IsMany [Word8] Data.ByteString.Short.ShortByteString

instance IsMany Data.ByteString.Short.ShortByteString [Word8]

instance Is [Word8] Data.ByteString.Short.ShortByteString

instance Is Data.ByteString.Short.ShortByteString [Word8]
