{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyByteStringAndWord8List where

import qualified Data.ByteString.Lazy
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome Data.ByteString.Lazy.ByteString [Word8] where
  to = Data.ByteString.Lazy.pack

instance IsSome [Word8] Data.ByteString.Lazy.ByteString where
  to = Data.ByteString.Lazy.unpack

instance IsMany Data.ByteString.Lazy.ByteString [Word8]

instance IsMany [Word8] Data.ByteString.Lazy.ByteString

instance Is Data.ByteString.Lazy.ByteString [Word8]

instance Is [Word8] Data.ByteString.Lazy.ByteString
