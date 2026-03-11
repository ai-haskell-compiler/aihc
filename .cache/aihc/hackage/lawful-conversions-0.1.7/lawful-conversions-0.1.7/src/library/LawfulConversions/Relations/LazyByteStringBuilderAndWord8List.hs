{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyByteStringBuilderAndWord8List where

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome Data.ByteString.Builder.Builder [Word8] where
  to = Data.ByteString.Builder.lazyByteString . Data.ByteString.Lazy.pack

instance IsSome [Word8] Data.ByteString.Builder.Builder where
  to = Data.ByteString.Lazy.unpack . Data.ByteString.Builder.toLazyByteString

instance IsMany Data.ByteString.Builder.Builder [Word8]

instance IsMany [Word8] Data.ByteString.Builder.Builder

instance Is Data.ByteString.Builder.Builder [Word8]

instance Is [Word8] Data.ByteString.Builder.Builder
