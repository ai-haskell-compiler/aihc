{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyByteStringBuilderAndShortByteString where

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome Data.ByteString.Builder.Builder Data.ByteString.Short.ShortByteString where
  to = Data.ByteString.Builder.shortByteString

instance IsSome Data.ByteString.Short.ShortByteString Data.ByteString.Builder.Builder where
  to = Data.ByteString.Short.toShort . Data.ByteString.Lazy.toStrict . Data.ByteString.Builder.toLazyByteString
  maybeFrom = Just . Data.ByteString.Builder.shortByteString

instance IsMany Data.ByteString.Builder.Builder Data.ByteString.Short.ShortByteString

instance IsMany Data.ByteString.Short.ShortByteString Data.ByteString.Builder.Builder

instance Is Data.ByteString.Builder.Builder Data.ByteString.Short.ShortByteString

instance Is Data.ByteString.Short.ShortByteString Data.ByteString.Builder.Builder
