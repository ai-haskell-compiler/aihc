{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyByteStringAndShortByteString where

import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome Data.ByteString.Lazy.ByteString Data.ByteString.Short.ShortByteString where
  to = Data.ByteString.Lazy.fromStrict . Data.ByteString.Short.fromShort

instance IsSome Data.ByteString.Short.ShortByteString Data.ByteString.Lazy.ByteString where
  to = Data.ByteString.Short.toShort . Data.ByteString.Lazy.toStrict

instance IsMany Data.ByteString.Lazy.ByteString Data.ByteString.Short.ShortByteString

instance IsMany Data.ByteString.Short.ShortByteString Data.ByteString.Lazy.ByteString

instance Is Data.ByteString.Lazy.ByteString Data.ByteString.Short.ShortByteString

instance Is Data.ByteString.Short.ShortByteString Data.ByteString.Lazy.ByteString
