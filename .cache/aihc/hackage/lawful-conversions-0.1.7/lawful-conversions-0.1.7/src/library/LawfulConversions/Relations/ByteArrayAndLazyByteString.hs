{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteArrayAndLazyByteString where

import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Data.Primitive.ByteArray
import LawfulConversions.Classes
import LawfulConversions.Prelude
import LawfulConversions.Relations.ByteArrayAndShortByteString ()
import LawfulConversions.Relations.LazyByteStringAndShortByteString ()

instance IsSome Data.ByteString.Lazy.ByteString Data.Primitive.ByteArray.ByteArray where
  to = to . to @Data.ByteString.Short.ShortByteString

instance IsSome Data.Primitive.ByteArray.ByteArray Data.ByteString.Lazy.ByteString where
  to = to . to @Data.ByteString.Short.ShortByteString

instance IsMany Data.Primitive.ByteArray.ByteArray Data.ByteString.Lazy.ByteString

instance IsMany Data.ByteString.Lazy.ByteString Data.Primitive.ByteArray.ByteArray

instance Is Data.Primitive.ByteArray.ByteArray Data.ByteString.Lazy.ByteString

instance Is Data.ByteString.Lazy.ByteString Data.Primitive.ByteArray.ByteArray
