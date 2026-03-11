{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteArrayAndLazyByteStringBuilder where

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Short
import qualified Data.Primitive.ByteArray
import LawfulConversions.Classes
import LawfulConversions.Prelude
import LawfulConversions.Relations.ByteArrayAndShortByteString ()
import LawfulConversions.Relations.LazyByteStringBuilderAndShortByteString ()

instance IsSome Data.Primitive.ByteArray.ByteArray Data.ByteString.Builder.Builder where
  to = to . to @Data.ByteString.Short.ShortByteString

instance IsSome Data.ByteString.Builder.Builder Data.Primitive.ByteArray.ByteArray where
  to = to . to @Data.ByteString.Short.ShortByteString

instance IsMany Data.Primitive.ByteArray.ByteArray Data.ByteString.Builder.Builder

instance IsMany Data.ByteString.Builder.Builder Data.Primitive.ByteArray.ByteArray

instance Is Data.Primitive.ByteArray.ByteArray Data.ByteString.Builder.Builder

instance Is Data.ByteString.Builder.Builder Data.Primitive.ByteArray.ByteArray
