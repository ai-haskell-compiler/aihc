{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteArrayAndByteString where

import qualified Data.ByteString.Short
import qualified Data.Primitive.ByteArray
import LawfulConversions.Classes
import LawfulConversions.Prelude
import LawfulConversions.Relations.ByteArrayAndShortByteString ()
import LawfulConversions.Relations.ByteStringAndShortByteString ()

instance IsSome Data.Primitive.ByteArray.ByteArray ByteString where
  to = to . to @Data.ByteString.Short.ShortByteString

instance IsSome ByteString Data.Primitive.ByteArray.ByteArray where
  to = to . to @Data.ByteString.Short.ShortByteString

instance IsMany Data.Primitive.ByteArray.ByteArray ByteString

instance IsMany ByteString Data.Primitive.ByteArray.ByteArray

instance Is Data.Primitive.ByteArray.ByteArray ByteString

instance Is ByteString Data.Primitive.ByteArray.ByteArray
