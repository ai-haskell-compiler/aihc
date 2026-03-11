{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteStringAndLazyByteStringBuilder where

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome ByteString Data.ByteString.Builder.Builder where
  to = Data.ByteString.Lazy.toStrict . Data.ByteString.Builder.toLazyByteString

instance IsSome Data.ByteString.Builder.Builder ByteString where
  to = Data.ByteString.Builder.byteString

instance IsMany Data.ByteString.Builder.Builder ByteString

instance IsMany ByteString Data.ByteString.Builder.Builder

instance Is Data.ByteString.Builder.Builder ByteString

instance Is ByteString Data.ByteString.Builder.Builder
