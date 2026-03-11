{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyByteStringAndLazyByteStringBuilder where

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import LawfulConversions.Classes

instance IsSome Data.ByteString.Lazy.ByteString Data.ByteString.Builder.Builder where
  to = Data.ByteString.Builder.toLazyByteString

instance IsSome Data.ByteString.Builder.Builder Data.ByteString.Lazy.ByteString where
  to = Data.ByteString.Builder.lazyByteString

instance IsMany Data.ByteString.Lazy.ByteString Data.ByteString.Builder.Builder

instance IsMany Data.ByteString.Builder.Builder Data.ByteString.Lazy.ByteString

instance Is Data.ByteString.Lazy.ByteString Data.ByteString.Builder.Builder

instance Is Data.ByteString.Builder.Builder Data.ByteString.Lazy.ByteString
