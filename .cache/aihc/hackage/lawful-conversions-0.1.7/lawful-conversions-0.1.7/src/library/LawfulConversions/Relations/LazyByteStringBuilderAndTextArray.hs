{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyByteStringBuilderAndTextArray where

#if !MIN_VERSION_text(2,1,0)

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Data.Text.Array
import LawfulConversions.Classes
import LawfulConversions.Prelude
import qualified LawfulConversions.TextCompat.Array

instance IsSome Data.ByteString.Builder.Builder Data.Text.Array.Array where
  to = Data.ByteString.Builder.shortByteString . LawfulConversions.TextCompat.Array.toShortByteString

instance IsSome Data.Text.Array.Array Data.ByteString.Builder.Builder where
  to =
    LawfulConversions.TextCompat.Array.fromShortByteString
      . Data.ByteString.Short.toShort
      . Data.ByteString.Lazy.toStrict
      . Data.ByteString.Builder.toLazyByteString

instance IsMany Data.Text.Array.Array Data.ByteString.Builder.Builder

instance IsMany Data.ByteString.Builder.Builder Data.Text.Array.Array

instance Is Data.Text.Array.Array Data.ByteString.Builder.Builder

instance Is Data.ByteString.Builder.Builder Data.Text.Array.Array

#endif
