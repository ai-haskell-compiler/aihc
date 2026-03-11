{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyByteStringAndTextArray where

#if !MIN_VERSION_text(2,1,0)

import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Data.Text.Array
import LawfulConversions.Classes
import LawfulConversions.Prelude
import qualified LawfulConversions.TextCompat.Array

instance IsSome Data.ByteString.Lazy.ByteString Data.Text.Array.Array where
  to =
    Data.ByteString.Lazy.fromStrict
      . Data.ByteString.Short.fromShort
      . LawfulConversions.TextCompat.Array.toShortByteString

instance IsSome Data.Text.Array.Array Data.ByteString.Lazy.ByteString where
  to =
    LawfulConversions.TextCompat.Array.fromShortByteString
      . Data.ByteString.Short.toShort
      . Data.ByteString.Lazy.toStrict

instance IsMany Data.Text.Array.Array Data.ByteString.Lazy.ByteString

instance IsMany Data.ByteString.Lazy.ByteString Data.Text.Array.Array

instance Is Data.Text.Array.Array Data.ByteString.Lazy.ByteString

instance Is Data.ByteString.Lazy.ByteString Data.Text.Array.Array

#endif
