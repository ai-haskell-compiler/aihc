{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ShortByteStringAndTextArray where

#if !MIN_VERSION_text(2,1,0)

import qualified Data.ByteString.Short
import qualified Data.Text.Array
import LawfulConversions.Classes
import LawfulConversions.Prelude
import qualified LawfulConversions.TextCompat.Array

instance IsSome Data.ByteString.Short.ShortByteString Data.Text.Array.Array where
  to = LawfulConversions.TextCompat.Array.toShortByteString

instance IsSome Data.Text.Array.Array Data.ByteString.Short.ShortByteString where
  to = LawfulConversions.TextCompat.Array.fromShortByteString

instance IsMany Data.ByteString.Short.ShortByteString Data.Text.Array.Array

instance IsMany Data.Text.Array.Array Data.ByteString.Short.ShortByteString

instance Is Data.ByteString.Short.ShortByteString Data.Text.Array.Array

instance Is Data.Text.Array.Array Data.ByteString.Short.ShortByteString

#endif
