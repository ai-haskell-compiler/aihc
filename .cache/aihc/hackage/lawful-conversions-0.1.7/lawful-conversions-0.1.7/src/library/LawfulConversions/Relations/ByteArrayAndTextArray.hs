{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteArrayAndTextArray where

#if !MIN_VERSION_text(2,1,0)

import qualified Data.Primitive.ByteArray
import qualified Data.Text.Array
import LawfulConversions.Classes
import LawfulConversions.Prelude
import qualified LawfulConversions.TextCompat.Array

instance IsSome Data.Primitive.ByteArray.ByteArray Data.Text.Array.Array where
  to = LawfulConversions.TextCompat.Array.toByteArray

instance IsSome Data.Text.Array.Array Data.Primitive.ByteArray.ByteArray where
  to = LawfulConversions.TextCompat.Array.fromByteArray

instance IsMany Data.Primitive.ByteArray.ByteArray Data.Text.Array.Array

instance IsMany Data.Text.Array.Array Data.Primitive.ByteArray.ByteArray

instance Is Data.Primitive.ByteArray.ByteArray Data.Text.Array.Array

instance Is Data.Text.Array.Array Data.Primitive.ByteArray.ByteArray

#endif
