{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteArrayAndTextArray where

#if !MIN_VERSION_text(2,1,0)

import qualified Data.Primitive.ByteArray
import qualified Data.Text.Array
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import qualified IsomorphismClass.TextCompat.Array

instance IsomorphicTo Data.Primitive.ByteArray.ByteArray Data.Text.Array.Array where
  to = IsomorphismClass.TextCompat.Array.toByteArray

instance IsomorphicTo Data.Text.Array.Array Data.Primitive.ByteArray.ByteArray where
  to = IsomorphismClass.TextCompat.Array.fromByteArray

#endif
