{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ShortByteStringAndTextArray where

#if !MIN_VERSION_text(2,1,0)

import qualified Data.ByteString.Short
import qualified Data.Text.Array
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import qualified IsomorphismClass.TextCompat.Array

instance IsomorphicTo Data.ByteString.Short.ShortByteString Data.Text.Array.Array where
  to = IsomorphismClass.TextCompat.Array.toShortByteString

instance IsomorphicTo Data.Text.Array.Array Data.ByteString.Short.ShortByteString where
  to = IsomorphismClass.TextCompat.Array.fromShortByteString

#endif
