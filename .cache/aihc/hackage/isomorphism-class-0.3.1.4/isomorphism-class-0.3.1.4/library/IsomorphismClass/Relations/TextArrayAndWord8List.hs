{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.TextArrayAndWord8List where

#if !MIN_VERSION_text(2,1,0)

import qualified Data.ByteString.Short
import qualified Data.Text.Array
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import qualified IsomorphismClass.TextCompat.Array

instance IsomorphicTo Data.Text.Array.Array [Word8] where
  to = IsomorphismClass.TextCompat.Array.fromShortByteString . Data.ByteString.Short.pack

instance IsomorphicTo [Word8] Data.Text.Array.Array where
  to = Data.ByteString.Short.unpack . IsomorphismClass.TextCompat.Array.toShortByteString

#endif
