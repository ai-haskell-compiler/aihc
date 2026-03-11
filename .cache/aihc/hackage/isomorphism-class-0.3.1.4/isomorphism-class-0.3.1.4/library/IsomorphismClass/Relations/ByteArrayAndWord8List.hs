{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteArrayAndWord8List where

import qualified Data.Primitive.ByteArray
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import IsomorphismClass.Relations.ByteArrayAndShortByteString ()

instance IsomorphicTo Data.Primitive.ByteArray.ByteArray [Word8] where
  to = fromList

instance IsomorphicTo [Word8] Data.Primitive.ByteArray.ByteArray where
  to = toList
