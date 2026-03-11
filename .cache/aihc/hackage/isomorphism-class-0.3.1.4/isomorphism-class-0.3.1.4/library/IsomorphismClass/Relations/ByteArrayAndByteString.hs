{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteArrayAndByteString where

import qualified Data.ByteString.Short
import qualified Data.Primitive.ByteArray
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import IsomorphismClass.Relations.ByteArrayAndShortByteString ()
import IsomorphismClass.Relations.ByteStringAndShortByteString ()

instance IsomorphicTo Data.Primitive.ByteArray.ByteArray ByteString where
  to = to . to @Data.ByteString.Short.ShortByteString

instance IsomorphicTo ByteString Data.Primitive.ByteArray.ByteArray where
  to = to . to @Data.ByteString.Short.ShortByteString
