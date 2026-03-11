{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteStringAndShortByteString where

import qualified Data.ByteString.Short
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicTo ByteString Data.ByteString.Short.ShortByteString where
  to = Data.ByteString.Short.fromShort

instance IsomorphicTo Data.ByteString.Short.ShortByteString ByteString where
  to = Data.ByteString.Short.toShort
