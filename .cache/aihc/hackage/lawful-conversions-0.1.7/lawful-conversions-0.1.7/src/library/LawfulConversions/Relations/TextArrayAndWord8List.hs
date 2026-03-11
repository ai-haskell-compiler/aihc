{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.TextArrayAndWord8List where

#if !MIN_VERSION_text(2,1,0)

import qualified Data.ByteString.Short
import qualified Data.Text.Array
import LawfulConversions.Classes
import LawfulConversions.Prelude
import qualified LawfulConversions.TextCompat.Array

instance IsSome Data.Text.Array.Array [Word8] where
  to = LawfulConversions.TextCompat.Array.fromShortByteString . Data.ByteString.Short.pack

instance IsSome [Word8] Data.Text.Array.Array where
  to = Data.ByteString.Short.unpack . LawfulConversions.TextCompat.Array.toShortByteString

instance IsMany Data.Text.Array.Array [Word8]

instance IsMany [Word8] Data.Text.Array.Array

instance Is Data.Text.Array.Array [Word8]

instance Is [Word8] Data.Text.Array.Array

#endif
