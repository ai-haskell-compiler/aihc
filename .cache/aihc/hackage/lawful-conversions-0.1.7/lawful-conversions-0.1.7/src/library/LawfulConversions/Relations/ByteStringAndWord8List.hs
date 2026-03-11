{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteStringAndWord8List where

import qualified Data.ByteString
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome ByteString [Word8] where
  to = Data.ByteString.pack

instance IsSome [Word8] ByteString where
  to = Data.ByteString.unpack

instance IsMany ByteString [Word8]

instance IsMany [Word8] ByteString

instance Is ByteString [Word8]

instance Is [Word8] ByteString
