{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteStringAndShortByteString where

import qualified Data.ByteString.Short
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome ByteString Data.ByteString.Short.ShortByteString where
  to = Data.ByteString.Short.fromShort

instance IsSome Data.ByteString.Short.ShortByteString ByteString where
  to = Data.ByteString.Short.toShort

instance IsMany ByteString Data.ByteString.Short.ShortByteString

instance IsMany Data.ByteString.Short.ShortByteString ByteString

instance Is ByteString Data.ByteString.Short.ShortByteString

instance Is Data.ByteString.Short.ShortByteString ByteString
