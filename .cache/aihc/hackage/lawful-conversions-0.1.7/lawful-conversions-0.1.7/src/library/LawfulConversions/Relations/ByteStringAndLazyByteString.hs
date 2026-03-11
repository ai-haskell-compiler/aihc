{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteStringAndLazyByteString where

import qualified Data.ByteString.Lazy
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome ByteString Data.ByteString.Lazy.ByteString where
  to = Data.ByteString.Lazy.toStrict

instance IsSome Data.ByteString.Lazy.ByteString ByteString where
  to = Data.ByteString.Lazy.fromStrict

instance IsMany ByteString Data.ByteString.Lazy.ByteString

instance IsMany Data.ByteString.Lazy.ByteString ByteString

instance Is ByteString Data.ByteString.Lazy.ByteString

instance Is Data.ByteString.Lazy.ByteString ByteString
