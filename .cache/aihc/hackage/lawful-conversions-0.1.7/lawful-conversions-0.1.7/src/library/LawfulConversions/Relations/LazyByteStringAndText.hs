{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyByteStringAndText where

import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Data.Text.Encoding.Error
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import LawfulConversions.Classes
import LawfulConversions.Prelude

-- | UTF-8 codec.
instance IsSome Data.ByteString.Lazy.ByteString Data.Text.Text where
  to = Data.Text.Lazy.Encoding.encodeUtf8 . Data.Text.Lazy.fromStrict
  maybeFrom = either (const Nothing) (Just . Data.Text.Lazy.toStrict) . Data.Text.Lazy.Encoding.decodeUtf8'

-- | Lenient UTF-8 decoding.
instance IsMany Data.ByteString.Lazy.ByteString Data.Text.Text where
  from = Data.Text.Lazy.toStrict . Data.Text.Lazy.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode
