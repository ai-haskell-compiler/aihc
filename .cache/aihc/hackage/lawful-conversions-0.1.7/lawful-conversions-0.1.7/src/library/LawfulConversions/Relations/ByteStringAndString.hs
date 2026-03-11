{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteStringAndString where

import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import LawfulConversions.Classes
import LawfulConversions.Prelude

-- | UTF-8 codec.
instance IsSome ByteString String where
  to = Data.Text.Encoding.encodeUtf8 . Data.Text.pack
  maybeFrom = either (const Nothing) (Just . Data.Text.unpack) . Data.Text.Encoding.decodeUtf8'

-- | Lenient UTF-8 decoding.
instance IsMany ByteString String where
  from = Data.Text.unpack . Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode
