module Text.Hex
  ( -- * Encoding and decoding
    encodeHex,
    decodeHex,
    lazilyEncodeHex,

    -- * Types
    Text,
    LazyText,
    ByteString,
    LazyByteString,

    -- * Type conversions
    lazyText,
    strictText,
    lazyByteString,
    strictByteString,
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Base16.Lazy qualified as LazyBase16
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Encoding qualified as LazyText
import Prelude (Maybe (..), const, either)

-- | Strict byte string
type ByteString = ByteString.ByteString

-- | Lazy byte string
type LazyByteString = LazyByteString.ByteString

-- | Strict text
type Text = Text.Text

-- | Lazy text
type LazyText = LazyText.Text

-- | Encodes a byte string as hexadecimal number represented in text
--
-- Each byte of the input is converted into two characters in the
-- resulting text.
--
-- >>> (encodeHex . ByteString.singleton) 192
-- "c0"
--
-- >>> (encodeHex . ByteString.singleton) 168
-- "a8"
--
-- >>> (encodeHex . ByteString.pack) [192, 168, 1, 2]
-- "c0a80102"
--
-- 'Text' produced by @encodeHex@ can be converted back to a
-- 'ByteString' using 'decodeHex'.
--
-- The lazy variant of @encodeHex@ is 'lazilyEncodeHex'.
encodeHex :: ByteString -> Text
encodeHex bs =
  Text.decodeUtf8 (Base16.encode bs)

-- | Decodes hexadecimal text as a byte string

-- If the text contains an even number of characters and consists
-- only of the digits @0@ through @9@ and letters @a@ through @f@,
-- then the result is a 'Just' value.
--
-- Unpacking the ByteString in the following examples allows for
-- prettier printing in the REPL.
--
-- >>> (fmap ByteString.unpack . decodeHex . Text.pack) "c0a80102"
-- Just [192,168,1,2]
--
-- If the text contains an odd number of characters, decoding fails
-- and produces 'Nothing'.
--
-- >>> (fmap ByteString.unpack . decodeHex . Text.pack) "c0a8010"
-- Nothing
--
-- If the text contains non-hexadecimal characters, decoding fails
-- and produces 'Nothing'.
--
-- >>> (fmap ByteString.unpack . decodeHex . Text.pack) "x0a80102"
-- Nothing
--
-- The letters may be in either upper or lower case. This next
-- example therefore gives the same result as the first one above:
--
-- >>> (fmap ByteString.unpack . decodeHex . Text.pack) "C0A80102"
-- Just [192,168,1,2]

decodeHex :: Text -> Maybe ByteString
decodeHex txt =
  either (const Nothing) Just (Base16.decode (Text.encodeUtf8 txt))

-- | The lazy variant of 'encodeHex'
--
-- With laziness, it is possible to encode byte strings of
-- infinite length:
--
-- >>> (LazyText.take 8 . lazilyEncodeHex . LazyByteString.pack . cycle) [1, 2, 3]
-- "01020301"
lazilyEncodeHex :: LazyByteString -> LazyText
lazilyEncodeHex bs =
  LazyText.decodeUtf8 (LazyBase16.encode bs)

lazyText :: Text -> LazyText
lazyText = LazyText.fromStrict

strictText :: LazyText -> Text
strictText = LazyText.toStrict

lazyByteString :: ByteString -> LazyByteString
lazyByteString = LazyByteString.fromStrict

strictByteString :: LazyByteString -> ByteString
strictByteString = LazyByteString.toStrict
