{-# LANGUAGE CPP #-}
{- |
Unicode aware URI encoding and decoding functions for both String and Text.
Although standards are pretty vague about Unicode in URIs most browsers are
pretty straightforward when encoding URIs: Encode a text to a UTF-8 string and
URI-encode every individual byte (not character).
-}

module Network.URI.Encode
  ( encode
  , encodeWith
  , encodeText
  , encodeTextWith
  , encodeTextToBS
  , encodeTextToBSWith
  , encodeByteString
  , encodeByteStringWith

  , decode
  , decodeText
  , decodeBSToText
  , decodeByteString

  , isAllowed
  ) where

import Data.Text
import Network.URI
import qualified Data.ByteString.Char8 as U
import qualified Data.ByteString.UTF8  as U

-------------------------------------------------------------------------------
-- | URI encode a 'String', unicode aware.

encode :: String -> String
encode = encodeWith isAllowed

-- | URI encode a 'String', unicode aware, using the predicate to
-- decide which characters are escaped ('False' means escape).

encodeWith :: (Char -> Bool) -> String -> String
encodeWith predicate = escapeURIString predicate . fixUtf8

-- | URI decode a 'String', unicode aware.

decode :: String -> String
decode = unfixUtf8 . unEscapeString

-------------------------------------------------------------------------------
-- | URI encode a 'Text', unicode aware.

encodeText :: Text -> Text
encodeText = pack . encode . unpack

-- | URI encode a 'Text', unicode aware, using the predicate to
-- decide which characters are escaped ('False' means escape).

encodeTextWith :: (Char -> Bool) -> Text -> Text
encodeTextWith predicate = pack . encodeWith predicate . unpack

-- | URI decode a 'Text', unicode aware.

decodeText :: Text -> Text
decodeText = pack . decode . unpack

-------------------------------------------------------------------------------
-- | URI encode a 'Text' into a 'ByteString', unicode aware.

encodeTextToBS :: Text -> U.ByteString
encodeTextToBS = U.pack . encode . unpack

-- | URI encode a 'Text' into a 'ByteString', unicode aware, using the
-- predicate to decide which characters are escaped ('False' means escape).

encodeTextToBSWith :: (Char -> Bool) -> Text -> U.ByteString
encodeTextToBSWith predicate = U.pack . encodeWith predicate . unpack

-- | URI decode a 'ByteString' into a 'Text', unicode aware.

decodeBSToText :: U.ByteString -> Text
decodeBSToText = pack . decode . U.unpack

-------------------------------------------------------------------------------
-- | URI encode a UTF8-encoded 'ByteString' into a 'ByteString', unicode aware.

encodeByteString :: U.ByteString -> U.ByteString
encodeByteString = U.pack . encode . U.toString

-- | URI encode a UTF8-encoded 'ByteString into a 'ByteString', unicode aware,
-- using the predicate to decide which characters are escaped ('False' means
-- escape).

encodeByteStringWith :: (Char -> Bool) -> U.ByteString -> U.ByteString
encodeByteStringWith predicate = U.pack . encodeWith predicate . U.unpack

-- | URI decode a 'ByteString' into a UTF8-encoded 'ByteString', unicode aware.

decodeByteString :: U.ByteString -> U.ByteString
decodeByteString = U.fromString . decode . U.unpack

-------------------------------------------------------------------------------
-- | Is a character allowed in a URI. Only ASCII alphabetic
-- characters, decimal digits, and - _ . ~ are allowed. This is
-- following RFC 3986.

isAllowed :: Char -> Bool
isAllowed = isUnreserved

-------------------------------------------------------------------------------
-- | "Fix" a String before encoding. This actually breaks the string,
-- by changing unicode characters into their byte pairs. For network
-- \>= 2.4, this is the identity, since that correctly handles unicode
-- characters.
fixUtf8 :: String -> String
#ifdef MIN_VERSION_network
#if MIN_VERSION_network(2,4,0)
fixUtf8 = id
#else
fixUtf8 = U.unpack . U.fromString
#endif
#else
fixUtf8 = id
#endif

-- | "Unfix" a String again. For network \>= 2.4.1.1 this is the
-- identity, since that correctly handles unicode characters. Note
-- that network 2.4.1.0 (one that is still broken) cannot be excluded
-- by CPP, so this version is excluded in the cabal dependencies.
unfixUtf8 :: String -> String
#ifdef MIN_VERSION_network
#if MIN_VERSION_network(2,4,1)
unfixUtf8 = id
#else
unfixUtf8 = U.toString . U.pack
#endif
#else
unfixUtf8 = id
#endif
