-- | String marshalling through text encodings.
--
-- A 'TextEncoding' here is a name only, so this chooses between the two
-- codecs "Foreign.C.String" has: UTF-8 for the UTF-8 encodings and one byte
-- per character for every other.
module GHC.Foreign
  ( peekCString,
    peekCStringLen,
    newCString,
    newCStringLen,
    withCString,
    withCStringLen,
  )
where

import Foreign.C.String (CString, CStringLen)
import Foreign.C.String qualified as C
import GHC.Base (String)
import GHC.IO (IO)
import GHC.IO.Encoding (TextEncoding, textEncodingName)
import Prelude (Bool, Eq (..), otherwise, (||))

peekCString :: TextEncoding -> CString -> IO String
peekCString encoding
  | isUtf8 encoding = C.peekCString
  | otherwise = C.peekCAString

peekCStringLen :: TextEncoding -> CStringLen -> IO String
peekCStringLen encoding
  | isUtf8 encoding = C.peekCStringLen
  | otherwise = C.peekCAStringLen

newCString :: TextEncoding -> String -> IO CString
newCString encoding
  | isUtf8 encoding = C.newCString
  | otherwise = C.newCAString

newCStringLen :: TextEncoding -> String -> IO CStringLen
newCStringLen encoding
  | isUtf8 encoding = C.newCStringLen
  | otherwise = C.newCAStringLen

withCString :: TextEncoding -> String -> (CString -> IO a) -> IO a
withCString encoding
  | isUtf8 encoding = C.withCString
  | otherwise = C.withCAString

withCStringLen :: TextEncoding -> String -> (CStringLen -> IO a) -> IO a
withCStringLen encoding
  | isUtf8 encoding = C.withCStringLen
  | otherwise = C.withCAStringLen

isUtf8 :: TextEncoding -> Bool
isUtf8 encoding =
  let name = textEncodingName encoding
   in name == "UTF-8" || name == "UTF-8BOM" || name == "UTF8"
