module GHC.IO.Encoding
  ( BufferCodec (..),
    TextEncoding (..),
    TextEncoder,
    TextDecoder,
    CodeBuffer,
    EncodeBuffer,
    DecodeBuffer,
    CodingProgress (..),
    latin1,
    latin1_checked,
    utf8,
    utf8_bom,
    utf16,
    utf16le,
    utf16be,
    utf32,
    utf32le,
    utf32be,
    char8,
    getLocaleEncoding,
    getFileSystemEncoding,
    getForeignEncoding,
    setLocaleEncoding,
    setFileSystemEncoding,
    setForeignEncoding,
    initLocaleEncoding,
    mkTextEncoding,
    argvEncoding,
  )
where

import GHC.Base (Applicative (..), String)
import GHC.IO (IO)
import GHC.IO.Encoding.Latin1 (latin1, latin1_checked)
import GHC.IO.Encoding.Types
  ( BufferCodec (..),
    CodeBuffer,
    CodingProgress (..),
    DecodeBuffer,
    EncodeBuffer,
    TextDecoder,
    TextEncoder,
    TextEncoding (..),
  )
import GHC.IO.Encoding.UTF16 (utf16, utf16be, utf16le)
import GHC.IO.Encoding.UTF32 (utf32, utf32be, utf32le)
import GHC.IO.Encoding.UTF8 (utf8, utf8_bom)
import GHC.Internal.IO.Encoding.Codec (nameOnlyEncoding)

char8 :: TextEncoding
char8 = latin1

getLocaleEncoding :: IO TextEncoding
getLocaleEncoding = pure utf8

getFileSystemEncoding :: IO TextEncoding
getFileSystemEncoding = pure utf8

getForeignEncoding :: IO TextEncoding
getForeignEncoding = pure utf8

setLocaleEncoding :: TextEncoding -> IO ()
setLocaleEncoding _ = pure ()

setFileSystemEncoding :: TextEncoding -> IO ()
setFileSystemEncoding _ = pure ()

setForeignEncoding :: TextEncoding -> IO ()
setForeignEncoding _ = pure ()

-- | The locale encoding at program start. The runtime only has UTF-8.
initLocaleEncoding :: TextEncoding
initLocaleEncoding = utf8

-- | The encoding of the program arguments.
argvEncoding :: IO TextEncoding
argvEncoding = pure utf8

-- | The runtime only has UTF-8, so an encoding built by name carries that
-- name and nothing else.
mkTextEncoding :: String -> IO TextEncoding
mkTextEncoding name = pure (nameOnlyEncoding name)
