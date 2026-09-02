module GHC.IO.Encoding
  ( TextEncoding,
    textEncodingName,
    utf8,
    utf8_bom,
    latin1,
    char8,
    getLocaleEncoding,
    getFileSystemEncoding,
    getForeignEncoding,
    setLocaleEncoding,
    setFileSystemEncoding,
    setForeignEncoding,
    mkTextEncoding,
  )
where

import Prelude (IO, String, pure)

-- | Text encodings carry only a name. All handles use UTF-8.
newtype TextEncoding = TextEncoding {textEncodingName :: String}

utf8 :: TextEncoding
utf8 = TextEncoding "UTF-8"

utf8_bom :: TextEncoding
utf8_bom = TextEncoding "UTF-8BOM"

latin1 :: TextEncoding
latin1 = TextEncoding "ISO-8859-1"

char8 :: TextEncoding
char8 = TextEncoding "char8"

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

mkTextEncoding :: String -> IO TextEncoding
mkTextEncoding name = pure (TextEncoding name)
