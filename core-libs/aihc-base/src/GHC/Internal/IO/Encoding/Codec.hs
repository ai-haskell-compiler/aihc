-- | Building 'TextEncoding' values for encodings whose codecs aihc does
-- not run. The handle layer sends all text through UTF-8 and only carries
-- the name of an encoding, so the codec fields exist for libraries that
-- construct and pattern match on the record; asking one for a decoder or
-- an encoder reports an unsupported operation.
module GHC.Internal.IO.Encoding.Codec
  ( nameOnlyEncoding,
  )
where

import Data.Maybe (Maybe (..))
import GHC.Base (String, (++))
import GHC.IO (IO, throwIO)
import GHC.IO.Encoding.Types (BufferCodec, TextEncoding (..))
import GHC.Internal.IO.Types (IOErrorType (..), IOException (..))

-- | An encoding that is only its name.
nameOnlyEncoding :: String -> TextEncoding
nameOnlyEncoding name =
  TextEncoding
    { textEncodingName = name,
      mkTextDecoder = unsupportedCodec name,
      mkTextEncoder = unsupportedCodec name
    }

unsupportedCodec :: String -> IO (BufferCodec from to state)
unsupportedCodec name =
  throwIO
    IOError
      { ioe_handle = Nothing,
        ioe_type = UnsupportedOperation,
        ioe_location = "mkTextCodec",
        ioe_description = "aihc does not run the " ++ name ++ " codec; all text goes through UTF-8",
        ioe_errno = Nothing,
        ioe_filename = Nothing
      }
