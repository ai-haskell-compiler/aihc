-- | High-level functions throw 'BZError' on error.
--
-- @since 0.1.1.0
module Codec.Compression.BZip ( -- * High-level functions.
                                compress
                              , compressWith
                              , decompress
                              , decompressErr
                              -- * Errors
                              , BZError (..)
                              -- * Miscellany
                              , bZ2BzlibVersion
                              ) where

import           Codec.Compression.BZip.Foreign.Common
import           Codec.Compression.BZip.Pack
import           Codec.Compression.BZip.Unpack
