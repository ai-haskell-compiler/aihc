module System.CloudFoundry.Environment.Internal.VcapServicesDecoder
  ( decode
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LazyByteString

import System.CloudFoundry.Environment.Internal.Types

decode :: String -> Either String Services
decode = fmap Services . Aeson.eitherDecode . LazyByteString.pack
