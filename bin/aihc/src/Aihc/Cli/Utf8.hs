-- | Locale-independent UTF-8 file input for Haskell source files.
module Aihc.Cli.Utf8 (readFile) where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Prelude hiding (readFile)

-- | Read a file as strict UTF-8, independently of the process locale.
readFile :: FilePath -> IO Text
readFile path = Text.decodeUtf8 <$> BS.readFile path
