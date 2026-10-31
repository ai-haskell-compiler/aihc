module Backend (version, dataDirectory) where

import Data.Version (Version)
import qualified Happy.Paths as Paths

version :: Version
version = Paths.version

dataDirectory :: IO FilePath
dataDirectory = Paths.getDataDir
