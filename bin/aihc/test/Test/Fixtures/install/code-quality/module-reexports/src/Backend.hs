module Backend (version, dataDirectory) where

import Happy.Version (Version)
import qualified Happy.Paths as Paths

version :: Version
version = Paths.version

dataDirectory :: IO FilePath
dataDirectory = Paths.getDataDir
