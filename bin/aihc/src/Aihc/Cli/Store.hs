-- | Filesystem layout shared by the installed libraries and the compiler
-- that consumes them.
module Aihc.Cli.Store
  ( defaultStoreRoot,
  )
where

import System.Directory (XdgDirectory (XdgCache), getXdgDirectory)
import System.FilePath ((</>))

defaultStoreRoot :: IO FilePath
defaultStoreRoot = do
  cacheDirectory <- getXdgDirectory XdgCache "aihc"
  pure (cacheDirectory </> "store")
