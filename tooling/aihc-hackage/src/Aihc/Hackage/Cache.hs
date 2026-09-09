-- | XDG cache layout for downloaded Hackage packages.
module Aihc.Hackage.Cache
  ( getHackageCacheDir,
  )
where

import System.Directory
  ( XdgDirectory (XdgCache),
    getXdgDirectory,
  )
import System.FilePath ((</>))

-- | XDG cache directory for downloaded Hackage packages.
--
-- @~\/.cache\/aihc\/hackage@
getHackageCacheDir :: IO FilePath
getHackageCacheDir = do
  cacheBase <- getXdgDirectory XdgCache "aihc"
  pure (cacheBase </> "hackage")
