-- | XDG cache layout for downloaded Hackage packages and Stackage snapshots.
module Aihc.Hackage.Cache
  ( getHackageCacheDir,
    getStackageCacheDir,
    snapshotCacheFile,
    sanitizeName,
  )
where

import Data.Char (isAlphaNum)
import System.Directory
  ( XdgDirectory (XdgCache),
    createDirectoryIfMissing,
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

-- | XDG cache directory for Stackage snapshot data.
--
-- @~\/.cache\/aihc\/stackage@
getStackageCacheDir :: IO FilePath
getStackageCacheDir = do
  cacheBase <- getXdgDirectory XdgCache "aihc"
  let dir = cacheBase </> "stackage"
  createDirectoryIfMissing True dir
  pure dir

-- | Get the cache file path for a snapshot.
snapshotCacheFile :: String -> IO FilePath
snapshotCacheFile snapshot = do
  dir <- getStackageCacheDir
  let file = sanitizeName snapshot ++ "-cabal.config"
  pure (dir </> file)

-- | Sanitize a string for use as a filename.
sanitizeName :: String -> String
sanitizeName = map sanitizeChar
  where
    sanitizeChar c
      | isAlphaNum c || c == '-' || c == '_' = c
      | otherwise = '_'
