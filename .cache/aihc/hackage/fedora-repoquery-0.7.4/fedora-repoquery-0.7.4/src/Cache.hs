module Cache
  (cacheSize,
   cleanEmptyCaches)
where

import Control.Monad
import Data.List.Extra
import SimpleCmd
import System.Directory
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.FilePath
import System.FilePath.Glob

cacheDirs :: IO [FilePath]
cacheDirs = do
  dnf5cache <- getUserCacheDir "libdnf5"
  exists <- doesDirectoryExist dnf5cache
  olddirs <- glob "/var/tmp/dnf-*"
  return $
    (if exists then (dnf5cache :) else id) olddirs

cacheSize :: IO ()
cacheSize = do
  caches <- cacheDirs
  forM_ caches $ \cache ->
    cmd_ "du" ["-sh", cache]

-- FIXME offer deleting old repos
cleanEmptyCaches :: IO ()
cleanEmptyCaches = do
  caches <- cacheDirs
  forM_ caches $ \cache ->
    withCurrentDirectory cache $ do
    repocaches <- groupOn cachePrefix .
                  filter (\ f -> '.' `notElem` f && '-' `elem` f) <$>
                  listDirectory "."
    mapM_ removeEmptyCaches repocaches
  where
    removeEmptyCaches :: [FilePath] -> IO ()
    removeEmptyCaches [] = return ()
    removeEmptyCaches (r:rs) = do
      let repodata = r </> "repodata"
      exists <- doesDirectoryExist repodata
      if exists
        then do
        files <- listDirectory repodata
        when (null files) $ do
          putStrLn $ "removing " ++ r
          removeDirectory repodata
          removeDirectory r
        else do
        files <- listDirectory r
        when (null files) $ do
          putStrLn ("removing " ++ r)
          removeDirectory r
      removeEmptyCaches rs

-- "AppStream-8-312ff1df17be2171" -> "AppStream-8-"
cachePrefix :: FilePath -> String
cachePrefix = dropWhileEnd (/= '-')
