{-# LANGUAGE MultiWayIf #-}

{-|
Module      : Data.VCS.Ignore.FileSystem
Description : Helper functions for working with file system
Copyright   : (c) 2020-2022 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module contains mainly helper functions, that are internally used by this
library.
-}

module Data.VCS.Ignore.FileSystem
  ( findPaths
  , listPaths
  , walkPaths
  , toPosixPath
  )
where


import           Control.Monad                  ( forM
                                                , mfilter
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import           System.Directory               ( doesDirectoryExist
                                                , doesFileExist
                                                , getDirectoryContents
                                                )
import           System.FilePath                ( (</>) )


-- | Recursively finds paths on given path whose filename matches the predicate.
findPaths :: MonadIO m
          => FilePath             -- ^ path to traverse
          -> (FilePath -> m Bool) -- ^ predicate to match filename
          -> m [FilePath]         -- ^ list of found paths
findPaths entryPath predicate = catMaybes <$> walkPaths entryPath process
 where
  process path = (\p -> if p then Just path else Nothing) <$> predicate path


-- | Recursively finds all paths on given path. If file reference is passed
-- instead of directory, such path is returned.
listPaths :: MonadIO m
          => FilePath      -- ^ path to traverse
          -> m [FilePath]  -- ^ list of found paths
listPaths entryPath = walkPaths entryPath pure


-- | Recursively walks the given path and performs selected action for each
-- found file. Output of this function is:
--
--   * If the given __path is file__, only this single path is processed and
--     returned.
--   * If the given __path is directory__, all subdirectories and files are
--     recursively processed and returned.
--   * If the given __path doesn't exist__, empy list will be returned.
walkPaths :: MonadIO m
          => FilePath          -- ^ path to traverse
          -> (FilePath -> m a) -- ^ function to process path
          -> m [a]             -- ^ result of traversed & processed paths
walkPaths entryPath fn = do
  isDir  <- liftIO $ doesDirectoryExist entryPath
  isFile <- liftIO $ doesFileExist entryPath
  if
    | isDir     -> fn entryPath >>= (\p -> (p :) <$> listDirectory entryPath)
    | isFile    -> pure <$> fn entryPath
    | otherwise -> pure []
 where
  listDirectory dir = do
    names <- liftIO $ getDirectoryContents dir
    paths <- forM (filter (`notElem` [".", ".."]) names) $ \name -> do
      let path = dir </> name
      isDirectory <- liftIO $ doesDirectoryExist path
      if isDirectory then walkPaths path fn else pure <$> fn path
    pure $ concat paths


-- | If the given path contains backward slashes (Windows style), converts them
-- into forward ones (Unix style).
--
-- >>> toPosixPath "foo\\bar\\x.txt"
-- "foo/bar/x.txt"
--
-- >>> toPosixPath "foo/bar/x.txt"
-- "foo/bar/x.txt"
toPosixPath :: FilePath -- ^ input filepath to convert
            -> FilePath -- ^ output filepath
toPosixPath = replace '\\' '/'
  where replace a b = fmap $ fromMaybe b . mfilter (/= a) . Just
