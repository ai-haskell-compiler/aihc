-- |
-- Module:     System.Directory.OsPath.SafeStreaming
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com
--
-- You’ll most likely be interested in either
--
-- * 'getDirectoryContentsRecursive' to search directory hierarchy recursively
-- * 'DirStream', 'openDirStream', 'readDirStream', and 'closeDirStream' to traverse single directory efficiently

module System.Directory.OsPath.Streaming
  ( DirStream
  , openDirStream
  , readDirStream
  , closeDirStream

  -- * File types
  , SymlinkType(..)
  , FileType(..)
  , Basename(..)
  , getFileType

  -- * Get directory contents
  , getDirectoryContentsRecursive
  , getDirectoryContentsWithFilterRecursive

  , listContentsRecFold

  -- * Utilities
  , regularFile
  , regularDirectory
  , regularOther
  , symlinkFile
  , symlinkDirectory
  , symlinkOther
  ) where

import System.Directory.OsPath.Contents
import System.Directory.OsPath.FileType
import System.Directory.OsPath.Streaming.Internal as Streaming
import System.Directory.OsPath.Types

