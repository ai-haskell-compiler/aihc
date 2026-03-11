-- |
-- Module: System.Directory.ProjectRoot
-- Description: Find the root of a project
-- Copyright: (c) Pedro Tacla Yamada, 2015
-- License: MIT
-- Maintainer: tacla.yamada@gmail.com
-- Stability: Stable
-- Portability: POSIX
--
-- Simple way of finding the root of a project given an entry-point.
-- This module provides bindings to the <https://github.com/yamadapc/projectroot projectroot> C library
module System.Directory.ProjectRoot
  where

import           Control.Applicative ((<$>))
import           Foreign.C           (CString, newCString, peekCString)
import           Foreign.Ptr         (nullPtr)
import           System.Directory    (getCurrentDirectory)

-- * Haskell Wrappers

-- |
-- Find the project root given an entry point
getProjectRoot :: FilePath -> IO (Maybe FilePath)
getProjectRoot fp = do
    root <- find_project_root =<< newCString fp
    if root == nullPtr
        then return Nothing
        else Just <$> peekCString root

-- |
-- Find the weighted project root given an entry point
getProjectRootWeighted :: FilePath -> IO (Maybe FilePath)
getProjectRootWeighted fp = do
    root <- find_project_root_weighted =<< newCString fp
    if root == nullPtr
        then return Nothing
        else Just <$> peekCString root

-- |
-- Find the project root of the current directory
getProjectRootCurrent :: IO (Maybe FilePath)
getProjectRootCurrent = getCurrentDirectory >>= getProjectRoot

-- |
-- Find the weighted project root of the current directory
getProjectRootWeightedCurrent :: IO (Maybe FilePath)
getProjectRootWeightedCurrent = getCurrentDirectory >>= getProjectRootWeighted

-- * C functions

foreign import ccall "find_project_root" find_project_root :: CString -> IO CString
foreign import ccall "find_project_root_weighted" find_project_root_weighted :: CString -> IO CString
