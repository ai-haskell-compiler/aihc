{-|
Module      : Data.VCS.Ignore
Description : Reexported modules for convenience
Copyright   : (c) 2020-2022 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

@vcs-ignore@ is small Haskell library used to find, check and process files
ignored by selected /VCS/.

= Example of Use
Because this library is really simple to use, following example should be
enough to understand how to use it for your project.

== Listing all files/directories ignored by VCS
@
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Test where

import Data.VCS.Ignore ( Git, Repo(..), listRepo )

example :: IO [FilePath]
example = do
  repo <- scanRepo @Git "path/to/repo"
  listRepo repo
@

== Walking files/directories ignored by VCS
@
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Test where

import Data.Maybe       ( catMaybes )
import System.Directory ( doesFileExist )
import Data.VCS.Ignore  ( Git, Repo(..), walkRepo )

onlyFiles :: IO [FilePath]
onlyFiles = do
  repo <- scanRepo @Git "path/to/repo"
  catMaybes <$> walkRepo repo walkFn
 where
  walkFn path = do
    file <- doesFileExist path
    pure (if file then Just path else Nothing)
@

== Checking if path is ignored by VCS
@
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Test where

import Data.VCS.Ignore ( Git, Repo(..) )

checkIgnored :: IO Bool
checkIgnored = do
  repo <- scanRepo @Git "path/to/repo"
  isIgnored repo "/some/path/.DS_Store"
@
-}

module Data.VCS.Ignore
  ( -- Working with ignored files
    findRepo
  , listRepo
  , walkRepo
    -- * Repo /type class/
  , Repo(..)
  , RepoError(..)
    -- * /GIT/ implementation
  , Git(..)
    -- * Common data types
  , VCSIgnoreError(..)
  )
where

import           Data.VCS.Ignore.Core           ( findRepo
                                                , listRepo
                                                , walkRepo
                                                )
import           Data.VCS.Ignore.Repo           ( Repo(..)
                                                , RepoError(..)
                                                )
import           Data.VCS.Ignore.Repo.Git       ( Git(..) )
import           Data.VCS.Ignore.Types          ( VCSIgnoreError(..) )
