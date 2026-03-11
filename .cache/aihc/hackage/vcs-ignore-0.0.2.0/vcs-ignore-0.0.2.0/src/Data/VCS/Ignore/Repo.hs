{-# LANGUAGE StrictData #-}

{-|
Module      : Data.VCS.Ignore.Repo
Description : Type class representing the VCS repository
Copyright   : (c) 2020-2022 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module contains /type class/ representing the selected type of /VCS/
repository.
-}

module Data.VCS.Ignore.Repo
  ( Repo(..)
  , RepoError(..)
  )
where

import           Control.Exception              ( Exception(..) )
import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.VCS.Ignore.Types          ( fromVCSIgnoreError
                                                , toVCSIgnoreError
                                                )


-- | /Type class/ representing instance of /VCS/ repository of selected type.
-- In order to obtain instance, the physical repository needs to be scanned
-- first by the 'scanRepo' method. Then absolute path to the repository root is
-- provided by 'repoRoot' method. To check if any path (relative to the repo
-- root) is ignored or not, use the 'isIgnored' method.
class Repo r where

  -- | Returns name of the repository (e.g. @GIT@).
  repoName :: r    -- ^ /VCS/ repository instance
           -> Text -- ^ name of the repository

  -- | Returns absolute path to the root of the /VCS/ repository.
  repoRoot :: r        -- ^ /VCS/ repository instance
           -> FilePath -- ^ absolute path to the repository

  -- | Scans repository at given path. If the given path doesn't contain valid
  -- repository, 'RepoError' may be thrown.
  scanRepo :: (MonadIO m, MonadThrow m)
           => FilePath -- ^ path to the /VCS/ repository root
           -> m r      -- ^ scanned repository (or failure)

  -- | Checks whether the given path is ignored. The input path is expected to
  -- be relative to the repository root, it might or might not point to existing
  -- file or directory.
  isIgnored :: MonadIO m
            => r        -- ^ /VCS/ repository instance
            -> FilePath -- ^ path to check, relative to the repository root
            -> m Bool   -- ^ whether the path is ignored or not


-- | Represents error related to operations over the /VCS/ repository.
data RepoError = InvalidRepo FilePath Text
  -- ^ Given 'FilePath' doesn't contain valid /VCS/ repository root.
  deriving (Eq, Show)

instance Exception RepoError where
  displayException = displayException'
  fromException    = fromVCSIgnoreError
  toException      = toVCSIgnoreError


displayException' :: RepoError -> String
displayException' (InvalidRepo path reason) =
  mconcat ["Path '", path, "' is not a valid repository: ", T.unpack reason]
