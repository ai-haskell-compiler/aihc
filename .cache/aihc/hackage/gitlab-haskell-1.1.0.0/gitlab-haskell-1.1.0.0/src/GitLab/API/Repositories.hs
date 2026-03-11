{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Repositories
-- Description : Queries about project repositories
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Repositories
  ( -- * List repository tree
    repositoryTree,

    -- * Get file archive
    fileArchive,
    fileArchiveBS,

    -- * Contributors
    contributors,

    -- * Merge Base
    mergeBase,
  )
where

import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Either
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client
import Network.HTTP.Types.Status

-- | returns a list of repository files and directories in a project.
repositoryTree ::
  -- | the project
  Project ->
  GitLab [Repository]
repositoryTree project =
  fromRight (error "repositories error") <$> repositories' (project_id project)

-- | returns a list of repository files and directories in a project
-- given its project ID.
repositories' ::
  -- | the project ID
  Int ->
  GitLab (Either (Response BSL.ByteString) [Repository])
repositories' projectId =
  gitlabGetMany addr []
  where
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/repository"
        <> "/tree"

-- | get a file archive of the repository files. For example:
--
-- > fileArchive myProject TarGz "/tmp/myProject.tar.gz"
fileArchive ::
  -- | project
  Project ->
  -- | file format
  ArchiveFormat ->
  -- | file path to store the archive
  FilePath ->
  GitLab (Either (Response BSL.ByteString) ())
fileArchive project = getFileArchive' (project_id project)

-- | get a file archive of the repository files as a
-- 'BSL.ByteString'. For example:
--
-- > fileArchiveBS myProject TarGz "/tmp/myProject.tar.gz"
fileArchiveBS ::
  -- | project
  Project ->
  -- | file format
  ArchiveFormat ->
  GitLab (Either (Response BSL.ByteString) BSL.ByteString)
fileArchiveBS project format = do
  result <- getFileArchiveBS' (project_id project) format
  case result of
    Left resp -> return (Left resp)
    Right Nothing -> error "could not download file"
    Right (Just bs) -> return (Right bs)

-- | get a file archive of the repository files using the project's
--   ID. For example:
--
-- > getFileArchive' 3453 Zip "/tmp/myProject.zip"
getFileArchive' ::
  -- | project ID
  Int ->
  -- | file format
  ArchiveFormat ->
  -- | file path to store the archive
  FilePath ->
  GitLab (Either (Response BSL.ByteString) ())
getFileArchive' projectId format fPath = do
  attempt <- getFileArchiveBS' projectId format
  case attempt of
    Left st -> return (Left st)
    Right Nothing ->
      Right <$> error "cannot download file"
    Right (Just archiveData) ->
      Right <$> liftIO (BSL.writeFile fPath archiveData)

-- | get a file archive of the repository files as a 'BSL.ByteString'
--   using the project's ID. For example:
--
-- > getFileArchiveBS' 3453 Zip "/tmp/myProject.zip"
getFileArchiveBS' ::
  -- | project ID
  Int ->
  -- | file format
  ArchiveFormat ->
  GitLab (Either (Response BSL.ByteString) (Maybe BSL.ByteString))
getFileArchiveBS' projectId format = do
  result <- gitlabGetByteStringResponse addr []
  let (Status n _msg) = responseStatus result
  if n >= 200 && n <= 226
    then return (Right (Just (responseBody result)))
    else return (Left result)
  where
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/repository"
        <> "/archive"
        <> T.pack (show format)

-- | Get repository contributors list.
contributors ::
  -- | project
  Project ->
  -- | Return contributors ordered by name, email, or commits (orders
  -- by commit date) fields. Default is commits.
  Maybe OrderBy ->
  -- | Return contributors sorted in asc or desc order. Default is
  -- asc.
  Maybe SortBy ->
  GitLab [Contributor]
contributors prj order sort =
  fromRight (error "contributors error")
    <$> gitlabGetMany addr params
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/repository"
        <> "/contributors"
    params :: [GitLabParam]
    params =
      catMaybes
        [ (\x -> Just ("sort", showAttr x)) =<< sort,
          (\x -> Just ("order_by", showAttr x)) =<< order
        ]
    showAttr :: (Show a) => a -> Maybe BS.ByteString
    showAttr = Just . T.encodeUtf8 . T.pack . show

-- | Get the common ancestor for 2 or more refs.
mergeBase ::
  -- | project
  Project ->
  -- | The refs to find the common ancestor of, multiple refs can be
  -- passed. An example of a ref is
  -- '304d257dcb821665ab5110318fc58a007bd104ed'.
  [Text] ->
  GitLab (Either (Response BSL.ByteString) (Maybe Commit))
mergeBase prj refs =
  gitlabGetOne
    addr
    params
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/repository"
        <> "/merge_base"
    params :: [GitLabParam]
    params =
      map (\ref -> ("refs[]", Just (T.encodeUtf8 ref))) refs
