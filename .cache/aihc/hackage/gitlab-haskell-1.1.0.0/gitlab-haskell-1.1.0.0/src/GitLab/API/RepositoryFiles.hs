{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : RepositoryFiles
-- Description : Queries about project repository files
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.RepositoryFiles where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI

-- | Allows you to receive information about file in repository like
-- name, size, content. File content is Base64 encoded.
repositoryFile ::
  -- | the project
  Project ->
  -- | the file path
  Text ->
  -- | name of the branch, tag or commit
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe RepositoryFile))
repositoryFile prj filePath reference =
  gitlabGetOne addr [("ref", Just (T.encodeUtf8 reference))]
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/repository"
        <> "/files"
        <> "/"
        <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 filePath))

-- | Allows you to receive blame information. Each blame range
-- contains lines and corresponding commit information.
repositoryFileBlame ::
  -- | the project
  Project ->
  -- | the file path
  Text ->
  -- | name of the branch, tag or commit
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe RepositoryFileBlame))
repositoryFileBlame prj filePath reference =
  gitlabGetOne addr [("ref", Just (T.encodeUtf8 reference))]
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/repository"
        <> "/files"
        <> "/"
        <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 filePath))
        <> "/blame"

-- | Get a raw file from a repository.
repositoryFileRawFile ::
  -- | the project
  Project ->
  -- | the file path
  Text ->
  -- | The name of branch, tag or commit. Default is the HEAD of the
  -- project.
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Text))
repositoryFileRawFile prj filePath reference =
  gitlabGetOne addr [("ref", Just (T.encodeUtf8 reference))]
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/repository"
        <> "/files"
        <> "/"
        <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 filePath))
        <> "/raw"

-- | Allows you to receive information about blob in repository like
-- size and content. Blob content is Base64 encoded.
repositoryFileBlob ::
  -- | project ID
  Int ->
  -- | blob SHA
  Text ->
  GitLab (Either (Response BSL.ByteString) String)
repositoryFileBlob projectId blobSha = do
  resp <- gitlabGetByteStringResponse addr []
  if successStatus (responseStatus resp)
    then return (Right (BSL8.unpack (responseBody resp)))
    else return (Left resp)
  where
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/repository"
        <> "/blobs/"
        <> blobSha
        <> "/raw"
    successStatus :: Status -> Bool
    successStatus (Status n _msg) =
      n >= 200 && n <= 226

-- | This allows you to create a single file. For creating multiple
-- files with a single request see the commits API.
createRepositoryFile ::
  -- | the project
  Project ->
  -- | the file path
  Text ->
  -- | Name of the new branch to create. The commit is added to this
  -- branch.
  Text ->
  -- | The file’s content
  Text ->
  -- | The commit message
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe RepositoryFileSimple))
createRepositoryFile prj filePath branchName fContent commitMsg =
  gitlabPost addr [("branch", Just (T.encodeUtf8 branchName)), ("content", Just (T.encodeUtf8 fContent)), ("commit_message", Just (T.encodeUtf8 commitMsg))]
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/repository"
        <> "/files/"
        <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 filePath))

-- | This allows you to update a single file. For updating multiple
-- files with a single request see the commits API.
updateRepositoryFile ::
  -- | the project
  Project ->
  -- | the file path
  Text ->
  -- | Name of the new branch to create. The commit is added to this
  -- branch.
  Text ->
  -- | The file’s content
  Text ->
  -- | The commit message
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe RepositoryFileSimple))
updateRepositoryFile prj filePath branchName fContent commitMsg =
  gitlabPut addr [("branch", Just (T.encodeUtf8 branchName)), ("content", Just (T.encodeUtf8 fContent)), ("commit_message", Just (T.encodeUtf8 commitMsg))]
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/repository"
        <> "/files/"
        <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 filePath))

-- | This allows you to delete a single file. For deleting multiple files with a single request, see the commits API.
deleteRepositoryFile ::
  -- | the project
  Project ->
  -- | the file path
  Text ->
  -- | Name of the new branch to create. The commit is added to this
  -- branch.
  Text ->
  -- | The commit message
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteRepositoryFile prj filePath branchName commitMsg =
  gitlabDelete addr [("branch", Just (T.encodeUtf8 branchName)), ("commit_message", Just (T.encodeUtf8 commitMsg))]
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/repository"
        <> "/files/"
        <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 filePath))
