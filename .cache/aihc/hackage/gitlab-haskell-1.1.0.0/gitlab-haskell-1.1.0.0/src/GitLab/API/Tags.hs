{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tags
-- Description : Queries about tags in repositories
-- Copyright   : (c) Jihyun Yu, 2021; Rob Stewart, 2022
-- License     : BSD3
-- Maintainer  : yjh0502@gmail.com, robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Tags
  ( -- * List project repository tags
    tags,

    -- * Get a single repository tag
    tag,

    -- * Create a new tag
    createTag,

    -- * Delete a tag
    deleteTag,
  )
where

import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client

-- | Get a list of repository tags from a project.
tags ::
  -- | the project
  Project ->
  GitLab (Either (Response BSL.ByteString) [Tag])
tags prj = do
  gitlabGetMany (commitsAddr (project_id prj)) []
  where
    commitsAddr :: Int -> Text
    commitsAddr projId =
      "/projects/"
        <> T.pack (show projId)
        <> "/repository"
        <> "/tags"

-- | Get a specific repository tag determined by its name.
tag ::
  -- | the project
  Project ->
  -- | the name of the tag
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Tag))
tag prj tagName = do
  gitlabGetOne (commitsAddr (project_id prj)) []
  where
    commitsAddr :: Int -> Text
    commitsAddr projId =
      "/projects/"
        <> T.pack (show projId)
        <> "/repository"
        <> "/tags/"
        <> tagName

-- | Creates a new tag in the repository that points to the supplied
-- ref.
createTag ::
  -- | the project
  Project ->
  -- | the name of the tag
  Text ->
  -- | Create tag using commit SHA, another tag name, or branch name
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Tag))
createTag prj tagName ref = do
  gitlabPost newTagAddr [("tag_name", Just (T.encodeUtf8 tagName)), ("ref", Just (T.encodeUtf8 ref))]
  where
    newTagAddr :: Text
    newTagAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/repository"
        <> "/tags"

-- | Deletes a tag of a repository with given name.
deleteTag ::
  -- | the project
  Project ->
  -- | the name of the tag
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteTag prj tagName =
  gitlabDelete tagAddr []
  where
    tagAddr :: Text
    tagAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/repository"
        <> "/tags/"
        <> tagName
