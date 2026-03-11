{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Branches
-- Description : Queries about repository branches
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Branches
  ( -- * List repository branches
    branches,

    -- * Get single repository branch
    branch,

    -- * Create repository branch
    createRepositoryBranch,

    -- * Delete repository branch
    deleteRepositoryBranch,

    -- * Delete merged branches
    deleteMergedBranches,
  )
where

import qualified Data.ByteString.Lazy as BSL
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
  ( gitlabDelete,
    gitlabGetMany,
    gitlabGetOne,
    gitlabPost,
  )
import Network.HTTP.Client

-- | Get a list of repository branches from a project, sorted by name
-- alphabetically.
branches :: Project -> GitLab [Branch]
branches project = do
  result <- branches' (project_id project)
  return (fromRight (error "branches error") result)

-- | Get a list of repository branches from a project given its
-- project ID, sorted by name alphabetically.
branches' ::
  -- | project ID
  Int ->
  GitLab (Either (Response BSL.ByteString) [Branch])
branches' projectId =
  gitlabGetMany addr []
  where
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/repository"
        <> "/branches"

-- | Get a single project repository branch.
branch ::
  -- | the project
  Project ->
  -- | the branch name
  Text ->
  GitLab (Maybe Branch)
branch project branchName = do
  result <- branch' (project_id project) branchName
  return (fromRight (error "branch error") result)

-- | Get a single project repository branch.
branch' ::
  -- | the project ID
  Int ->
  -- | name of the branch
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Branch))
branch' projectId branchName =
  gitlabGetOne addr []
  where
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/repository"
        <> "/branches/"
        <> branchName

-- | Create a new branch in the repository.
createRepositoryBranch ::
  -- | the project
  Project ->
  -- | branch name
  Text ->
  -- | Branch name or commit SHA to create branch from
  Text ->
  GitLab (Maybe Branch)
createRepositoryBranch project branchName branchFrom = do
  result <- createRepositoryBranch' (project_id project) branchName branchFrom
  -- return an empty list if the repository could not be found.
  return (fromRight Nothing result)

-- | Create a new branch in the repository.
createRepositoryBranch' ::
  -- | project ID
  Int ->
  -- | branch name
  Text ->
  -- | Branch name or commit SHA to create branch from
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Branch))
createRepositoryBranch' projectId branchName branchFrom =
  gitlabPost newBranchAddr [("branch", Just (T.encodeUtf8 branchName)), ("ref", Just (T.encodeUtf8 branchFrom))]
  where
    newBranchAddr :: Text
    newBranchAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/repository"
        <> "/branches"

-- | Delete a branch from the repository.
deleteRepositoryBranch ::
  -- | project
  Project ->
  -- | branch name
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteRepositoryBranch project =
  deleteRepositoryBranch' (project_id project)

-- | Delete a branch from the repository.
deleteRepositoryBranch' ::
  -- | project ID
  Int ->
  -- | branch name
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteRepositoryBranch' projectId branchName =
  gitlabDelete branchAddr []
  where
    branchAddr :: Text
    branchAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/repository"
        <> "/branches/"
        <> branchName

-- | Deletes all branches that are merged into the project’s default branch.
deleteMergedBranches ::
  -- | project
  Project ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteMergedBranches project =
  deleteMergedBranches' (project_id project)

-- | Deletes all branches that are merged into the project’s default branch.
deleteMergedBranches' ::
  -- | project ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteMergedBranches' projectId =
  gitlabDelete branchAddr []
  where
    branchAddr :: Text
    branchAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/repository"
        <> "/merged_branches"
