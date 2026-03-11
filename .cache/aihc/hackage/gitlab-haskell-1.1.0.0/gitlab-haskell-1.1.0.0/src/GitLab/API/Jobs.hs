{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Jobs
-- Description : Queries about jobs ran on projects
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Jobs
  ( -- * List project jobs
    jobs,

    -- * List pipeline jobs
    pipelineJobs,

    -- * List pipeline bridges
    pipelineBridges,

    -- * Get a single job
    job,

    -- * Cancel a job
    cancelJob,

    -- * Retry a job
    retryJob,

    -- * Erase a job
    eraseJob,

    -- * Run a job
    runJob,
  )
where

import qualified Data.ByteString.Lazy as BSL
import Data.Either
import qualified Data.Text as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client

-- | returns all jobs ran on a project.
jobs ::
  -- | the project
  Project ->
  GitLab [Job]
jobs project = do
  result <- jobs' (project_id project)
  return (fromRight (error "jobs error") result)

-- | Get a list of jobs in a project. Jobs are sorted in descending
-- order of their IDs.
jobs' ::
  -- | the project ID
  Int ->
  GitLab (Either (Response BSL.ByteString) [Job])
jobs' projectId =
  gitlabGetMany addr []
  where
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/jobs"

-- | Get a list of jobs for a pipeline.
pipelineJobs ::
  -- | the project
  Project ->
  -- | pipeline ID
  Int ->
  GitLab (Either (Response BSL.ByteString) [Job])
pipelineJobs prj pipelineId =
  gitlabGetMany addr []
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/pipelines/"
        <> T.pack (show pipelineId)
        <> "/jobs"

-- | Get a list of bridge jobs for a pipeline.
pipelineBridges ::
  -- | the project
  Project ->
  -- | pipeline ID
  Int ->
  GitLab (Either (Response BSL.ByteString) [Job])
pipelineBridges prj pipelineId =
  gitlabGetMany addr []
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/pipelines/"
        <> T.pack (show pipelineId)
        <> "/bridges"

-- | Get a single job of a project.
job ::
  -- | the project
  Project ->
  -- | job ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Job))
job prj jobId =
  gitlabGetOne addr []
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/jobs/"
        <> T.pack (show jobId)

-- | Cancel a single job of a project.
cancelJob ::
  -- | the project
  Project ->
  -- | job ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Job))
cancelJob prj jobId =
  gitlabPost addr []
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/jobs/"
        <> T.pack (show jobId)
        <> "/cancel"

-- | Retry a single job of a project.
retryJob ::
  -- | the project
  Project ->
  -- | job ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Job))
retryJob prj jobId =
  gitlabPost addr []
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/jobs/"
        <> T.pack (show jobId)
        <> "/cancel"

-- | Retry a single job of a project.
eraseJob ::
  -- | the project
  Project ->
  -- | job ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Job))
eraseJob prj jobId =
  gitlabPost addr []
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/jobs/"
        <> T.pack (show jobId)
        <> "/erase"

-- | Triggers a manual action to start a job.
runJob ::
  -- | the project
  Project ->
  -- | job ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Job))
runJob prj jobId =
  gitlabPost addr []
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/jobs/"
        <> T.pack (show jobId)
        <> "/play"
