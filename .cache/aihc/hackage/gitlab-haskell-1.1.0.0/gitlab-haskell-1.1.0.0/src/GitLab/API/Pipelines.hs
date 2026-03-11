{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Pipelines
-- Description : Queries about project pipelines
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Pipelines
  ( -- * List project pipelines
    pipelines,

    -- * Get a single pipeline
    pipeline,

    -- * Get a pipeline’s test report
    pipelineTestReport,

    -- * Create a new pipeline
    newPipeline,

    -- * Retry jobs in a pipeline
    retryPipeline,

    -- * Cancel a pipeline’s jobs
    cancelPipelineJobs,

    -- * Delete a pipeline
    deletePipeline,
  )
where

import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client

-- | List pipelines in a project. Child pipelines are not included in the
-- results, but you can get child pipeline individually. Returns 'Nothing' if
-- access to the pipelines are forbidden e.g. for a project without any files.
pipelines ::
  -- | the project
  Project ->
  GitLab (Maybe [Pipeline])
pipelines p = do
  result <- pipelines' (project_id p)
  case result of
    Right ps -> return (Just ps)
    Left _err -> return Nothing

-- return (fromRight (error "pipelines error") result)

-- | returns the pipelines for a project given its project ID.
pipelines' ::
  -- | the project ID
  Int ->
  GitLab (Either (Response BSL.ByteString) [Pipeline])
pipelines' projectId =
  gitlabGetMany
    addr
    [("sort", Just "desc")] -- most recent first
  where
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/pipelines"

-- | Get one pipeline from a project.
pipeline ::
  -- | the project
  Project ->
  -- | 	The ID of a pipeline
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Pipeline))
pipeline prj pipelineId =
  gitlabGetOne
    addr
    []
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/pipelines/"
        <> T.pack (show pipelineId)

-- | get a pipeline’s test report. Since GitLab 13.0.
pipelineTestReport ::
  -- | the project
  Project ->
  -- | the pipeline ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe TestReport))
pipelineTestReport prj pipelineId = do
  let urlPath =
        T.pack
          ( "/projects/"
              <> show (project_id prj)
              <> "/pipelines/"
              <> show pipelineId
              <> "/test_report"
          )
  gitlabGetOne urlPath []

-- | Create a new pipeline. Since GitLab 14.6.
newPipeline ::
  -- | the project
  Project ->
  -- | The branch or tag to run the pipeline on.
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Pipeline))
newPipeline prj ref = do
  gitlabPost
    pipelineAddr
    [("ref", Just (T.encodeUtf8 ref))]
  where
    pipelineAddr :: Text
    pipelineAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/pipeline"

-- | Retry a pipeline. Since GitLab 14.6.
retryPipeline ::
  -- | the project
  Project ->
  -- | The ID of a pipeline
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Pipeline))
retryPipeline prj pipelineId = do
  gitlabPost
    pipelineAddr
    []
  where
    pipelineAddr :: Text
    pipelineAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/pipelines/"
        <> T.pack (show pipelineId)
        <> "/retry"

-- | Cancel a pipeline's jobs.
cancelPipelineJobs ::
  -- | the project
  Project ->
  -- | The ID of a pipeline
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Pipeline))
cancelPipelineJobs prj pipelineId = do
  gitlabPost
    pipelineAddr
    []
  where
    pipelineAddr :: Text
    pipelineAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/pipelines/"
        <> T.pack (show pipelineId)
        <> "/cancel"

-- | Delete a pipline. Since GitLab 14.6.
deletePipeline ::
  -- | the project
  Project ->
  -- | The ID of a pipeline
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deletePipeline prj pipelineId = do
  gitlabDelete pipelineAddr []
  where
    pipelineAddr :: Text
    pipelineAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/pipelines/"
        <> T.pack (show pipelineId)
