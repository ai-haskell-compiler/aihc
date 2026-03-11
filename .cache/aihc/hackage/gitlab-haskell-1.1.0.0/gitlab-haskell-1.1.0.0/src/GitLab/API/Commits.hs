{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Commits
-- Description : Queries about commits in repositories
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Commits
  ( -- * List repository commits
    repoCommits,

    -- * Create a commit with multiple files and actions
    createCommitMultipleFilesActions,

    -- * Get a single commit
    singleCommit,

    -- * Get references a commit is pushed to

    -- * Cherry-pick a commit
    cheryPickCommit,

    -- * Revert a commit
    revertCommit,

    -- * Get the diff of a commit
    commitDiff,

    -- * Get the comments of a commit
    commitComments,

    -- * Post comment to commit
    postCommitComment,

    -- * Get the discussions of a commit
    commitDiscussions,
    -- -- * Commit status

    -- -- * List the statuses of a commit

    -- -- * Post the build status to a commit

    -- * List merge requests associated with a commit
    commitMergeRequests,
    -- -- * Get GPG signature of a commit

    -- * Commits on specific branch
    branchCommits,

    -- * Types
    CommitAction (..),
    ContentEncoding (..),
    Action (..),
  )
where

import qualified Data.ByteString.Lazy as BSL
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client

-- | Get a list of repository commits in a project.
repoCommits ::
  -- | the project
  Project ->
  GitLab [Commit]
repoCommits prj = do
  -- return an empty list if the repository could not be found.
  result <- gitlabGetMany (commitsAddr (project_id prj)) [("with_stats", Just "true")]
  return (fromRight [] result)
  where
    commitsAddr :: Int -> Text
    commitsAddr projId =
      "/projects/"
        <> T.pack (show projId)
        <> "/repository"
        <> "/commits"

-- | Get a list of repository commits in a project.
createCommitMultipleFilesActions ::
  -- | the project
  Project ->
  -- | Name of the branch to commit into.
  Text ->
  -- | Commit message
  Text ->
  [CommitAction] ->
  GitLab (Maybe Commit)
createCommitMultipleFilesActions prj branchName commitMsg actions = do
  -- return an empty list if the repository could not be found.
  result <-
    gitlabPost
      (commitsAddr (project_id prj))
      [ ("branch", Just (T.encodeUtf8 branchName)),
        ("commit_message", Just (T.encodeUtf8 commitMsg)),
        ("actions", Just (T.encodeUtf8 (T.pack (show actions))))
      ]
  case result of
    Left resp -> error ("createCommitMultipleFilesActions: " <> show resp)
    Right x -> return x
  where
    commitsAddr :: Int -> Text
    commitsAddr projId =
      "/projects/"
        <> T.pack (show projId)
        <> "/repository"
        <> "/commits"

-- | A commit action.
data CommitAction = CommitAction
  { commit_action_action :: Action,
    -- | Full path to the file.
    commit_action_file_path :: FilePath,
    -- | Original full path to the file being
    -- moved. Ex. lib/class1.rb. Only considered for move action.
    commit_action_previous_path :: Maybe Text,
    -- | File content, required for all except delete, chmod, and
    -- move. Move actions that do not specify content preserve the
    -- existing file content, and any other value of content overwrites
    -- the file content.
    commit_action_content :: Maybe Text,
    -- | text or base64. text is default.
    commit_action_encoding :: Maybe ContentEncoding,
    -- | Last known file commit ID. Only considered in update, move, and
    -- delete actions.
    commit_action_last_commit_id :: Maybe Text,
    -- | When true/false enables/disables the execute flag on the
    -- file. Only considered for chmod action.
    commit_action_execute_filemode :: Maybe Bool
  }
  deriving (Show, Eq)

-- | The actual action within a commit action.
data Action
  = ActionCreate
  | ActionDelete
  | ActionMove
  | ActionUpdate
  | ActionChmod
  deriving (Eq)

instance Show Action where
  show ActionCreate = "create"
  show ActionDelete = "delete"
  show ActionMove = "move"
  show ActionUpdate = "update"
  show ActionChmod = "chmod"

-- | Whether the content is text or base 64.
data ContentEncoding
  = EncodingText
  | EncodingBase64
  deriving (Eq)

instance Show ContentEncoding where
  show EncodingText = "text"
  show EncodingBase64 = "base64"

-- | returns all commits of a branch from a project
-- given its project ID and the branch name.
branchCommits ::
  -- | project
  Project ->
  -- | branch name
  Text ->
  GitLab (Either (Response BSL.ByteString) [Commit])
branchCommits prj branchName = do
  gitlabGetMany (commitsAddr (project_id prj)) [("ref_name", Just (T.encodeUtf8 branchName))]
  where
    commitsAddr :: Int -> Text
    commitsAddr projId =
      "/projects/" <> T.pack (show projId) <> "/repository" <> "/commits"

-- | Get a specific commit identified by the commit hash or name of a
-- branch or tag.
singleCommit ::
  -- | the project
  Project ->
  -- | the commit hash
  Text ->
  GitLab (Maybe Commit)
singleCommit project theHash = do
  result <- gitlabGetOne (commitsAddr (project_id project)) []
  return (fromRight Nothing result)
  where
    commitsAddr :: Int -> Text
    commitsAddr projId =
      "/projects/"
        <> T.pack (show projId)
        <> "/repository"
        <> "/commits"
        <> "/"
        <> theHash

-- | Cherry-picks a commit to a given branch.
cheryPickCommit ::
  -- | the project
  Project ->
  -- | the commit hash
  Text ->
  -- | 	The name of the branch
  Text ->
  GitLab (Maybe Commit)
cheryPickCommit project theHash branchName = do
  result <-
    gitlabPost
      commitsAddr
      [ ("branch", Just (T.encodeUtf8 branchName))
      ]
  case result of
    Left _ -> return Nothing
    Right x -> return x
  where
    commitsAddr :: Text
    commitsAddr =
      "/projects/"
        <> T.pack (show (project_id project))
        <> "/repository"
        <> "/commits/"
        <> theHash
        <> "/cherry_pick"

-- | Reverts a commit in a given branch.
revertCommit ::
  -- | the project
  Project ->
  -- | the commit hash
  Text ->
  -- | target branch name
  Text ->
  GitLab (Maybe Commit)
revertCommit project theHash branchName = do
  result <-
    gitlabPost
      commitsAddr
      [ ("branch", Just (T.encodeUtf8 branchName))
      ]
  case result of
    Left _ -> return Nothing
    Right x -> return x
  where
    commitsAddr :: Text
    commitsAddr =
      "/projects/"
        <> T.pack (show (project_id project))
        <> "/repository"
        <> "/commits/"
        <> theHash
        <> "/revert"

-- | Get the diff of a commit in a project.
commitDiff ::
  -- | project
  Project ->
  -- | 	The commit hash or name of a repository branch or tag
  Text ->
  GitLab (Either (Response BSL.ByteString) [Diff])
commitDiff project sha = do
  gitlabGetMany (commitsAddr (project_id project)) []
  where
    commitsAddr :: Int -> Text
    commitsAddr projId =
      "/projects/"
        <> T.pack (show projId)
        <> "/repository"
        <> "/commits/"
        <> sha
        <> "/diff"

-- | Get the diff of a commit in a project.
commitComments ::
  -- | project
  Project ->
  -- | 	The commit hash or name of a repository branch or tag
  Text ->
  GitLab (Either (Response BSL.ByteString) [CommitNote])
commitComments project sha = do
  gitlabGetMany (commitsAddr (project_id project)) []
  where
    commitsAddr :: Int -> Text
    commitsAddr projId =
      "/projects/"
        <> T.pack (show projId)
        <> "/repository"
        <> "/commits/"
        <> sha
        <> "/comments"

-- | Adds a comment to a commit.
postCommitComment ::
  -- | project
  Project ->
  -- | The commit hash or name of a repository branch or tag
  Text ->
  -- | The text of the comment
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe CommitNote))
postCommitComment project sha note = do
  gitlabPost
    (commitsAddr (project_id project))
    [("note", Just (T.encodeUtf8 note))]
  where
    commitsAddr :: Int -> Text
    commitsAddr projId =
      "/projects/"
        <> T.pack (show projId)
        <> "/repository"
        <> "/commits/"
        <> sha
        <> "/comments"

-- | Get the discussions of a commit in a project.
commitDiscussions ::
  -- | project
  Project ->
  -- | 	The commit hash or name of a repository branch or tag
  Text ->
  GitLab (Either (Response BSL.ByteString) [Discussion])
commitDiscussions project sha = do
  gitlabGetMany (commitsAddr (project_id project)) []
  where
    commitsAddr :: Int -> Text
    commitsAddr projId =
      "/projects/"
        <> T.pack (show projId)
        <> "/repository"
        <> "/commits/"
        <> sha
        <> "/discussions"

-- | Get the discussions of a commit in a project.
commitMergeRequests ::
  -- | project
  Project ->
  -- | The commit SHA
  Text ->
  GitLab (Either (Response BSL.ByteString) [MergeRequest])
commitMergeRequests project sha = do
  gitlabGetMany (commitsAddr (project_id project)) []
  where
    commitsAddr :: Int -> Text
    commitsAddr projId =
      "/projects/"
        <> T.pack (show projId)
        <> "/repository"
        <> "/commits/"
        <> sha
        <> "/merge_requests"

-------------
-- candidates for deletion

-- -- | returns all commits of a branch from a project given the branch
-- -- name.
-- branchCommits ::
--   -- | project
--   Project ->
--   -- | branch name
--   Text ->
--   GitLab [Commit]
-- branchCommits project branchName = do
--   result <- branchCommits' (project_id project) branchName
--   return (fromRight [] result)
