{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Discussions
-- Description : Queries about discussions, which are a set of related notes on snippets, issues, epics, merge requests and commits.
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2021
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Discussions
  ( -- * Issues

    -- ** List project issue discussion items
    projectIssueDiscussions,

    -- ** Get single issue discussion item
    issueDiscussion,

    -- ** Create new issue thread
    createIssueThread,

    -- ** Add note to existing issue thread
    addNoteToIssueThread,

    -- ** Modify existing issue thread note
    modifyThreadNoteIssue,

    -- ** Delete an issue thread note
    deleteIssueThreadNote,

    -- * Snippets

    -- ** List project snippet discussion items
    snippetDiscussionItems,

    -- ** Get single snippet discussion item
    snippetDiscussionItem,

    -- ** Create new snippet thread
    createSnippetThread,

    -- ** Add note to existing snippet thread
    addNoteToSnippetThread,

    -- ** Modify existing snippet thread note
    modifySnippetThreadNote,

    -- ** Delete a snippet thread note
    deleteSnippetThreadNote,
    -- -- * Epics

    -- -- ** List group epic discussion items

    -- -- ** Get single epic discussion item

    -- -- ** Create new epic thread

    -- -- ** Add note to existing epic thread

    -- -- ** Modify existing epic thread note

    -- -- ** Delete an epic thread note

    -- * Merge requests

    -- ** List project merge request discussion items
    projectMergeRequestDiscussionItems,

    -- ** Get single merge request discussion item
    mergeRequestDiscussionItems,

    -- ** Create new merge request thread
    createMergeRequestThread,
    -- -- ** Create a new thread on the overview page

    -- -- ** Create a new thread in the merge request diff

    -- -- ** Parameters for multiline comments

    -- * Line code

    -- ** Resolve a merge request thread
    resolveMergeRequestThread,

    -- ** Add note to existing merge request thread
    addNoteToMergeRequestThread,

    -- ** Modify an existing merge request thread note
    modifyMergeRequestThreadNote,

    -- ** Delete a merge request thread note
    deleteMergeRequestThreadNote,

    -- * Commits

    -- ** List project commit discussion items
    projectCommitDiscussionItems,

    -- ** Get single commit discussion item
    projectCommitDiscussionItem,

    -- ** Create new commit thread
    createCommitThread,

    -- ** Add note to existing commit thread
    addNoteToCommitThread,

    -- ** Modify an existing commit thread note
    modifyCommityThreadNote,

    -- ** Delete a commit thread note
    deleteCommitThreadNote,

    -- * Types
    PositionReference (..),
  )
where

import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client

-- | Gets a list of all discussion items for a single issue.
projectIssueDiscussions ::
  -- | project
  Project ->
  -- | The IID of an issue
  Int ->
  GitLab (Either (Response BSL.ByteString) [Discussion])
projectIssueDiscussions prj issueIid = do
  let urlPath =
        T.pack $
          "/projects/"
            <> show (project_id prj)
            <> "/issues/"
            <> show issueIid
            <> "/discussions"
  gitlabGetMany urlPath []

-- | Returns a single discussion item for a specific project issue.
issueDiscussion ::
  -- | project
  Project ->
  -- | The IID of an issue
  Int ->
  -- | The ID of a discussion item
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
issueDiscussion prj issueIid discussionId = do
  let urlPath =
        T.pack $
          "/projects/"
            <> show (project_id prj)
            <> "/issues/"
            <> show issueIid
            <> "/discussions/"
            <> show discussionId
  gitlabGetOne urlPath []

-- | Creates a new thread to a single project issue. This is similar
-- to creating a note but other comments (replies) can be added to it
-- later.
createIssueThread ::
  -- | project
  Project ->
  -- | The IID of an issue
  Int ->
  -- | The content of the thread
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
createIssueThread prj issueIid threadContent = do
  gitlabPost discussionAddr [("body", Just (T.encodeUtf8 threadContent))]
  where
    discussionAddr :: Text
    discussionAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/issues/"
        <> T.pack (show issueIid)
        <> "/discussions"

-- | Adds a new note to the thread. This can also create a thread from
-- a single comment. Notes can be added to other items than comments,
-- such as system notes, making them threads.
addNoteToIssueThread ::
  -- | project
  Project ->
  -- | The IID of an issue
  Int ->
  -- | The ID of a thread
  Int ->
  -- -- | The ID of a thread note
  -- Int ->

  -- | The content of the note/reply
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
addNoteToIssueThread prj issueIid discussionId content = do
  gitlabPost discussionAddr [("body", Just (T.encodeUtf8 content))]
  where
    discussionAddr :: Text
    discussionAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/issues/"
        <> T.pack (show issueIid)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes"

-- <> "/notes/"
-- <> T.pack (show noteId)

-- | Modify existing thread note of an issue.
modifyThreadNoteIssue ::
  -- | project
  Project ->
  -- | The IID of an issue
  Int ->
  -- | The ID of a thread
  Int ->
  -- | The ID of a thread note
  Int ->
  -- | The content of the note/reply
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
modifyThreadNoteIssue prj issueIid discussionId noteId content = do
  gitlabPut noteAddr [("body", Just (T.encodeUtf8 content))]
  where
    noteAddr :: Text
    noteAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/issues/"
        <> T.pack (show issueIid)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes/"
        <> T.pack (show noteId)

-- | Deletes an existing thread note of an issue.
deleteIssueThreadNote ::
  -- | project
  Project ->
  -- | The IID of an issue
  Int ->
  -- | The ID of a discussion
  Int ->
  -- | The ID of a discussion note
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteIssueThreadNote prj issueIid discussionId noteId = do
  gitlabDelete noteAddr []
  where
    noteAddr :: Text
    noteAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/issues/"
        <> T.pack (show issueIid)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes/"
        <> T.pack (show noteId)

-- | Gets a list of all discussion items for a single snippet.
snippetDiscussionItems ::
  -- | project
  Project ->
  -- | snippet ID
  Int ->
  GitLab (Either (Response BSL.ByteString) [Discussion])
snippetDiscussionItems prj snippetId =
  gitlabGetMany urlPath []
  where
    urlPath =
      T.pack $
        "/projects/"
          <> show (project_id prj)
          <> "/snippets/"
          <> show snippetId
          <> "/discussions"

-- | Returns a single discussion item for a specific project snippet.
snippetDiscussionItem ::
  -- | project
  Project ->
  -- | snippet ID
  Int ->
  -- | discussion ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
snippetDiscussionItem prj snippetId discussionId =
  gitlabGetOne urlPath []
  where
    urlPath =
      T.pack $
        "/projects/"
          <> show (project_id prj)
          <> "/snippets/"
          <> show snippetId
          <> "/discussions/"
          <> show discussionId

-- | Creates a new thread to a single project snippet. This is similar
-- to creating a note but other comments (replies) can be added to it
-- later.
createSnippetThread ::
  -- | project
  Project ->
  -- | snippet ID
  Int ->
  -- | The content of a discussion
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
createSnippetThread prj snippetId content = do
  gitlabPost discussionAddr [("body", Just (T.encodeUtf8 content))]
  where
    discussionAddr :: Text
    discussionAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/snippets/"
        <> T.pack (show snippetId)
        <> "/discussions"

-- | Adds a new note to the thread.
addNoteToSnippetThread ::
  -- | project
  Project ->
  -- | snippet ID
  Int ->
  -- | discussion ID
  Int ->
  -- -- | note ID
  -- Int ->

  -- | The content of the note/reply
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
addNoteToSnippetThread prj snippetId discussionId content =
  gitlabPost discussionAddr [("body", Just (T.encodeUtf8 content))]
  where
    discussionAddr :: Text
    discussionAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/snippets/"
        <> T.pack (show snippetId)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes"

-- <> "/notes/"
-- <> T.pack (show noteId)

-- | Modify existing thread note of a snippet.
modifySnippetThreadNote ::
  -- | project
  Project ->
  -- | snippet ID
  Int ->
  -- | discussion ID
  Int ->
  -- | note ID
  Int ->
  -- | The content of the note/reply
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
modifySnippetThreadNote prj snippetId discussionId noteId content =
  gitlabPut noteAddr [("body", Just (T.encodeUtf8 content))]
  where
    noteAddr :: Text
    noteAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/snippets/"
        <> T.pack (show snippetId)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes/"
        <> T.pack (show noteId)

-- | Deletes an existing thread note of an issue.
deleteSnippetThreadNote ::
  -- | Project
  Project ->
  -- | snippet ID
  Int ->
  -- | discussion ID
  Int ->
  -- | note ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteSnippetThreadNote prj snippetId discussionId noteId = do
  gitlabDelete noteAddr []
  where
    noteAddr :: Text
    noteAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/snippets/"
        <> T.pack (show snippetId)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes/"
        <> T.pack (show noteId)

-- | Gets a list of all discussion items for a single merge request.
projectMergeRequestDiscussionItems ::
  -- | project
  Project ->
  -- | Merge request IID
  Int ->
  GitLab (Either (Response BSL.ByteString) [Discussion])
projectMergeRequestDiscussionItems prj mergeRequestIid = do
  gitlabGetMany urlPath []
  where
    urlPath =
      T.pack $
        "/projects/"
          <> show (project_id prj)
          <> "/merge_requests/"
          <> show mergeRequestIid
          <> "/discussions"

-- | Gets a list of all discussion items for a single merge request.
mergeRequestDiscussionItems ::
  -- | project
  Project ->
  -- | Merge request IID
  Int ->
  -- | discussion ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
mergeRequestDiscussionItems prj mergeRequestIid discussionId = do
  gitlabGetOne urlPath []
  where
    urlPath =
      T.pack $
        "/projects/"
          <> show (project_id prj)
          <> "/merge_requests/"
          <> show mergeRequestIid
          <> "/discussions/"
          <> show discussionId

-- | Position reference for an entry in a discussion.
data PositionReference = TextPos | ImagePos

instance Show PositionReference where
  show TextPos = "text"
  show ImagePos = "image"

-- | Creates a new thread to a single project merge request. This is
-- similar to creating a note but other comments (replies) can be
-- added to it later.  See the GitLab document:
-- https://docs.gitlab.com/ee/api/discussions.html#create-new-merge-request-thread
createMergeRequestThread ::
  -- | project
  Project ->
  -- | merge request ID
  Int ->
  -- | The content of the thread
  Text ->
  -- | Base commit SHA in the source branch
  Text ->
  -- | SHA referencing commit in target branch
  Text ->
  -- | SHA referencing HEAD of this merge request
  Text ->
  -- | Type of the position reference
  PositionReference ->
  -- | File path after change
  Text ->
  -- | File path before change
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
createMergeRequestThread prj mergeRequestIid content baseCommitShaSource shaCommitTarget shaHeadMR typePosRef filePathAfter filePathBefore =
  gitlabPost
    discussionAddr
    [ ("body", Just (T.encodeUtf8 content)),
      ("position[base_sha]", Just (T.encodeUtf8 baseCommitShaSource)),
      ("position[start_sha]", Just (T.encodeUtf8 shaCommitTarget)),
      ("position[head_sha]", Just (T.encodeUtf8 shaHeadMR)),
      ("position[position_type]", Just (T.encodeUtf8 (T.pack (show typePosRef)))),
      ("position[new_path]", Just (T.encodeUtf8 filePathAfter)),
      ("position[old_path]", Just (T.encodeUtf8 filePathBefore))
    ]
  where
    discussionAddr :: Text
    discussionAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/merge_requests/"
        <> T.pack (show mergeRequestIid)
        <> "/discussions"

-- | Resolve/unresolve whole thread of a merge request.
resolveMergeRequestThread ::
  -- | project
  Project ->
  -- | merge request IID
  Int ->
  -- | discussion ID
  Int ->
  -- | Resolve/unresolve the discussion
  Bool ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
resolveMergeRequestThread prj mergeRequestIid discussionId resolved =
  gitlabPut noteAddr [("resolved", Just (T.encodeUtf8 (resolvedStr resolved)))]
  where
    noteAddr :: Text
    noteAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/merge_requests/"
        <> T.pack (show mergeRequestIid)
        <> "/discussions/"
        <> T.pack (show discussionId)
    resolvedStr True = "true"
    resolvedStr False = "false"

-- | Adds a new note to the thread. This can also create a thread from a single comment.
addNoteToMergeRequestThread ::
  -- | project
  Project ->
  -- | merge request ID
  Int ->
  -- | discussion ID
  Int ->
  -- -- | note ID
  -- Int ->

  -- | The content of the note/reply
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
addNoteToMergeRequestThread prj mergeRequestIid discussionId content =
  gitlabPost
    discussionAddr
    [ ("body", Just (T.encodeUtf8 content))
    ]
  where
    discussionAddr :: Text
    discussionAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/merge_requests/"
        <> T.pack (show mergeRequestIid)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes"

-- | exactly one of body or resolved must be a 'Just' value
modifyMergeRequestThreadNote ::
  -- | project
  Project ->
  -- | merge request IID
  Int ->
  -- | discussion ID
  Int ->
  -- | note ID
  Int ->
  -- | The content of the note/reply
  Maybe Text ->
  -- | Resolve/unresolve the note
  Maybe Bool ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
modifyMergeRequestThreadNote prj mergeRequestIid discussionId noteId content resolved =
  gitlabPut
    noteAddr
    (catMaybes [contentAttr, resolveAttr])
  where
    contentAttr =
      case content of
        Nothing -> Nothing
        Just x -> Just ("body", Just (T.encodeUtf8 x))
    resolveAttr =
      case resolved of
        Nothing -> Nothing
        Just x ->
          if x
            then Just ("resolved", Just (T.encodeUtf8 "true"))
            else Just ("resolved", Just (T.encodeUtf8 "false"))
    noteAddr :: Text
    noteAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/merge_requests/"
        <> T.pack (show mergeRequestIid)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes/"
        <> T.pack (show noteId)

-- | Deletes an existing thread note of a merge request.
deleteMergeRequestThreadNote ::
  -- | project
  Project ->
  -- | merge request IID
  Int ->
  -- | discussion ID
  Int ->
  -- | note ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteMergeRequestThreadNote prj mergeRequestIid discussionId noteId = do
  gitlabDelete noteAddr []
  where
    noteAddr :: Text
    noteAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/merge_requests/"
        <> T.pack (show mergeRequestIid)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes/"
        <> T.pack (show noteId)

-- | Gets a list of all discussion items for a single commit.
projectCommitDiscussionItems ::
  -- | project
  Project ->
  -- | commit ID
  Int ->
  GitLab (Either (Response BSL.ByteString) [Discussion])
projectCommitDiscussionItems prj commitId =
  gitlabGetMany urlPath []
  where
    urlPath =
      T.pack $
        "/projects/"
          <> show (project_id prj)
          <> "/commits/"
          <> show commitId
          <> "/discussions"

-- | Returns a single discussion item for a specific project commit.
projectCommitDiscussionItem ::
  -- | project
  Project ->
  -- | commit ID
  Int ->
  -- | discussion ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
projectCommitDiscussionItem prj commitId discussionId =
  gitlabGetOne urlPath []
  where
    urlPath =
      T.pack $
        "/projects/"
          <> show (project_id prj)
          <> "/commits/"
          <> show commitId
          <> "/discussions/"
          <> show discussionId

-- | Creates a new thread to a single project commit. This is similar
-- to creating a note but other comments (replies) can be added to it
-- later.
createCommitThread ::
  -- | project
  Project ->
  -- | commit ID
  Int ->
  -- | The content of the thread
  Text ->
  -- | SHA of the parent commit
  Text ->
  -- | SHA of the parent commit (bug in GitLab document?)
  Text ->
  -- | The SHA of this commit
  Text ->
  -- | Type of the position reference
  PositionReference ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
createCommitThread prj commitId content shaParent shaStart shaThisCommit typePosRef =
  gitlabPost
    discussionAddr
    [ ("body", Just (T.encodeUtf8 content)),
      ("position[base_sha]", Just (T.encodeUtf8 shaParent)),
      ("position[start_sha]", Just (T.encodeUtf8 shaStart)),
      ("position[head_sha]", Just (T.encodeUtf8 shaThisCommit)),
      ("position[position_type]", Just (T.encodeUtf8 (T.pack (show typePosRef))))
    ]
  where
    discussionAddr :: Text
    discussionAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/commits/"
        <> T.pack (show commitId)
        <> "/discussions"

-- | Adds a new note to the thread.
addNoteToCommitThread ::
  -- | project
  Project ->
  -- | commit ID
  Int ->
  -- | discussion ID
  Int ->
  -- -- | note ID
  -- Int ->

  -- | The content of the note/reply
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
addNoteToCommitThread prj commitId discussionId content =
  gitlabPost
    discussionAddr
    [ ("body", Just (T.encodeUtf8 content))
    ]
  where
    discussionAddr :: Text
    discussionAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/commits/"
        <> T.pack (show commitId)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes"

-- | Adds a new note to the thread.
modifyCommityThreadNote ::
  -- | project
  Project ->
  -- | commit ID
  Int ->
  -- | discussion ID
  Int ->
  -- | note ID
  Int ->
  -- | The content of the note/reply
  Maybe Text ->
  -- | Resolve/unresolve the note
  Maybe Bool ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
modifyCommityThreadNote prj commitId discussionId noteId content resolved =
  gitlabPut
    noteAddr
    (catMaybes [contentAttr, resolveAttr])
  where
    contentAttr =
      case content of
        Nothing -> Nothing
        Just x -> Just ("body", Just (T.encodeUtf8 x))
    resolveAttr =
      case resolved of
        Nothing -> Nothing
        Just x ->
          if x
            then Just ("resolved", Just (T.encodeUtf8 "true"))
            else Just ("resolved", Just (T.encodeUtf8 "false"))
    noteAddr :: Text
    noteAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/commits/"
        <> T.pack (show commitId)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes/"
        <> T.pack (show noteId)

-- | Deletes an existing thread note of a commit.
deleteCommitThreadNote ::
  -- | project
  Project ->
  -- | commit ID
  Int ->
  -- | discussion ID
  Int ->
  -- | The ID of a discussion note
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteCommitThreadNote prj commitId discussionId noteId = do
  gitlabDelete noteAddr []
  where
    noteAddr :: Text
    noteAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/commits/"
        <> T.pack (show commitId)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes/"
        <> T.pack (show noteId)
