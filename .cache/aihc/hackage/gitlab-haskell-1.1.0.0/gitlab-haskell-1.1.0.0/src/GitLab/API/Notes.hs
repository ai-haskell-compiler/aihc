{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Notes
-- Description : Notes on issues, snippets, merge requests and epics
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2020
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Notes
  ( -- * Issues
    issueNotes,
    issueNote,
    newIssueNote,
    modifyIssueNote,
    deleteIssueNote,

    -- * Snippets
    snippetNotes,
    snippetNote,
    newSnippetNote,
    modifySnippetNote,
    deleteSnippetNote,

    -- * Merge Requests
    mergeRequestNotes,
    mergeRequestNote,
    newMergeRequestNote,
    modifyMergeRequestNote,
    deleteMergeRequestNote,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client

-- | Gets a list of all notes for a single issue.
issueNotes ::
  -- | project
  Project ->
  -- | issue IID
  Int ->
  -- | sort the issues
  Maybe SortBy ->
  -- | Return issue notes ordered by created_at or updated_at fields
  Maybe OrderBy ->
  GitLab (Either (Response BSL.ByteString) [Note])
issueNotes prj issueIid sort order =
  gitlabGetMany addr params
  where
    params :: [GitLabParam]
    params =
      catMaybes
        [ (\x -> Just ("sort", showAttr x)) =<< sort,
          (\x -> Just ("order_by", showAttr x)) =<< order
        ]
    addr =
      T.pack $
        "/projects/"
          <> show (project_id prj)
          <> "/issues/"
          <> show issueIid
          <> "/notes"
    showAttr :: (Show a) => a -> Maybe BS.ByteString
    showAttr = Just . T.encodeUtf8 . T.pack . show

-- | Returns a single note for a specific project issue.
issueNote ::
  -- | project
  Project ->
  -- | issue IID
  Int ->
  -- | note ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Note))
issueNote prj issueIid noteId =
  gitlabGetOne addr params
  where
    params :: [GitLabParam]
    params = []
    addr =
      T.pack $
        "/projects/"
          <> show (project_id prj)
          <> "/issues/"
          <> show issueIid
          <> "/notes/"
          <> show noteId

-- | Creates a new note to a single project issue.
newIssueNote ::
  -- | project
  Project ->
  -- | issue IID
  Int ->
  -- | the body of the note
  Text ->
  -- | The confidential flag of a note. Default is false.
  Maybe Bool ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
newIssueNote prj issueIid theNote isConfidential =
  gitlabPost addr params
  where
    params :: [GitLabParam]
    params =
      catMaybes
        [ Just ("body", Just (T.encodeUtf8 theNote)),
          (\x -> Just ("confidential", showAttr x)) =<< isConfidential
        ]
    addr =
      T.pack $
        "/projects/"
          <> show (project_id prj)
          <> "/issues/"
          <> show issueIid
          <> "/notes"
    showAttr :: (Show a) => a -> Maybe BS.ByteString
    showAttr = Just . T.encodeUtf8 . T.pack . show

-- | Modify existing note of an issue.
modifyIssueNote ::
  -- | project
  Project ->
  -- | issue IID
  Int ->
  -- | note ID
  Int ->
  -- | the body of the note
  Maybe Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Note))
modifyIssueNote prj issueIid noteId theNote = do
  gitlabPut urlPath params
  where
    urlPath =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/issues/"
        <> T.pack (show issueIid)
        <> "/notes/"
        <> T.pack
          (show noteId)
    params ::
      [GitLabParam]
    params =
      catMaybes
        [ (\x -> Just ("body", Just (T.encodeUtf8 x))) =<< theNote
        ]

-- | Deletes an existing note of an issue.
deleteIssueNote ::
  -- | project
  Project ->
  -- | issue IID
  Int ->
  -- | note ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteIssueNote prj issueIid noteId = do
  gitlabDelete addr []
  where
    addr =
      T.pack $
        "/projects/"
          <> show (project_id prj)
          <> "/issues/"
          <> show issueIid
          <> "/notes/"
          <> show noteId

-- | Gets a list of all notes for a single snippet.
snippetNotes ::
  -- | project
  Project ->
  -- | snippet IID
  Int ->
  -- | sort the snippets
  Maybe SortBy ->
  -- | Return snippet notes ordered by created_at or updated_at fields
  Maybe OrderBy ->
  GitLab (Either (Response BSL.ByteString) [Note])
snippetNotes prj snippetIid sort order =
  gitlabGetMany addr params
  where
    params :: [GitLabParam]
    params =
      catMaybes
        [ (\x -> Just ("sort", showAttr x)) =<< sort,
          (\x -> Just ("order_by", showAttr x)) =<< order
        ]
    addr =
      T.pack $
        "/projects/"
          <> show (project_id prj)
          <> "/snippets/"
          <> show snippetIid
          <> "/notes"
    showAttr :: (Show a) => a -> Maybe BS.ByteString
    showAttr = Just . T.encodeUtf8 . T.pack . show

-- | Returns a single note for a specific project snippet.
snippetNote ::
  -- | project
  Project ->
  -- | snippet IID
  Int ->
  -- | note ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Note))
snippetNote prj snippetIid noteId =
  gitlabGetOne addr params
  where
    params :: [GitLabParam]
    params = []
    addr =
      T.pack $
        "/projects/"
          <> show (project_id prj)
          <> "/snippets/"
          <> show snippetIid
          <> "/notes/"
          <> show noteId

-- | Creates a new note to a single project snippet.
newSnippetNote ::
  -- | project
  Project ->
  -- | snippet IID
  Int ->
  -- | the body of the note
  Text ->
  -- | The confidential flag of a note. Default is false.
  Maybe Bool ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
newSnippetNote prj snippetIid theNote isConfidential =
  gitlabPost addr params
  where
    params :: [GitLabParam]
    params =
      catMaybes
        [ Just ("body", Just (T.encodeUtf8 theNote)),
          (\x -> Just ("confidential", showAttr x)) =<< isConfidential
        ]
    addr =
      T.pack $
        "/projects/"
          <> show (project_id prj)
          <> "/snippets/"
          <> show snippetIid
          <> "/notes"
    showAttr :: (Show a) => a -> Maybe BS.ByteString
    showAttr = Just . T.encodeUtf8 . T.pack . show

-- | Modify existing note of an snippet.
modifySnippetNote ::
  -- | project
  Project ->
  -- | snippet IID
  Int ->
  -- | note ID
  Int ->
  -- | the body of the note
  Maybe Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Note))
modifySnippetNote prj snippetIid noteId theNote = do
  gitlabPut urlPath params
  where
    urlPath =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/snippets/"
        <> T.pack (show snippetIid)
        <> "/notes/"
        <> T.pack
          (show noteId)
    params ::
      [GitLabParam]
    params =
      catMaybes
        [ (\x -> Just ("body", Just (T.encodeUtf8 x))) =<< theNote
        ]

-- | Deletes an existing note of an snippet.
deleteSnippetNote ::
  -- | project
  Project ->
  -- | snippet IID
  Int ->
  -- | note ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteSnippetNote prj snippetIid noteId = do
  gitlabDelete addr []
  where
    addr =
      T.pack $
        "/projects/"
          <> show (project_id prj)
          <> "/snippets/"
          <> show snippetIid
          <> "/notes/"
          <> show noteId

-- | Gets a list of all notes for a single merge request.
mergeRequestNotes ::
  -- | project
  Project ->
  -- | merge request IID
  Int ->
  -- | sort the merge requests
  Maybe SortBy ->
  -- | Return merge request notes ordered by created_at or updated_at fields
  Maybe OrderBy ->
  GitLab (Either (Response BSL.ByteString) [Note])
mergeRequestNotes prj mergeRequestIid sort order =
  gitlabGetMany addr params
  where
    params :: [GitLabParam]
    params =
      catMaybes
        [ (\x -> Just ("sort", showAttr x)) =<< sort,
          (\x -> Just ("order_by", showAttr x)) =<< order
        ]
    addr =
      T.pack $
        "/projects/"
          <> show (project_id prj)
          <> "/merge_requests/"
          <> show mergeRequestIid
          <> "/notes"
    showAttr :: (Show a) => a -> Maybe BS.ByteString
    showAttr = Just . T.encodeUtf8 . T.pack . show

-- | Returns a single note for a specific project merge request.
mergeRequestNote ::
  -- | project
  Project ->
  -- | merge request IID
  Int ->
  -- | note ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Note))
mergeRequestNote prj mergeRequestIid noteId =
  gitlabGetOne addr params
  where
    params :: [GitLabParam]
    params = []
    addr =
      T.pack $
        "/projects/"
          <> show (project_id prj)
          <> "/merge_requests/"
          <> show mergeRequestIid
          <> "/notes/"
          <> show noteId

-- | Creates a new note to a single project merge request.
newMergeRequestNote ::
  -- | project
  Project ->
  -- | merge request IID
  Int ->
  -- | the body of the note
  Text ->
  -- | The confidential flag of a note. Default is false.
  Maybe Bool ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
newMergeRequestNote prj mergeRequestIid theNote isConfidential =
  gitlabPost addr params
  where
    params :: [GitLabParam]
    params =
      catMaybes
        [ Just ("body", Just (T.encodeUtf8 theNote)),
          (\x -> Just ("confidential", showAttr x)) =<< isConfidential
        ]
    addr =
      T.pack $
        "/projects/"
          <> show (project_id prj)
          <> "/merge_requests/"
          <> show mergeRequestIid
          <> "/notes"
    showAttr :: (Show a) => a -> Maybe BS.ByteString
    showAttr = Just . T.encodeUtf8 . T.pack . show

-- | Modify existing note of an merge request.
modifyMergeRequestNote ::
  -- | project
  Project ->
  -- | merge request IID
  Int ->
  -- | note ID
  Int ->
  -- | the body of the note
  Maybe Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Note))
modifyMergeRequestNote prj mergeRequestIid noteId theNote = do
  gitlabPut urlPath params
  where
    urlPath =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/merge_requests/"
        <> T.pack (show mergeRequestIid)
        <> "/notes/"
        <> T.pack
          (show noteId)
    params ::
      [GitLabParam]
    params =
      catMaybes
        [ (\x -> Just ("body", Just (T.encodeUtf8 x))) =<< theNote
        ]

-- | Deletes an existing note of an merge request.
deleteMergeRequestNote ::
  -- | project
  Project ->
  -- | merge request IID
  Int ->
  -- | note ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteMergeRequestNote prj mergeRequestIid noteId = do
  gitlabDelete addr []
  where
    addr =
      T.pack $
        "/projects/"
          <> show (project_id prj)
          <> "/merge_requests/"
          <> show mergeRequestIid
          <> "/notes/"
          <> show noteId
