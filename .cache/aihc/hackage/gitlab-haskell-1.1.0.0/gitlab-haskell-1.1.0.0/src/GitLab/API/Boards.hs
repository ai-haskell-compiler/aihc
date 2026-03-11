{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Boards
-- Description : Project issue boards, see https://docs.gitlab.com/ce/api/boards.html
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2021
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Boards
  ( -- * List project issue boards
    projectIssueBoards,

    -- * Show a single issue board
    projectIssueBoard,

    -- * Create an issue board
    createIssueBoard,

    -- * Update an issue board
    updateIssueBoard,

    -- * Delete an issue board
    deleteIssueBoard,

    -- * List board lists in a project issue board
    projectBoardLists,

    -- * Show a single board list
    boardList,

    -- * Create a board list
    createBoardList,

    -- * Reorder a list in a board
    reorderBoardList,

    -- * Update an issue board
    deleteBoardList,

    -- * Board attributes
    UpdateBoardAttrs (..),
    defaultUpdateBoardAttrs,
    CreateBoardAttrs (..),
    defaultCreateBoardAttrs,
  )
where

import qualified Data.ByteString.Lazy as BSL
import Data.Either
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client

-- | returns all issue boards for a project.
projectIssueBoards ::
  -- | the project
  Project ->
  GitLab [IssueBoard]
projectIssueBoards project = do
  result <- projectIssueBoards' (project_id project)
  -- return an empty list if the repository could not be found.
  return (fromRight [] result)

-- | returns all issue boards for a project given its project ID.
projectIssueBoards' ::
  -- | project ID
  Int ->
  GitLab (Either (Response BSL.ByteString) [IssueBoard])
projectIssueBoards' projectId =
  gitlabGetMany (boardsAddr projectId) []
  where
    boardsAddr :: Int -> Text
    boardsAddr projId =
      "/projects/" <> T.pack (show projId) <> "/boards"

-- | returns a single project issue board.
projectIssueBoard ::
  -- | the project
  Project ->
  -- | the board ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe IssueBoard))
projectIssueBoard project = do
  projectIssueBoard' (project_id project)

-- | returns a single project issue board.
projectIssueBoard' ::
  -- | the project ID
  Int ->
  -- | the board ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe IssueBoard))
projectIssueBoard' projectId boardId = do
  gitlabGetOne boardAddr []
  where
    boardAddr :: Text
    boardAddr =
      "/projects/" <> T.pack (show projectId) <> "/boards/" <> T.pack (show boardId)

-- | Creates a project issue board.
createIssueBoard ::
  -- | the project
  Project ->
  -- | board name
  Text ->
  GitLab (Maybe IssueBoard)
createIssueBoard project boardName = do
  result <- createIssueBoard' (project_id project) boardName
  return (fromRight Nothing result)

-- | Creates a project issue board.
createIssueBoard' ::
  -- | the project ID
  Int ->
  -- | board name
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe IssueBoard))
createIssueBoard' projectId boardName = do
  gitlabPost boardAddr [("name", Just (T.encodeUtf8 boardName))]
  where
    boardAddr :: Text
    boardAddr =
      "/projects/" <> T.pack (show projectId) <> "/boards"

-- | Updates a project issue board.
updateIssueBoard ::
  -- | project
  Project ->
  -- | the board ID
  Int ->
  -- | attributes for updating boards
  UpdateBoardAttrs ->
  GitLab (Either (Response BSL.ByteString) (Maybe IssueBoard))
updateIssueBoard prj boardId attrs = do
  gitlabPut boardAddr (updateBoardAttrs attrs)
  where
    boardAddr :: Text
    boardAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/boards/"
        <> T.pack (show boardId)

-- | Deletes a project issue board.
deleteIssueBoard ::
  -- | the project
  Project ->
  -- | the board
  IssueBoard ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteIssueBoard project board = do
  deleteIssueBoard' (project_id project) (board_id board)

-- | Deletes a project issue board.
deleteIssueBoard' ::
  -- | the project ID
  Int ->
  -- | the board ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteIssueBoard' projectId boardId = do
  gitlabDelete boardAddr []
  where
    boardAddr :: Text
    boardAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/boards/"
        <> T.pack (show boardId)

-- | Get a list of the board’s lists. Does not include open and closed lists.
projectBoardLists ::
  -- | the project
  Project ->
  -- | the board
  IssueBoard ->
  GitLab [BoardIssue]
projectBoardLists project board = do
  result <- projectBoardLists' (project_id project) (board_id board)
  -- return an empty list if the repository could not be found.
  return (fromRight [] result)

-- | Get a list of the board’s lists. Does not include open and closed lists.
projectBoardLists' ::
  -- | project ID
  Int ->
  -- | board ID
  Int ->
  GitLab (Either (Response BSL.ByteString) [BoardIssue])
projectBoardLists' projectId boardId =
  gitlabGetMany boardsAddr []
  where
    boardsAddr :: Text
    boardsAddr =
      "/projects/" <> T.pack (show projectId) <> "/boards/" <> T.pack (show boardId) <> "/lists"

-- | Get a single board list. Does not include open and closed lists.
boardList ::
  -- | the project
  Project ->
  -- | the board
  IssueBoard ->
  -- | list ID
  Int ->
  GitLab (Maybe BoardIssue)
boardList project board listId = do
  result <- boardList' (project_id project) (board_id board) listId
  -- return an empty list if the repository could not be found.
  return (fromRight Nothing result)

-- | Get a single board list. Does not include open and closed lists.
boardList' ::
  -- | project ID
  Int ->
  -- | board ID
  Int ->
  -- | list ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe BoardIssue))
boardList' projectId boardId listId =
  gitlabGetOne boardsAddr []
  where
    boardsAddr :: Text
    boardsAddr =
      "/projects/" <> T.pack (show projectId) <> "/boards/" <> T.pack (show boardId) <> "/lists/" <> T.pack (show listId)

-- | Creates a new issue board list.
createBoardList ::
  -- | the project
  Project ->
  -- | the board
  IssueBoard ->
  -- | attributes for creating boards
  CreateBoardAttrs ->
  GitLab (Maybe BoardIssue)
createBoardList project board attrs = do
  result <- createBoardList' (project_id project) (board_id board) attrs
  -- return an empty list if the repository could not be found.
  return (fromRight Nothing result)

-- | Creates a new issue board list.
createBoardList' ::
  -- | project ID
  Int ->
  -- | board ID
  Int ->
  -- | attributes for creating the board
  CreateBoardAttrs ->
  GitLab (Either (Response BSL.ByteString) (Maybe BoardIssue))
createBoardList' projectId boardId attrs =
  gitlabPost boardsAddr (createBoardAttrs attrs)
  where
    boardsAddr :: Text
    boardsAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/boards/"
        <> T.pack (show boardId)
        <> "/lists"

-- | Updates an existing issue board list. This call is used to change list position.
reorderBoardList ::
  -- | project
  Project ->
  -- | board
  IssueBoard ->
  -- | list ID
  Int ->
  -- | the position of the list
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe BoardIssue))
reorderBoardList project board =
  reorderBoardList' (project_id project) (board_id board)

-- | Updates an existing issue board list. This call is used to change list position.
reorderBoardList' ::
  -- | project ID
  Int ->
  -- | board ID
  Int ->
  -- | list ID
  Int ->
  -- | the position of the list
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe BoardIssue))
reorderBoardList' projectId boardId listId newPosition =
  gitlabPut boardsAddr [("position", Just (T.encodeUtf8 (T.pack (show newPosition))))]
  where
    boardsAddr :: Text
    boardsAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/boards/"
        <> T.pack (show boardId)
        <> "/lists/"
        <> T.pack (show listId)

-- | Only for administrators and project owners. Deletes a board list.
deleteBoardList ::
  -- | project
  Project ->
  -- | board
  IssueBoard ->
  -- | list ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteBoardList project board =
  deleteBoardList' (project_id project) (board_id board)

-- | Only for administrators and project owners. Deletes a board list.
deleteBoardList' ::
  -- | project ID
  Int ->
  -- | board ID
  Int ->
  -- | list ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteBoardList' projectId boardId listId =
  gitlabDelete boardsAddr []
  where
    boardsAddr :: Text
    boardsAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/boards/"
        <> T.pack (show boardId)
        <> "/lists/"
        <> T.pack (show listId)

-- | Attributes for updating when editing a board with the
-- functions for updating issue boards.
data UpdateBoardAttrs = UpdateBoardAttrs
  { updateBoard_new_name :: Maybe String,
    updateBoard_assignee_id :: Maybe Int,
    updateBoard_milestone_id :: Maybe Int,
    updateBoard_labels :: Maybe String,
    updateBoard_weight :: Maybe Int
  }

-- | default attributes for board update.
defaultUpdateBoardAttrs :: UpdateBoardAttrs
defaultUpdateBoardAttrs =
  UpdateBoardAttrs Nothing Nothing Nothing Nothing Nothing

updateBoardAttrs :: UpdateBoardAttrs -> [GitLabParam]
updateBoardAttrs attrs =
  catMaybes
    [ (\s -> Just ("name", Just (T.encodeUtf8 (T.pack s)))) =<< updateBoard_new_name attrs,
      (\i -> Just ("assignee_id", Just (T.encodeUtf8 (T.pack (show i))))) =<< updateBoard_assignee_id attrs,
      (\i -> Just ("milestone_id", Just (T.encodeUtf8 (T.pack (show i))))) =<< updateBoard_milestone_id attrs,
      (\s -> Just ("labels", Just (T.encodeUtf8 (T.pack s)))) =<< updateBoard_labels attrs,
      (\i -> Just ("weight", Just (T.encodeUtf8 (T.pack (show i))))) =<< updateBoard_weight attrs
    ]

-- | exactly one parameter must be provided.
data CreateBoardAttrs = CreateBoardAttrs
  { createBoard_label_id :: Maybe Int,
    createBoard_assignee_id :: Maybe Int,
    createBoard_milestone_id :: Maybe Int
  }

-- | default attributes for board creation.
defaultCreateBoardAttrs :: CreateBoardAttrs
defaultCreateBoardAttrs =
  CreateBoardAttrs Nothing Nothing Nothing

createBoardAttrs :: CreateBoardAttrs -> [GitLabParam]
createBoardAttrs attrs =
  catMaybes
    [ (\i -> Just ("label_id", Just (T.encodeUtf8 (T.pack (show i))))) =<< createBoard_label_id attrs,
      (\i -> Just ("assignee_id", Just (T.encodeUtf8 (T.pack (show i))))) =<< createBoard_assignee_id attrs,
      (\i -> Just ("milestone_id", Just (T.encodeUtf8 (T.pack (show i))))) =<< createBoard_milestone_id attrs
    ]
