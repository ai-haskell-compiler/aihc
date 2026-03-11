{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Todos
-- Description : Queries about todos for users
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Todos
  ( -- * Get a list of to-do items
    todos,

    -- * Mark a to-do item as done
    todoDone,

    -- * Mark all to-do items as done
    todosDone,

    -- * TODO's filters
    defaultTodoFilters,
    TodoAttrs (..),
  )
where

import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client

-- | returns all pending todos for the user, as defined by the access token.
todos :: TodoAttrs -> GitLab [Todo]
todos attrs = gitlabUnsafe (gitlabGetOne "/todos" params)
  where
    params :: [GitLabParam]
    params = groupProjectAttrs attrs

-- | Attributes related to listing groups
data TodoAttrs = TodoAttrs
  { -- | The action to be filtered
    todoFilter_action :: Maybe TodoAction,
    -- | The ID of an author
    todoFilter_author_id :: Maybe Int,
    -- | The ID of a project
    todoFilter_project_id :: Maybe Int,
    -- | The ID of a group
    todoFilter_group_id :: Maybe Int,
    -- | The state of the to-do item
    todoFilter_state :: Maybe TodoState,
    -- | The type of to-do item.
    todoFilter_type :: Maybe TodoType
  }

groupProjectAttrs :: TodoAttrs -> [GitLabParam]
groupProjectAttrs filters =
  catMaybes
    [ (\x -> Just ("action", textToBS (T.pack (show x)))) =<< todoFilter_action filters,
      (\i -> Just ("author_id", textToBS (T.pack (show i)))) =<< todoFilter_author_id filters,
      (\i -> Just ("project_id", textToBS (T.pack (show i)))) =<< todoFilter_project_id filters,
      (\i -> Just ("group_id", textToBS (T.pack (show i)))) =<< todoFilter_group_id filters,
      (\x -> Just ("state", textToBS (T.pack (show x)))) =<< todoFilter_state filters,
      (\x -> Just ("type", textToBS (T.pack (show x)))) =<< todoFilter_type filters
    ]
  where
    textToBS = Just . T.encodeUtf8

-- | No todo filters applied.
defaultTodoFilters :: TodoAttrs
defaultTodoFilters =
  TodoAttrs Nothing Nothing Nothing Nothing Nothing Nothing

-- | Marks a single pending to-do item given by its ID for the current
-- user as done.
todoDone ::
  -- | The ID of to-do item
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
todoDone todoId =
  gitlabPost addr []
  where
    addr =
      T.pack $
        "/todos/"
          <> show todoId
          <> "/mark_as_done"

-- | Marks all pending to-do items for the current user as done. It
-- returns the HTTP status code 204 with an empty response.
todosDone ::
  GitLab
    (Either (Response BSL.ByteString) (Maybe ()))
todosDone =
  gitlabPost addr []
  where
    addr =
      T.pack $
        "/todos"
          <> "/mark_as_done"
