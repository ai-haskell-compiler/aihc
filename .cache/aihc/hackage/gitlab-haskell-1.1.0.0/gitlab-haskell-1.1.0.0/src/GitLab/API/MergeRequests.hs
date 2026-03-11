{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : MergeRequests
-- Description : Queries about merge requests against projects
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.MergeRequests
  ( -- * List merge requests
    mergeRequests,
    mergeRequestsWith,
    --     -- * Merge requests list response notes

    --     -- * List project merge requests

    --     -- * List group merge requests

    -- * Get single MR
    mergeRequest,
    -- -- * Single merge request response notes

    --     -- * Get single MR participants

    --     -- * Get single MR reviewers

    --     -- * Get single MR commits

    --     -- * Get single MR changes

    --     -- * List MR pipelines

    --     -- * Create MR Pipeline

    -- * Create MR
    createMergeRequest,
    --     -- * Update MR

    -- * Accept MR
    acceptMergeRequest,

    -- * Delete a merge request
    deleteMergeRequest,

    -- * merge request attributes
    mrAttrs,
    MergeProjectAttrs (..),
    MergeRequestState (..),
    WIP (..),
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Either
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import Data.Time.Format.ISO8601
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client

-- | returns the merge request for a project given its merge request
-- IID.
mergeRequest ::
  -- | project
  Project ->
  -- | merge request IID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe MergeRequest))
mergeRequest project =
  mergeRequest' (project_id project)

-- | returns the merge request for a project given its project ID and
-- merge request IID.
mergeRequest' ::
  -- | project ID
  Int ->
  -- | merge request IID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe MergeRequest))
mergeRequest' projectId mergeRequestIID =
  gitlabGetOne addr []
  where
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/merge_requests/"
        <> T.pack (show mergeRequestIID)

-- | returns the merge requests for a project.
mergeRequests ::
  -- | the project
  Project ->
  GitLab [MergeRequest]
mergeRequests p = do
  result <- mergeRequests' (project_id p)
  return (fromRight (error "mergeRequests error") result)

-- | returns the merge requests for a project given its project ID.
mergeRequests' ::
  -- | project ID
  Int ->
  GitLab (Either (Response BSL.ByteString) [MergeRequest])
mergeRequests' projectId =
  gitlabGetMany addr [("scope", Just "all")]
  where
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/merge_requests"

-- | returns the merge requests for a project and a set of search
-- attributes as 'Just' values in 'MergeProjectAttrs'. The 'mrAttrs'
-- value has default merge request search values, which is a record
-- that can be modified with 'Just' values.
--
-- For example to search only for open merge requests for a project:
--
-- > mergeRequestsWith myProject (mrAttrs {mr_attr_state = Just MROpened})
mergeRequestsWith ::
  -- | the project
  Project ->
  -- | merge request search attributes
  MergeProjectAttrs ->
  GitLab [MergeRequest]
mergeRequestsWith p attrs = do
  result <- mergeRequestsWith' (project_id p) attrs
  return (fromRight (error "mergeRequests error") result)

-- | returns the merge requests for a project given its project ID and
-- a set of search attributes as 'Just' values in 'MergeProjectAttrs'.
-- The 'mrAttrs' value has default merge request search values, which
-- is a record that can be modified with 'Just' values.
--
-- For example to search only for open merge requests for project with
-- ID 11744514:
--
-- > mergeRequestsWith' 11744514 (mrAttrs {mr_attr_state = Just MROpened})
mergeRequestsWith' ::
  -- | project ID
  Int ->
  -- | merge request search attributes
  MergeProjectAttrs ->
  GitLab (Either (Response BSL.ByteString) [MergeRequest])
mergeRequestsWith' projectId attrs =
  gitlabGetMany addr (mrAttrsParams attrs)
  where
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/merge_requests"

-- | Creates a merge request.
createMergeRequest ::
  -- | project
  Project ->
  -- | source branch
  Text ->
  -- | target branch
  Text ->
  -- | target project ID
  Int ->
  -- | merge request title
  Text ->
  -- | merge request description
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe MergeRequest))
createMergeRequest project =
  createMergeRequest' (project_id project)

-- | Creates a merge request.
createMergeRequest' ::
  -- | project ID
  Int ->
  -- | source branch
  Text ->
  -- | target branch
  Text ->
  -- | target project ID
  Int ->
  -- | merge request title
  Text ->
  -- | merge request description
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe MergeRequest))
createMergeRequest' projectId sourceBranch targetBranch targetProjectId mrTitle mrDescription =
  gitlabPost addr params
  where
    params :: [GitLabParam]
    params =
      [ ("source_branch", Just (T.encodeUtf8 sourceBranch)),
        ("target_branch", Just (T.encodeUtf8 targetBranch)),
        ("target_project_id", Just (T.encodeUtf8 (T.pack (show targetProjectId)))),
        ("title", Just (T.encodeUtf8 mrTitle)),
        ("description", Just (T.encodeUtf8 mrDescription))
      ]
    addr = T.pack $ "/projects/" <> show projectId <> "/merge_requests"

-- | Accepts a merge request.
acceptMergeRequest ::
  -- | project
  Project ->
  -- | merge request IID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe MergeRequest))
acceptMergeRequest project =
  acceptMergeRequest' (project_id project)

-- | Accepts a merge request.
acceptMergeRequest' ::
  -- | project ID
  Int ->
  -- | merge request IID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe MergeRequest))
acceptMergeRequest' projectId mergeRequestIid = gitlabPost addr params
  where
    params :: [GitLabParam]
    params =
      [ ("id", Just (T.encodeUtf8 (T.pack (show projectId)))),
        ("merge_request_iid", Just (T.encodeUtf8 (T.pack (show mergeRequestIid))))
      ]
    addr =
      T.pack $
        "/projects/"
          <> show projectId
          <> "/merge_requests/"
          <> show mergeRequestIid
          <> "/merge"

-- | Deletes a merge request. Only for admins and project owners.
deleteMergeRequest ::
  -- | project
  Project ->
  -- | merge request IID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteMergeRequest project =
  deleteMergeRequest' (project_id project)

-- | Deletes a merge request. Only for admins and project owners.
deleteMergeRequest' ::
  -- | project ID
  Int ->
  -- | merge request IID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteMergeRequest' projectId mergeRequestIid = gitlabDelete addr []
  where
    addr =
      T.pack $
        "/projects/"
          <> show projectId
          <> "/merge_requests/"
          <> show mergeRequestIid

-- | Attributes when searching for merge requests with the
-- 'mergeRequestsWith' functions.
data MergeProjectAttrs = MergeProjectAttrs
  { -- | Return all merge requests or just those that are opened,
    -- closed, locked, or merged.
    mr_attr_state :: Maybe MergeRequestState,
    -- | Return requests ordered by created_at or updated_at
    -- fields. Default is created_at. Note that the 'OrderBy' type has
    -- more options, but only 'CreatedAt' and 'UpdatedAt' are
    -- applicable for ordering merge requests.
    mr_attr_order_by :: Maybe OrderBy,
    -- | Return requests sorted in asc or desc order. Default is desc.
    mr_attr_sort :: Maybe SortBy,
    -- | Return merge requests for a specific milestone. None returns
    -- merge requests with no milestone. Any returns merge requests
    -- that have an assigned milestone.
    mr_attr_milestone :: Maybe Milestone,
    -- -- | If simple, returns the iid, URL, title, description, and
    -- -- basic state of merge request.
    -- merge_request_view :: Maybe MergeRequestView,

    -- | Return merge requests matching a comma separated list of
    -- labels.
    mr_attr_labels :: Maybe Text,
    -- | If true, response returns more details for each label in
    -- labels field: :name, :color, :description, :description_html,
    -- :text_color. Default is false.
    mr_attr_with_labels_details :: Maybe Bool,
    -- | If true, this projection requests (but does not guarantee)
    -- that the merge_status field be recalculated
    -- asynchronously. Default is false.
    mr_attr_with_merge_status_recheck :: Maybe Bool,
    -- | Return merge requests created on or after the given time.
    mr_attr_created_after :: Maybe UTCTime,
    -- | Return merge requests created on or before the given time.
    mr_attr_created_before :: Maybe UTCTime,
    -- | Return merge requests updated on or after the given time.
    mr_attr_updated_after :: Maybe UTCTime,
    -- | Return merge requests updated on or before the given time.
    mr_attr_updated_before :: Maybe UTCTime,
    -- | Return merge requests for the given scope: created_by_me,
    -- assigned_to_me or all. Defaults to created_by_me.
    mr_attr_scope :: Maybe Scope,
    -- | Returns merge requests created by the given user id. Mutually
    -- exclusive with author_username. Combine with scope=all or
    -- scope=assigned_to_me.
    mr_attr_author_id :: Maybe Int,
    -- | Returns merge requests created by the given
    -- username. Mutually exclusive with author_id.
    mr_attr_author_username :: Maybe Text,
    -- | Returns merge requests assigned to the given user id.
    mr_attr_assignee_id :: Maybe Int,
    -- | Returns merge requests which have specified all the users
    -- with the given ids as individual approvers.
    mr_attr_approver_ids :: Maybe [Int],
    -- | Returns merge requests which have been approved by all the
    -- users with the given ids (Max: 5).
    mr_attr_approved_by_ids :: Maybe [Int],
    -- | Returns merge requests which have the user as a reviewer with
    -- the given user id. Mutually exclusive with reviewer_username.
    mr_attr_reviewer_id :: Maybe Int,
    -- | Returns merge requests which have the user as a reviewer with
    -- the given username. Mutually exclusive with reviewer_id.
    mr_attr_reviewer_username :: Maybe Text,
    -- | Return merge requests reacted by the authenticated user by
    -- the given emoji.
    mr_attr_my_reaction_emoji :: Maybe Text,
    -- | Return merge requests with the given source branch.
    mr_attr_source_branch :: Maybe Text,
    -- | Return merge requests with the given target branch.
    mr_attr_target_branch :: Maybe Text,
    -- | Search merge requests against their title and description.
    mr_attr_search :: Maybe Text,
    -- | Modify the scope of the search attribute. title, description,
    -- or a string joining them with comma. Default is
    -- title,description.
    mr_attr_in :: Maybe SearchIn,
    -- | Filter merge requests against their wip status. yes to return
    -- only draft merge requests, no to return non-draft merge
    -- requests.
    mr_attr_wip :: Maybe WIP,
    -- -- | Return merge requests that do not match the parameters
    -- -- supplied. Accepts: labels, milestone, author_id,
    -- -- author_username, assignee_id, assignee_username, reviewer_id,
    -- -- reviewer_username, my_reaction_emoji.
    -- merge_request_not :: Maybe Text,

    -- | Returns merge requests deployed to the given environment.
    mr_attr_environment :: Maybe UTCTime,
    -- | Return merge requests deployed before the given date/time.
    mr_attr_deployed_before :: Maybe UTCTime,
    -- | Return merge requests deployed after the given date/time.
    mr_attr_deployed_after :: Maybe UTCTime
  }

-- TODO create types for merge_request_my_reaction_emoji

-- | for filtering by merge request state.
data MergeRequestState
  = -- | return only opened merge requests
    MROpened
  | -- | return only closed merge requests
    MRClosed
  | -- | return only locked merge requests
    MRLocked
  | -- | return only merged merge requests
    MRMerged

instance Show MergeRequestState where
  show MROpened = "opened"
  show MRClosed = "closed"
  show MRLocked = "locked"
  show MRMerged = "merged"

-- | WIP status of merge requests
data WIP
  = -- | return only draft merge requests
    WIPYes
  | -- | return non-draft merge requests
    WIPNo

instance Show WIP where
  show WIPYes = "yes"
  show WIPNo = "no"

-- | No merge request search filters, thereby returning all merge requests. Default scope is "all".
mrAttrs :: MergeProjectAttrs
mrAttrs =
  MergeProjectAttrs Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

mrAttrsParams :: MergeProjectAttrs -> [GitLabParam]
mrAttrsParams attrs =
  catMaybes
    [ (\x -> Just ("state", showAttr x)) =<< mr_attr_state attrs,
      (\x -> Just ("order_by", showAttr x)) =<< mr_attr_order_by attrs,
      (\x -> Just ("sort", showAttr x)) =<< mr_attr_sort attrs,
      (\x -> Just ("milestone", showAttr x)) =<< mr_attr_milestone attrs,
      (\t -> Just ("labels", showAttrT t)) =<< mr_attr_labels attrs,
      (\b -> Just ("with_labels_details", showBool b)) =<< mr_attr_with_labels_details attrs,
      (\b -> Just ("with_merge_status_recheck", showBool b)) =<< mr_attr_with_merge_status_recheck attrs,
      (\t -> Just ("created_after", showTime t)) =<< mr_attr_created_after attrs,
      (\t -> Just ("created_before", showTime t)) =<< mr_attr_created_before attrs,
      (\t -> Just ("updated_after", showTime t)) =<< mr_attr_updated_after attrs,
      (\t -> Just ("updated_before", showTime t)) =<< mr_attr_updated_before attrs,
      (\x -> Just ("scope", showAttr x)) =<< mr_attr_scope attrs,
      (\i -> Just ("author_id", showAttr i)) =<< mr_attr_author_id attrs,
      (\t -> Just ("author_username", showAttrT t)) =<< mr_attr_author_username attrs,
      (\i -> Just ("assignee_id", showAttr i)) =<< mr_attr_assignee_id attrs,
      (\i -> Just ("approver_ids", showAttr i)) =<< mr_attr_approver_ids attrs,
      (\is -> Just ("approved_by_ids", showAttr is)) =<< mr_attr_approved_by_ids attrs,
      (\i -> Just ("reviewer_id", showAttr i)) =<< mr_attr_reviewer_id attrs,
      (\t -> Just ("reviewer_username", showAttrT t)) =<< mr_attr_reviewer_username attrs,
      (\t -> Just ("my_reaction_emoji", showAttrT t)) =<< mr_attr_my_reaction_emoji attrs,
      (\t -> Just ("source_branch", showAttrT t)) =<< mr_attr_source_branch attrs,
      (\t -> Just ("target_branch", showAttrT t)) =<< mr_attr_target_branch attrs,
      (\t -> Just ("search", showAttrT t)) =<< mr_attr_search attrs,
      (\x -> Just ("in", showAttr x)) =<< mr_attr_in attrs,
      (\x -> Just ("wip", showAttr x)) =<< mr_attr_wip attrs,
      (\t -> Just ("environment", showTime t)) =<< mr_attr_environment attrs,
      (\t -> Just ("deployed_before", showTime t)) =<< mr_attr_deployed_before attrs,
      (\t -> Just ("deployed_after", showTime t)) =<< mr_attr_deployed_after attrs
    ]
  where
    showAttr :: (Show a) => a -> Maybe BS.ByteString
    showAttr = Just . T.encodeUtf8 . T.pack . show
    showAttrT = Just . T.encodeUtf8
    showBool :: Bool -> Maybe BS.ByteString
    showBool True = Just (T.encodeUtf8 (T.pack "true"))
    showBool False = Just (T.encodeUtf8 (T.pack "false"))
    showTime :: UTCTime -> Maybe BS.ByteString
    showTime = Just . T.encodeUtf8 . T.pack . iso8601Show
