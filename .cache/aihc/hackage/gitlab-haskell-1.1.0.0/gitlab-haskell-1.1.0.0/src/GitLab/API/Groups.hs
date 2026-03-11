{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Groups
-- Description : Queries about and updates to groups
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Groups
  ( -- * List groups
    groups,

    -- * List a group’s subgroups
    subGroups,

    -- * List a group’s descendant groups
    descendantGroups,

    -- * List a group’s projects
    groupProjects,

    -- * List a group’s shared projects
    groupSharedProjects,

    -- * Details of a group
    group,

    -- * New group
    newGroup,

    -- * New Subgroup
    newSubGroup,

    -- * Update group
    updateGroup,

    -- * Remove group
    removeGroup,

    -- * Search for group
    searchGroup,

    -- * Group attributes
    ListGroupsAttrs (..),
    GroupOrderBy (..),
    GroupProjectAttrs (..),
    GroupProjectOrderBy (..),
    GroupAttrs (..),
    BranchProtection (..),
    defaultGroupFilters,
    defaultListGroupsFilters,
    defaultGroupProjectFilters,
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

-- | Get a list of visible groups for the authenticated user.
groups :: ListGroupsAttrs -> GitLab [Group]
groups attrs =
  fromRight (error "groups error")
    <$> gitlabGetMany "/groups" (listGroupsAttrs attrs)

-- | Get a list of visible direct subgroups in this group.
subGroups :: Group -> ListGroupsAttrs -> GitLab [Group]
subGroups parentGrp attrs =
  fromRight (error "subGroups error")
    <$> gitlabGetMany
      ( "/groups/"
          <> T.pack (show (group_id parentGrp))
          <> "/subgroups"
      )
      (listGroupsAttrs attrs)

-- | Get a list of visible descendant groups of this group.
descendantGroups :: Group -> ListGroupsAttrs -> GitLab [Group]
descendantGroups parentGrp attrs =
  fromRight (error "subGroups error")
    <$> gitlabGetMany
      ( "/groups/"
          <> T.pack (show (group_id parentGrp))
          <> "/descendant_groups"
      )
      (listGroupsAttrs attrs)

-- | Get a list of projects in this group.
groupProjects :: Group -> GroupProjectAttrs -> GitLab [Project]
groupProjects parentGrp attrs =
  fromRight (error "groupProjects error")
    <$> gitlabGetMany
      ( "/groups/"
          <> T.pack (show (group_id parentGrp))
          <> "/projects"
      )
      (groupProjectAttrs attrs)

-- | Get a list of projects in this group.
groupSharedProjects :: Group -> GroupProjectAttrs -> GitLab [Project]
groupSharedProjects parentGrp attrs =
  fromRight (error "groupSharedProjects error")
    <$> gitlabGetMany
      ( "/groups/"
          <> T.pack (show (group_id parentGrp))
          <> "/projects/shared"
      )
      (groupProjectAttrs attrs)

-- | Get all details of a group.
group ::
  -- | group ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Group))
group pId = do
  gitlabGetOne urlPath []
  where
    urlPath =
      "/groups/"
        <> T.pack (show pId)

-- | Creates a new project group (TODO include attributes).
newGroup ::
  -- | group name
  Text ->
  -- | group path
  Text ->
  -- | group attributes
  GroupAttrs ->
  GitLab (Either (Response BSL.ByteString) (Maybe Group))
newGroup nameTxt pathTxt attrs = do
  gitlabPost
    newProjectAddr
    ([("name", Just (T.encodeUtf8 nameTxt)), ("path", Just (T.encodeUtf8 pathTxt))] <> groupAttrs attrs)
  where
    newProjectAddr :: Text
    newProjectAddr =
      "/groups"

-- | Creates a new project group.
newSubGroup ::
  -- | group name
  Text ->
  -- | group path
  Text ->
  -- | parent group ID
  Int ->
  -- | group attributes
  GroupAttrs ->
  GitLab (Either (Response BSL.ByteString) (Maybe Group))
newSubGroup nameTxt pathTxt parentId attrs = do
  gitlabPost
    newProjectAddr
    ([("name", Just (T.encodeUtf8 nameTxt)), ("path", Just (T.encodeUtf8 pathTxt)), ("parent_id", Just (T.encodeUtf8 (T.pack (show parentId))))] <> groupAttrs attrs)
  where
    newProjectAddr :: Text
    newProjectAddr =
      "/groups"

-- | Updates the project group. Only available to group owners and
-- administrators.
updateGroup ::
  -- | The ID of the group.
  Int ->
  -- | Group attributes
  GroupAttrs ->
  GitLab (Either (Response BSL.ByteString) (Maybe Group))
updateGroup groupId attrs =
  gitlabPut
    newProjectAddr
    (groupAttrs attrs)
  where
    newProjectAddr :: Text
    newProjectAddr =
      "/groups/"
        <> T.pack (show groupId)

-- | Only available to group owners and administrators.
removeGroup ::
  -- | The ID of the group.
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
removeGroup grpId =
  gitlabDelete groupAddr []
  where
    groupAddr :: Text
    groupAddr =
      "/groups/"
        <> T.pack (show grpId)

-- | Get all groups that match your string in their name or path.
searchGroup ::
  -- | String or path to search for.
  Text ->
  GitLab [Group]
searchGroup searchTxt =
  fromRight (error "searchGroups error")
    <$> gitlabGetMany "/groups" [("search", Just (T.encodeUtf8 searchTxt))]

-- | Attributes related to a group
data GroupProjectAttrs = GroupProjectAttrs
  { groupProjectFilter_id :: Maybe Int,
    groupProjectFilter_archived :: Maybe Bool,
    groupProjectFilter_visibility :: Maybe Visibility,
    groupProjectFilter_order_by :: Maybe GroupProjectOrderBy,
    groupProjectFilter_sort :: Maybe SortBy,
    groupProjectFilter_search :: Maybe Text,
    groupProjectFilter_simple :: Maybe Bool,
    groupProjectFilter_owned :: Maybe Bool,
    groupProjectFilter_starred :: Maybe Bool,
    groupProjectFilter_with_issues_enabled :: Maybe Bool,
    groupProjectFilter_with_merge_requests_enabled :: Maybe Bool,
    groupProjectFilter_with_shared :: Maybe Bool,
    groupProjectFilter_include_subgroups :: Maybe Bool,
    groupProjectFilter_min_access_level :: Maybe AccessLevel,
    groupProjectFilter_with_custom_attributes :: Maybe Bool,
    groupProjectFilter_with_security_reports :: Maybe Bool
  }

-- | No group filters applied, thereby returning all groups.
defaultGroupProjectFilters :: GroupProjectAttrs
defaultGroupProjectFilters =
  GroupProjectAttrs Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | The order of group projects in search results.
data GroupProjectOrderBy
  = GroupProjectOrderId
  | GroupProjectOrderName
  | GroupProjectOrderPath
  | GroupProjectOrderCreatedAt
  | GroupProjectOrderUpdatedAt
  | GroupProjectOrderSimilarity
  | GroupProjectOrderLastActivityAt

instance Show GroupProjectOrderBy where
  show GroupProjectOrderName = "id"
  show GroupProjectOrderPath = "name"
  show GroupProjectOrderId = "path"
  show GroupProjectOrderCreatedAt = "created_at"
  show GroupProjectOrderUpdatedAt = "updated_at"
  show GroupProjectOrderSimilarity = "similarity"
  show GroupProjectOrderLastActivityAt = "last_activity_at"

groupProjectAttrs :: GroupProjectAttrs -> [GitLabParam]
groupProjectAttrs filters =
  catMaybes
    [ (\i -> Just ("id", textToBS (T.pack (show i)))) =<< groupProjectFilter_id filters,
      (\b -> Just ("archived", textToBS (showBool b))) =<< groupProjectFilter_archived filters,
      (\v -> Just ("visibility", textToBS (T.pack (show v)))) =<< groupProjectFilter_search filters,
      (\grpOrder -> Just ("order_by", textToBS (T.pack (show grpOrder)))) =<< groupProjectFilter_order_by filters,
      (\sortBy -> Just ("sort", textToBS (T.pack (show sortBy)))) =<< groupProjectFilter_sort filters,
      (\x -> Just ("search", textToBS (T.pack (show x)))) =<< groupProjectFilter_search filters,
      (\b -> Just ("simple", textToBS (showBool b))) =<< groupProjectFilter_simple filters,
      (\b -> Just ("owned", textToBS (showBool b))) =<< groupProjectFilter_owned filters,
      (\b -> Just ("starred", textToBS (showBool b))) =<< groupProjectFilter_starred filters,
      (\b -> Just ("with_issues_enabled", textToBS (showBool b))) =<< groupProjectFilter_with_issues_enabled filters,
      (\b -> Just ("with_merge_requests_enabled", textToBS (showBool b))) =<< groupProjectFilter_with_merge_requests_enabled filters,
      (\b -> Just ("with_shared", textToBS (showBool b))) =<< groupProjectFilter_with_shared filters,
      (\b -> Just ("include_subgroups", textToBS (showBool b))) =<< groupProjectFilter_include_subgroups filters,
      (\x -> Just ("min_access_level", textToBS (T.pack (show x)))) =<< groupProjectFilter_min_access_level filters,
      (\b -> Just ("with_custom_attributes", textToBS (showBool b))) =<< groupProjectFilter_with_custom_attributes filters,
      (\b -> Just ("with_security_reports", textToBS (showBool b))) =<< groupProjectFilter_with_security_reports filters
    ]
  where
    textToBS = Just . T.encodeUtf8
    showBool :: Bool -> Text
    showBool True = "true"
    showBool False = "false"

-- | Attributes related to listing groups
data ListGroupsAttrs = ListGroupsAttrs
  { listGroupsFilter_skip_groups :: Maybe [Int],
    listGroupsFilter_all_available :: Maybe Bool,
    listGroupsFilter_search :: Maybe Text,
    listGroupsFilter_order_by :: Maybe GroupOrderBy,
    listGroupsFilter_sort :: Maybe SortBy,
    listGroupsFilter_owned :: Maybe Bool,
    listGroupsFilter_min_access_level :: Maybe AccessLevel,
    listGroupsFilter_top_level_only :: Maybe Bool
  }

-- | The order of groups in search results.
data GroupOrderBy
  = GroupOrderName
  | GroupOrderPath
  | GroupOrderId
  | GroupOrderSimilarity

instance Show GroupOrderBy where
  show GroupOrderName = "name"
  show GroupOrderPath = "path"
  show GroupOrderId = "id"
  show GroupOrderSimilarity = "similarity"

listGroupsAttrs :: ListGroupsAttrs -> [GitLabParam]
listGroupsAttrs filters =
  catMaybes
    [ (\i -> Just ("skip_groups", textToBS (T.pack (show i)))) =<< listGroupsFilter_skip_groups filters,
      (\b -> Just ("all_available", textToBS (showBool b))) =<< listGroupsFilter_all_available filters,
      (\t -> Just ("search", textToBS t)) =<< listGroupsFilter_search filters,
      (\grpOrder -> Just ("order_by", textToBS (T.pack (show grpOrder)))) =<< listGroupsFilter_order_by filters,
      (\sortBy -> Just ("sort", textToBS (T.pack (show sortBy)))) =<< listGroupsFilter_sort filters,
      (\b -> Just ("owned", textToBS (showBool b))) =<< listGroupsFilter_owned filters,
      (\accLevel -> Just ("min_access_level", textToBS (T.pack (show accLevel)))) =<< listGroupsFilter_min_access_level filters,
      (\b -> Just ("top_level_only", textToBS (showBool b))) =<< listGroupsFilter_top_level_only filters
    ]
  where
    textToBS = Just . T.encodeUtf8
    showBool :: Bool -> Text
    showBool True = "true"
    showBool False = "false"

-- | No group filters applied, thereby returning all groups.
defaultListGroupsFilters :: ListGroupsAttrs
defaultListGroupsFilters =
  ListGroupsAttrs Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Attributes related to a group
data GroupAttrs = GroupAttrs
  { -- | The name of the group.
    groupFilter_name :: Maybe Text,
    -- | The path of the group.
    groupFilter_path :: Maybe Text,
    -- | Default to Auto DevOps pipeline for all projects within this
    -- group.
    groupFilter_auto_devops_enabled :: Maybe Bool,
    -- | Default to the global level default branch protection
    -- setting.
    groupFilter_default_branch_protection :: Maybe BranchProtection,
    -- | The group’s description.
    groupFilter_description :: Maybe Text,
    -- | Disable email notifications.
    groupFilter_emails_disabled :: Maybe Bool,
    -- | Enable/disable Large File Storage (LFS) for the projects in
    -- this group.
    groupFilter_lfs_enabled :: Maybe Bool,
    -- | Disable the capability of a group from getting mentioned.
    groupFilter_mentions_disabled :: Maybe Bool,
    -- | The parent group ID for creating nested group.
    groupFilter_parent_id :: Maybe Int,
    -- | Determine if developers can create projects in the group. Can
    -- be noone (No one), maintainer (users with the Maintainer role),
    -- or developer (users with the Developer or Maintainer role).
    groupFilter_project_creation_level :: Maybe AccessLevel,
    -- | Allow users to request member access.
    groupFilter_request_access_enabled :: Maybe Bool,
    -- | Require all users in this group to setup Two-factor
    -- authentication.
    groupFilter_require_two_factor_authentication :: Maybe Bool,
    -- | Prevent sharing a project with another group within this
    -- group.
    groupFilter_share_with_group_lock :: Maybe Bool,
    -- | Allowed to create subgroups. Can be owner (Owners), or
    -- maintainer (users with the Maintainer role).
    groupFilter_subgroup_creation_level :: Maybe AccessLevel,
    -- | Time before Two-factor authentication is enforced (in hours).
    groupFilter_two_factor_grace_period :: Maybe Int,
    -- | The group’s visibility. Can be private, internal, or public.
    groupFilter_visibility :: Maybe Visibility
  }

-- | A group level branch protection setting.
data BranchProtection
  = -- | Users with the Developer or Maintainer role can: push new
    -- commits, force push changes, delete the branch
    NoProtection
  | -- | Users with the Developer or Maintainer role can: push new commits
    PartialProtection
  | -- | Only users with the Maintainer role can: push new commits
    FullProtection
  | -- | Users with the Maintainer role can: push new commits, force
    -- push changes, accept merge requests; Users with the Developer
    -- role can: accept merge requests
    ProtectAgainstPushes

instance Show BranchProtection where
  show NoProtection = "0"
  show PartialProtection = "1"
  show FullProtection = "2"
  show ProtectAgainstPushes = "3"

groupAttrs :: GroupAttrs -> [GitLabParam]
groupAttrs filters =
  catMaybes
    [ (\t -> Just ("name", textToBS t)) =<< groupFilter_name filters,
      (\t -> Just ("path", textToBS t)) =<< groupFilter_path filters,
      (\b -> Just ("auto_devops_enabled", textToBS (showBool b))) =<< groupFilter_auto_devops_enabled filters,
      (\a -> Just ("default_branch_protection", textToBS (T.pack (show a)))) =<< groupFilter_default_branch_protection filters,
      (\t -> Just ("description", textToBS t)) =<< groupFilter_description filters,
      (\b -> Just ("emails_disabled", textToBS (showBool b))) =<< groupFilter_emails_disabled filters,
      (\b -> Just ("lfs_enabled", textToBS (showBool b))) =<< groupFilter_lfs_enabled filters,
      (\b -> Just ("mentions_disabled", textToBS (showBool b))) =<< groupFilter_mentions_disabled filters,
      (\i -> Just ("parent_id", textToBS (T.pack (show i)))) =<< groupFilter_parent_id filters,
      (\a -> Just ("project_creation_level", textToBS (T.pack (show a)))) =<< groupFilter_project_creation_level filters,
      (\b -> Just ("request_access_enabled", textToBS (showBool b))) =<< groupFilter_request_access_enabled filters,
      (\b -> Just ("require_two_factor_authentication", textToBS (showBool b))) =<< groupFilter_require_two_factor_authentication filters,
      (\b -> Just ("share_with_group_lock", textToBS (showBool b))) =<< groupFilter_share_with_group_lock filters,
      (\a -> Just ("subgroup_creation_level", textToBS (T.pack (show a)))) =<< groupFilter_subgroup_creation_level filters,
      (\i -> Just ("two_factor_grace_period", textToBS (T.pack (show i)))) =<< groupFilter_two_factor_grace_period filters,
      (\a -> Just ("visibility", textToBS (T.pack (show a)))) =<< groupFilter_visibility filters
    ]
  where
    textToBS = Just . T.encodeUtf8
    showBool :: Bool -> Text
    showBool True = "true"
    showBool False = "false"

-- | No group filters applied.
defaultGroupFilters ::
  GroupAttrs
defaultGroupFilters =
  GroupAttrs Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
