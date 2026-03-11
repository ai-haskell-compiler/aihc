{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Issues
-- Description : Queries about issues created against projects
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Issues
  ( -- * List issues

    -- * List group issues
    groupIssues,

    -- * List project issues
    projectIssues,

    -- * Single issue
    issue,

    -- * User issues
    userIssues,

    -- * Single project issue
    projectIssue,

    -- * New issue
    newIssue,
    newIssue',

    -- * Edit issue
    editIssue,

    -- * Delete an issue
    deleteIssue,

    -- * Reorder an issue
    reorderIssue,

    -- * Move an issue
    moveIssue,

    -- * Clone an issue
    cloneIssue,

    -- * Subscribe to an issue
    subscribeIssue,

    -- * Unsubscribe from an issue
    unsubscribeIssue,

    -- * Create a to-do item
    createTodo,

    -- * List merge requests related to issue
    issueMergeRequests,

    -- * List merge requests that close a particular issue on merge
    issueMergeRequestsThatClose,

    -- * Participants on issues
    issueParticipants,

    -- * Comments on issues

    -- * Get issues statistics
    issueStatisticsUser,

    -- * Get group issues statistics
    issueStatisticsGroup,

    -- * Get project issues statistics
    issueStatisticsProject,

    -- * Issues attributes
    defaultIssueFilters,
    defaultIssueAttrs,
    IssueAttrs (..),
    DueDate (..),
    IssueState (..),
  )
where

import Data.Aeson.TH
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

-- | Get a list of a project’s issues
groupIssues ::
  -- | the group
  Group ->
  -- | filter the issues, see https://docs.gitlab.com/ee/api/issues.html#list-issues
  IssueFilterAttrs ->
  -- the GitLab issues
  GitLab [Issue]
groupIssues grp attrs = do
  result <- gitlabGetMany urlPath (issueFilters attrs)
  return (fromRight (error "groupsIssues error") result)
  where
    urlPath =
      T.pack $
        "/groups/"
          <> show (group_id grp)
          <> "/issues"

-- result <- projectIssues' (project_id p) filters
-- return (fromRight (error "projectIssues error") result)

-- | Get a list of a project’s issues
projectIssues ::
  -- | the project
  Project ->
  -- | filter the issues, see https://docs.gitlab.com/ee/api/issues.html#list-issues
  IssueFilterAttrs ->
  -- the GitLab issues
  GitLab [Issue]
projectIssues p filters = do
  result <- projectIssues' (project_id p) filters
  return (fromRight (error "projectIssues error") result)

-- | Get a list of a project’s issues
projectIssues' ::
  -- | the project ID
  Int ->
  -- | filter the issues, see https://docs.gitlab.com/ee/api/issues.html#list-issues
  IssueFilterAttrs ->
  -- | the GitLab issues
  GitLab (Either (Response BSL.ByteString) [Issue])
projectIssues' projectId attrs =
  gitlabGetMany urlPath (issueFilters attrs)
  where
    urlPath =
      T.pack $
        "/projects/"
          <> show projectId
          <> "/issues"

-- | Only for administrators. Get a single issue.
issue ::
  -- | issue ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Issue))
issue issId =
  gitlabGetOne urlPath []
  where
    urlPath =
      T.pack
        "/issues/"
        <> T.pack (show issId)

-- | gets all issues create by a user.
userIssues ::
  -- | the user
  User ->
  GitLab [Issue]
userIssues usr =
  fromRight (error "userIssues error") <$> gitlabGetMany addr params
  where
    addr = "/issues"
    params :: [GitLabParam]
    params =
      [ ("author_id", Just (T.encodeUtf8 (T.pack (show (user_id usr))))),
        ("scope", Just "all")
      ]

-- | Get a single project issue. If the project is private or the
-- issue is confidential, you need to provide credentials to
-- authorize.
projectIssue ::
  -- | Project
  Project ->
  -- | issue ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Issue))
projectIssue p issId =
  gitlabGetOne urlPath []
  where
    urlPath =
      T.pack
        "/projects/"
        <> T.pack (show (project_id p))
        <> "/issues/"
        <> T.pack (show issId)

-- | create a new issue.
newIssue ::
  -- | project
  Project ->
  -- | issue title
  Text ->
  -- | issue description
  Text ->
  -- | issue attributes
  IssueAttrs ->
  GitLab (Either (Response BSL.ByteString) (Maybe Issue))
newIssue project =
  newIssue' (project_id project)

-- | create a new issue.
newIssue' ::
  -- | project ID
  Int ->
  -- | issue title
  Text ->
  -- | issue description
  Text ->
  -- | issue attributes
  IssueAttrs ->
  GitLab (Either (Response BSL.ByteString) (Maybe Issue))
newIssue' projectId issueTitle issueDescription attrs =
  gitlabPost addr dataBody
  where
    dataBody :: [GitLabParam]
    dataBody =
      [ ("title", Just (T.encodeUtf8 issueTitle)),
        ("description", Just (T.encodeUtf8 issueDescription))
      ]
        <> issueAttrs projectId attrs
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/issues"

-- | edits an issue. see <https://docs.gitlab.com/ee/api/issues.html#edit-issue>
editIssue ::
  Project ->
  -- | issue ID
  IssueId ->
  -- | issue attributes
  IssueAttrs ->
  GitLab (Either (Response BSL.ByteString) Issue)
editIssue prj issueId editIssueReq = do
  let urlPath =
        "/projects/"
          <> T.pack (show (project_id prj))
          <> "/issues/"
          <> T.pack (show issueId)
  result <-
    gitlabPut
      urlPath
      (issueAttrs (project_id prj) editIssueReq)
  case result of
    Left resp -> return (Left resp)
    Right Nothing -> error "editIssue error"
    Right (Just iss) -> return (Right iss)

-- | deletes an issue. see <https://docs.gitlab.com/ee/api/issues.html#delete-an-issue>
deleteIssue ::
  Project ->
  -- | issue ID
  IssueId ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteIssue prj issueId = do
  gitlabDelete issueAddr []
  where
    issueAddr :: Text
    issueAddr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/issues/"
        <> T.pack (show issueId)

-- | edits an issue. see <https://docs.gitlab.com/ee/api/issues.html#edit-issue>
reorderIssue ::
  Project ->
  -- | issue ID
  IssueId ->
  -- | The ID of a project’s issue that should be placed after this
  -- issue
  Int ->
  -- | The ID of a project’s issue that should be placed before this
  -- issue
  Int ->
  GitLab (Either (Response BSL.ByteString) Issue)
reorderIssue prj issueId moveAfterId moveBeforeId = do
  let urlPath =
        "/projects/"
          <> T.pack (show (project_id prj))
          <> "/issues/"
          <> T.pack (show issueId)
          <> "/reorder"
  result <-
    gitlabPut
      urlPath
      [ ("move_after_id", Just (T.encodeUtf8 (T.pack (show moveAfterId)))),
        ("move_before_id", Just (T.encodeUtf8 (T.pack (show moveBeforeId))))
      ]
  case result of
    Left resp -> return (Left resp)
    Right Nothing -> error "reorderIssue error"
    Right (Just iss) -> return (Right iss)

-- | Moves an issue to a different project. If a given label or
-- milestone with the same name also exists in the target project,
-- it’s then assigned to the issue being moved.
moveIssue ::
  -- | project
  Project ->
  -- | The internal ID of a project’s issue
  IssueId ->
  -- | The ID of the new project
  ProjectId ->
  GitLab (Either (Response BSL.ByteString) (Maybe Issue))
moveIssue prj issueId toPrjId =
  gitlabPost addr dataBody
  where
    dataBody :: [GitLabParam]
    dataBody =
      [ ("to_project_id", Just (T.encodeUtf8 (T.pack (show toPrjId))))
      ]
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/issues/"
        <> T.pack (show issueId)
        <> "/move"

-- | Clone the issue to given project. Copies as much data as possible
-- as long as the target project contains equivalent labels,
-- milestones, and so on.
cloneIssue ::
  -- | project
  Project ->
  -- | The internal ID of a project’s issue
  IssueId ->
  -- | The ID of the new project
  ProjectId ->
  GitLab (Either (Response BSL.ByteString) (Maybe Issue))
cloneIssue prj issueId toPrjId =
  gitlabPost addr dataBody
  where
    dataBody :: [GitLabParam]
    dataBody =
      [ ("to_project_id", Just (T.encodeUtf8 (T.pack (show toPrjId))))
      ]
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/issues/"
        <> T.pack (show issueId)
        <> "/clone"

-- | Subscribes the authenticated user to an issue to receive
-- notifications.
subscribeIssue ::
  -- | project
  Project ->
  -- | The internal ID of a project’s issue
  IssueId ->
  GitLab (Either (Response BSL.ByteString) (Maybe Issue))
subscribeIssue prj issueId =
  gitlabPost addr dataBody
  where
    dataBody :: [GitLabParam]
    dataBody =
      []
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/issues/"
        <> T.pack (show issueId)
        <> "/subscribe"

-- | Unsubscribes the authenticated user from the issue to not receive
-- notifications from it.
unsubscribeIssue ::
  -- | project
  Project ->
  -- | The internal ID of a project’s issue
  IssueId ->
  GitLab (Either (Response BSL.ByteString) (Maybe Issue))
unsubscribeIssue prj issueId =
  gitlabPost addr dataBody
  where
    dataBody :: [GitLabParam]
    dataBody =
      []
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/issues/"
        <> T.pack (show issueId)
        <> "/unsubscribe"

-- | Get all the merge requests that are related to the issue.
createTodo ::
  -- | project
  Project ->
  -- | The internal ID of a project’s issue
  IssueId ->
  GitLab (Either (Response BSL.ByteString) (Maybe Todo))
createTodo prj issueId =
  gitlabPost addr dataBody
  where
    dataBody :: [GitLabParam]
    dataBody =
      []
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/issues/"
        <> T.pack (show issueId)
        <> "/todo"

-- | Get all the merge requests that are related to the issue.
issueMergeRequests ::
  -- | project
  Project ->
  -- | The internal ID of a project’s issue
  IssueId ->
  GitLab (Either (Response BSL.ByteString) [MergeRequest])
issueMergeRequests prj issueId = do
  gitlabGetMany urlPath []
  where
    urlPath =
      T.pack $
        "/projects/"
          <> show (project_id prj)
          <> "/issues/"
          <> show issueId
          <> "/related_merge_requests"

-- | get all merge requests that close a particular issue when merged.
issueMergeRequestsThatClose ::
  -- | project
  Project ->
  -- | The internal ID of a project’s issue
  IssueId ->
  GitLab (Either (Response BSL.ByteString) [MergeRequest])
issueMergeRequestsThatClose prj issueId = do
  gitlabGetMany urlPath []
  where
    urlPath =
      T.pack $
        "/projects/"
          <> show (project_id prj)
          <> "/issues/"
          <> show issueId
          <> "/closed_by"

-- | get all merge requests that close a particular issue when merged.
issueParticipants ::
  -- | project
  Project ->
  -- | The internal ID of a project’s issue
  IssueId ->
  GitLab (Either (Response BSL.ByteString) [User])
issueParticipants prj issueId = do
  gitlabGetMany urlPath []
  where
    urlPath =
      T.pack $
        "/projects/"
          <> show (project_id prj)
          <> "/issues/"
          <> show issueId
          <> "/participants"

-- | Gets issues count statistics on all issues the authenticated user has access to.
issueStatisticsUser ::
  -- | filter the issues, see https://docs.gitlab.com/ee/api/issues_statistics.html#get-issues-statistics
  IssueFilterAttrs ->
  -- | the issue statistics
  GitLab IssueStatistics
issueStatisticsUser attrs =
  gitlabUnsafe (gitlabGetOne urlPath (issueFilters attrs))
  where
    urlPath =
      T.pack
        "/issues_statistics"

-- | Gets issues count statistics for a given group.
issueStatisticsGroup ::
  -- | the group
  Group ->
  -- | filter the issues, see https://docs.gitlab.com/ee/api/issues_statistics.html#get-issues-statistics
  IssueFilterAttrs ->
  -- | the issue statistics
  GitLab IssueStatistics
issueStatisticsGroup group filters = do
  result <- issueStatisticsGroup' (group_id group) filters
  case result of
    Left _s -> error "issueStatisticsGroup error"
    Right Nothing -> error "issueStatisticsGroup error"
    Right (Just stats) -> return stats

-- | Gets issues count statistics for a given group.
issueStatisticsGroup' ::
  -- | the group ID
  Int ->
  -- | filter the issues, see https://docs.gitlab.com/ee/api/issues_statistics.html#get-issues-statistics
  IssueFilterAttrs ->
  -- | the issue statistics
  GitLab (Either (Response BSL.ByteString) (Maybe IssueStatistics))
issueStatisticsGroup' groupId attrs =
  gitlabGetOne urlPath (issueFilters attrs)
  where
    urlPath =
      T.pack $
        "/groups/"
          <> show groupId
          <> "/issues_statistics"

-- | Gets issues count statistics for a given group.
issueStatisticsProject ::
  -- | the project
  Project ->
  -- | filter the issues, see https://docs.gitlab.com/ee/api/issues_statistics.html#get-issues-statistics
  IssueFilterAttrs ->
  -- | the issue statistics
  GitLab IssueStatistics
issueStatisticsProject proj filters = do
  result <- issueStatisticsProject' (project_id proj) filters
  case result of
    Left _s -> error "issueStatisticsProject error"
    Right Nothing -> error "issueStatisticsProject error"
    Right (Just stats) -> return stats

-- | Gets issues count statistics for a given project.
issueStatisticsProject' ::
  -- | the project ID
  Int ->
  -- | filter the issues, see https://docs.gitlab.com/ee/api/issues_statistics.html#get-issues-statistics
  IssueFilterAttrs ->
  -- | the issue statistics
  GitLab (Either (Response BSL.ByteString) (Maybe IssueStatistics))
issueStatisticsProject' projId attrs =
  gitlabGetOne urlPath (issueFilters attrs)
  where
    urlPath =
      T.pack $
        "/projects/"
          <> show projId
          <> "/issues_statistics"

-- | issue attributes.
data IssueAttrs = IssueAttrs
  { set_issue_id :: ProjectId,
    set_issue_title :: Maybe Text,
    set_issue_description :: Maybe Text,
    set_issue_confidential :: Maybe Bool,
    set_issue_assignee_id :: Maybe Int,
    set_issue_assignee_ids :: Maybe [Int],
    set_issue_milestone_id :: Maybe Int,
    set_issue_labels :: Maybe [Text],
    set_issue_state_event :: Maybe Text,
    set_issue_updated_at :: Maybe UTCTime,
    set_issue_due_date :: Maybe UTCTime,
    set_issue_weight :: Maybe Int,
    set_issue_discussion_locked :: Maybe Bool,
    set_issue_epic_id :: Maybe Int,
    set_issue_epic_iid :: Maybe Int
  }
  deriving (Show)

-- | Attributes related to a project issue
data IssueFilterAttrs = IssueFilterAttrs
  { issueFilter_assignee_id :: Maybe Int,
    issueFilter_assignee_username :: Maybe String,
    issueFilter_author_id :: Maybe Int,
    issueFilter_author_username :: Maybe String,
    issueFilter_confidential :: Maybe Bool,
    issueFilter_created_after :: Maybe UTCTime,
    issueFilter_created_before :: Maybe UTCTime,
    issueFilter_due_date :: Maybe DueDate,
    issueFilter_iids :: Maybe Int,
    issueFilter_in :: Maybe SearchIn,
    issueFilter_iteration_id :: Maybe Int,
    issueFilter_iteration_title :: Maybe String,
    issueFilter_milestone :: Maybe String,
    issueFilter_labels :: Maybe String,
    issueFilter_my_reaction_emoji :: Maybe String,
    issueFilter_non_archived :: Maybe Bool,
    issueFilter_order_by :: Maybe OrderBy,
    issueFilter_scope :: Maybe Scope,
    issueFilter_search :: Maybe String,
    issueFilter_sort :: Maybe SortBy,
    issueFilter_state :: Maybe IssueState,
    issueFilter_updated_after :: Maybe UTCTime,
    issueFilter_updated_before :: Maybe UTCTime,
    issueFilter_with_labels_details :: Maybe Bool
  }

issueAttrs :: Int -> IssueAttrs -> [GitLabParam]
issueAttrs prjId filters =
  catMaybes $
    [ Just ("id", textToBS (T.pack (show prjId))),
      (\i -> Just ("assignee_id", textToBS (T.pack (show i)))) =<< set_issue_assignee_id filters,
      (\t -> Just ("title", textToBS t)) =<< set_issue_title filters,
      (\t -> Just ("description", textToBS t)) =<< set_issue_description filters,
      (\b -> Just ("confidential", textToBS (showBool b))) =<< set_issue_confidential filters,
      (\i -> Just ("milestone_id", textToBS (T.pack (show i)))) =<< set_issue_milestone_id filters,
      (\ts -> Just ("labels", textToBS (T.intercalate (T.pack ",") ts))) =<< set_issue_labels filters,
      (\t -> Just ("state_event", textToBS t)) =<< set_issue_state_event filters,
      (\d -> Just ("updated_at", stringToBS (show d))) =<< set_issue_updated_at filters,
      (\d -> Just ("due_date", stringToBS (show d))) =<< set_issue_due_date filters,
      (\i -> Just ("weight", textToBS (T.pack (show i)))) =<< set_issue_weight filters,
      (\b -> Just ("discussion_locked", textToBS (showBool b))) =<< set_issue_discussion_locked filters,
      (\i -> Just ("epic_id", textToBS (T.pack (show i)))) =<< set_issue_epic_id filters,
      (\i -> Just ("epic_iid", textToBS (T.pack (show i)))) =<< set_issue_epic_iid filters
    ]
      <> case set_issue_assignee_ids filters of
        Nothing -> []
        Just ids ->
          map
            (\i -> Just ("assignee_ids[]", stringToBS (show i)))
            ids
  where
    -- <> (\is -> Just ("assignee_ids", arrayToBS is))
    -- =<< set_issue_assignee_ids filters

    textToBS = Just . T.encodeUtf8
    stringToBS = Just . T.encodeUtf8 . T.pack
    showBool :: Bool -> Text
    showBool True = "true"
    showBool False = "false"

issueFilters :: IssueFilterAttrs -> [GitLabParam]
issueFilters filters =
  catMaybes
    [ (\i -> Just ("assignee_id", textToBS (T.pack (show i)))) =<< issueFilter_assignee_id filters,
      (\t -> Just ("assignee_username", textToBS (T.pack t))) =<< issueFilter_assignee_username filters,
      (\i -> Just ("author_id", textToBS (T.pack (show i)))) =<< issueFilter_author_id filters,
      (\i -> Just ("author_username", textToBS (T.pack (show i)))) =<< issueFilter_author_username filters,
      (\b -> Just ("confidential", textToBS (showBool b))) =<< issueFilter_confidential filters,
      (\t -> Just ("created_after", textToBS (showTime t))) =<< issueFilter_created_after filters,
      (\t -> Just ("created_before", textToBS (showTime t))) =<< issueFilter_created_before filters,
      (\due -> Just ("due_date", textToBS (T.pack (show due)))) =<< issueFilter_due_date filters,
      (\iids -> Just ("iids[]", textToBS (T.pack (show iids)))) =<< issueFilter_iids filters,
      (\issueIn -> Just ("assignee_id", textToBS (T.pack (show issueIn)))) =<< issueFilter_in filters,
      (\i -> Just ("iteration_id", textToBS (T.pack (show i)))) =<< issueFilter_iteration_id filters,
      (\s -> Just ("iteration_title", textToBS (T.pack s))) =<< issueFilter_iteration_title filters,
      (\s -> Just ("milestone", textToBS (T.pack s))) =<< issueFilter_milestone filters,
      (\s -> Just ("labels", textToBS (T.pack s))) =<< issueFilter_labels filters,
      (\s -> Just ("my_reaction_emoji", textToBS (T.pack s))) =<< issueFilter_my_reaction_emoji filters,
      (\b -> Just ("non_archived", textToBS (showBool b))) =<< issueFilter_non_archived filters,
      (\x -> Just ("order_by", textToBS (T.pack (show x)))) =<< issueFilter_order_by filters,
      (\x -> Just ("scope", textToBS (T.pack (show x)))) =<< issueFilter_scope filters,
      (\s -> Just ("search", textToBS (T.pack s))) =<< issueFilter_search filters,
      (\x -> Just ("sort", textToBS (T.pack (show x)))) =<< issueFilter_sort filters,
      (\x -> Just ("state", textToBS (T.pack (show x)))) =<< issueFilter_state filters,
      (\t -> Just ("updated_after", textToBS (showTime t))) =<< issueFilter_updated_after filters,
      (\t -> Just ("updated_before", textToBS (showTime t))) =<< issueFilter_updated_before filters,
      (\b -> Just ("with_labels_details", textToBS (showBool b))) =<< issueFilter_with_labels_details filters
    ]
  where
    textToBS = Just . T.encodeUtf8
    showBool :: Bool -> Text
    showBool True = "true"
    showBool False = "false"
    showTime :: UTCTime -> Text
    showTime = T.pack . iso8601Show

-- | When an issue is due
data DueDate
  = NoDueDate
  | Overdue
  | Week
  | Month
  | NextMonthPreviousTwoWeeks

instance Show DueDate where
  show NoDueDate = "0"
  show Overdue = "overdue"
  show Week = "week"
  show Month = "month"
  show NextMonthPreviousTwoWeeks = "next_month_and_previous_two_weeks"

-- | Is a project issues open or closed
data IssueState
  = IssueOpen
  | IssueClosed

instance Show IssueState where
  show IssueOpen = "opened"
  show IssueClosed = "closed"

-- | No issue filters, thereby returning all issues. Default scope is "all".
defaultIssueFilters :: IssueFilterAttrs
defaultIssueFilters =
  IssueFilterAttrs Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just All) Nothing Nothing Nothing Nothing Nothing Nothing

-- | issue attributes when creating or editing issues.
defaultIssueAttrs ::
  -- | project ID
  Int ->
  IssueAttrs
defaultIssueAttrs prjId =
  IssueAttrs prjId Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "set_issue_"), omitNothingFields = True} ''IssueAttrs)
