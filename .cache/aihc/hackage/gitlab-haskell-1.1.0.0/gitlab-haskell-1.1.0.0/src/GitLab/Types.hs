{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : GitLab.Types
-- Description : Haskell records corresponding to JSON data from GitLab API calls
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.Types
  ( GitLab,
    GitLabT (..),
    GitLabState (..),
    GitLabServerConfig (..),
    AuthMethod (..),
    defaultGitLabServer,
    ArchiveFormat (..),
    AccessLevel (..),
    SearchIn (..),
    Scope (..),
    SortBy (..),
    OrderBy (..),
    Member (..),
    SamlIdentity (..),
    Identity (..),
    Namespace (..),
    Links (..),
    Owner (..),
    Permissions (..),
    ProjectId,
    Project (..),
    Statistics (..),
    User (..),
    Milestone (..),
    MilestoneState (..),
    TimeStats (..),
    IssueId,
    Issue (..),
    Epic (..),
    Pipeline (..),
    Commit (..),
    CommitTodo (..),
    CommitStats (..),
    Contributor (..),
    Tag (..),
    Release (..),
    Diff (..),
    Repository (..),
    Job (..),
    Artifact (..),
    Group (..),
    GroupShare (..),
    Branch (..),
    RepositoryFile (..),
    RepositoryFileBlame (..),
    RepositoryFileSimple (..),
    MergeRequest (..),
    Todo (..),
    TodoProject (..),
    TodoAction (..),
    TodoTarget (..),
    TodoTargetType (..),
    TodoType (..),
    TodoState (..),
    Version (..),
    URL,
    EditIssueReq (..),
    Discussion (..),
    CommitNote (..),
    Note (..),
    CommandsChanges (..),
    IssueStatistics (..),
    IssueStats (..),
    IssueCounts (..),
    IssueBoard (..),
    BoardIssue (..),
    BoardIssueLabel (..),
    Visibility (..),
    TestReport (..),
    TestSuite (..),
    TestCase (..),
    TimeEstimate (..),
    TaskCompletionStatus (..),
    References (..),
    Change (..),
    DiffRefs (..),
    DetailedStatus (..),
    License (..),
    ExpirationPolicy (..),
    RepositoryStorage (..),
    Starrer (..),
    ProjectAvatar (..),
    Email (..),
    Key (..),
    UserPrefs (..),
    UserStatus (..),
    UserCount (..),
    Event (..),
    EventActionName (..),
    EventTargetType (..),
    PushData (..),
    DebugSystemHooks (..),
  )
where

import Control.Monad.IO.Class
import qualified Control.Monad.IO.Class as MIO
import qualified Control.Monad.Reader as MR
import qualified Control.Monad.Trans.Class as MT
import Data.Aeson hiding (Key)
import Data.Aeson.TH
import Data.Aeson.Types hiding (Key)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Network.HTTP.Conduit

-- | The monad in which the GitLab operations can be run.
-- Contains the 'GitLabState' to run the requests with.
--
-- Run it with 'runGitLab'
newtype GitLabT m a = GitLabT (MR.ReaderT GitLabState m a)
  deriving (Functor, Applicative, Monad, MonadFail, MR.MonadReader GitLabState)

instance MT.MonadTrans GitLabT where
  lift = GitLabT . MT.lift

instance (MIO.MonadIO m) => MIO.MonadIO (GitLabT m) where
  liftIO = GitLabT . MIO.liftIO

-- | Utility type which uses 'IO' as underlying monad
type GitLab a = GitLabT IO a

-- | state used by GitLab actions, used internally.
data GitLabState = GitLabState
  { serverCfg :: GitLabServerConfig,
    httpManager :: Manager
  }

-- | configuration data specific to a GitLab server.
data GitLabServerConfig = GitLabServerConfig
  { url :: Text,
    token :: AuthMethod,
    -- | how many times to retry a HTTP request before giving up and returning an error.
    retries :: Int,
    -- | write system hook events to files in the system temporary directory.
    debugSystemHooks :: Maybe DebugSystemHooks
  }

data DebugSystemHooks
  = -- | Report JSON objects about GitLab events only for unprocessed events
    UnprocessedEvents
  | -- | Report JSON objects about GitLab events that were not successfully parsed
    NonParsedJSON
  | -- | Report all received JSON objects about GitLab events
    AllJSON
  deriving (Eq)

-- | default settings, the 'url' and 'token' values will need to be overwritten.
defaultGitLabServer :: GitLabServerConfig
defaultGitLabServer =
  GitLabServerConfig
    { url = "https://gitlab.com",
      token = AuthMethodToken "",
      retries = 5,
      debugSystemHooks = Nothing
    }

-- | personal access token, see <https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html>
data AuthMethod
  = AuthMethodToken Text
  | AuthMethodOAuth Text

-- https://docs.gitlab.com/ee/api/repositories.html#get-file-archive
-- tar.gz, tar.bz2, tbz, tbz2, tb2, bz2, tar, and zip

-- | archive format for file archives of repositories.
-- See 'GitLab.API.Repositories.getFileArchive' in 'GitLab.API.Repositories'.
data ArchiveFormat
  = -- | ".tar.gz"
    TarGz
  | -- | ".tar.bz2"
    TarBz2
  | -- | ".tbz"
    Tbz
  | -- | ".tbz2"
    Tbz2
  | -- | ".tb2"
    Tb2
  | -- | ".bz2"
    Bz2
  | -- | ".tar"
    Tar
  | -- | ".zip"
    Zip

instance Show ArchiveFormat where
  show TarGz = ".tar.gz"
  show TarBz2 = ".tar.bz2"
  show Tbz = ".tbz"
  show Tbz2 = ".tbz2"
  show Tb2 = ".tb2"
  show Bz2 = ".bz2"
  show Tar = ".tar"
  show Zip = ".zip"

-- | the access levels for project members. See <https://docs.gitlab.com/ee/user/permissions.html#project-members-permissions>
data AccessLevel
  = Guest
  | Reporter
  | Developer
  | Maintainer
  | Owner
  deriving (Eq)

instance Show AccessLevel where
  show Guest = "10"
  show Reporter = "20"
  show Developer = "30"
  show Maintainer = "40"
  show Owner = "50"

-- | Where to filter a search within
data SearchIn
  = JustTitle
  | JustDescription
  | TitleAndDescription

instance Show SearchIn where
  show JustTitle = "title"
  show JustDescription = "description"
  show TitleAndDescription = "title,description"

-- | Scope of search results
data Scope
  = CreatedByMe
  | AssignedToMe
  | All

instance Show Scope where
  show CreatedByMe = "created_by_me"
  show AssignedToMe = "assigned_to_me"
  show All = "all"

-- | Sort objects in ascending or descending order
data SortBy
  = Ascending
  | Descending

instance Show SortBy where
  show Ascending = "asc"
  show Descending = "desc"

-- | Ordering search results
data OrderBy
  = CreatedAt
  | UpdatedAt
  | Priority
  | DueDate
  | RelativePosition
  | LabelPriority
  | MilestoneDue
  | Popularity
  | Weight

instance Show OrderBy where
  show CreatedAt = "created_at"
  show UpdatedAt = "updated_at"
  show Priority = "priority"
  show DueDate = "due_date"
  show RelativePosition = "relative_position"
  show LabelPriority = "label_priority"
  show MilestoneDue = "milestone_due"
  show Popularity = "popularity"
  show Weight = "weight"

-- | member of a project.
data Member = Member
  { member_id :: Int,
    member_name :: Maybe Text,
    member_email :: Maybe Text, --- TODO type for email address e.g. zhang@example.com
    member_username :: Maybe Text,
    member_state :: Maybe Text,
    member_avatar_uri :: Maybe Text,
    member_web_url :: Maybe Text,
    member_access_level :: Maybe Int,
    member_group_saml_identity :: Maybe SamlIdentity,
    member_expires_at :: Maybe Text,
    member_invited :: Maybe Bool,
    member_override :: Maybe Bool,
    member_avatar_url :: Maybe Text, -- TODO type for  URL
    member_approved :: Maybe Bool,
    member_membership_type :: Maybe Text, -- TODO type for "group_member"
    member_last_activity_on :: Maybe Text, -- TODO type for "2021-01-27"
    member_created_at :: Maybe UTCTime,
    member_removable :: Maybe Bool,
    member_membership_state :: Maybe Text -- type for "active"
  }
  deriving (Show, Eq)

-- TODO merge Identity and SamlIdentity into a single type.

-- | identity
data Identity = Identity
  { identity_extern_uid :: Text,
    identity_provider :: Text,
    identity_provider_id :: Maybe Int
  }
  deriving (Show, Eq)

-- | SAML identity
data SamlIdentity = SamlIdentity
  { saml_identity_extern_uid :: Text,
    saml_identity_provider :: Text,
    saml_identity_saml_provider_id :: Maybe Int
  }
  deriving (Show, Eq)

-- | namespaces.
data Namespace = Namespace
  { namespace_id :: Int,
    namespace_name :: Text,
    namespace_path :: Text,
    namespace_kind :: Text,
    namespace_full_path :: Maybe Text,
    namespace_avatar_url :: Maybe Text,
    namespace_web_url :: Maybe Text,
    namespace_parent_id :: Maybe Int
  }
  deriving (Show, Eq)

-- | links.
data Links = Links
  { links_self :: Text,
    links_issues :: Maybe Text,
    links_notes :: Maybe Text,
    links_award_emoji :: Maybe Text,
    links_project :: Maybe Text,
    links_merge_requests :: Maybe Text,
    links_repo_branches :: Maybe Text,
    links_labels :: Maybe Text,
    links_events :: Maybe Text,
    links_members :: Maybe Text
  }
  deriving (Show, Eq)

-- | owners.
data Owner = Ownwer
  { owner_id :: Int,
    owner_name :: Text,
    owner_username :: Maybe Text,
    owner_email :: Maybe Text,
    owner_state :: Maybe Text,
    owner_avatar_url :: Maybe Text,
    owner_web_url :: Maybe Text,
    owner_created_at :: Maybe UTCTime
  }
  deriving (Show, Eq)

-- | permissions.
data Permissions = Permissions
  { permissions_project_access :: Maybe Value,
    permissions_group_access :: Maybe Value
  }
  deriving (Show, Eq)

-- | projects.
data Project = Project
  { project_id :: Int,
    project_description :: Maybe Text,
    project_name :: Text,
    project_name_with_namespace :: Text,
    project_path :: Text,
    project_path_with_namespace :: Text,
    project_created_at :: Maybe UTCTime,
    project_default_branch :: Maybe Text,
    project_tag_list :: Maybe [Text], --  GitLab Docs: "deprecated, use `topics` instead"
    project_topics :: Maybe [Text],
    project_ssh_url_to_repo :: Maybe Text,
    project_http_url_to_repo :: Maybe Text,
    project_web_url :: Text,
    project_readme_url :: Maybe Text, -- check
    project_avatar_url :: Maybe Text,
    project_license_url :: Maybe Text,
    project_license :: Maybe License,
    project_star_count :: Maybe Int,
    project_runners_token :: Maybe Text, -- "b8547b1dc37721d05889db52fa2f02"
    project_ci_default_git_depth :: Maybe Int,
    project_ci_forward_deployment_enabled :: Maybe Bool,
    project_forks_count :: Maybe Int,
    project_last_activity_at :: Maybe UTCTime,
    project_namespace :: Maybe Namespace,
    project_archived :: Maybe Bool,
    project_visibility :: Maybe Text,
    project_owner :: Maybe Owner,
    project_resolve_outdated_diff_discussions :: Maybe Bool,
    project_container_registry_enabled :: Maybe Bool,
    project_container_registry_access_level :: Maybe Text, -- TODO
    project_container_expiration_policy :: Maybe ExpirationPolicy,
    -- type for "disabled"
    project_issues_enabled :: Maybe Bool,
    project_merge_requests_enabled :: Maybe Bool,
    project_wiki_enabled :: Maybe Bool,
    project_jobs_enabled :: Maybe Bool,
    project_snippets_enabled :: Maybe Bool,
    project_can_create_merge_request_in :: Maybe Bool,
    project_issues_access_level :: Maybe Text, -- TODO a type for "enabled"
    project_repository_access_level :: Maybe Text, -- TODO a type for "enabled"
    project_merge_requests_access_level :: Maybe Text, -- TODO a type for "enabled"
    project_forking_access_level :: Maybe Text, -- TODO a type for "enabled"
    project_analytics_access_level :: Maybe Text, -- TODO a type for "enabled"
    project_wiki_access_level :: Maybe Text, -- TODO a type for "enabled"
    project_builds_access_level :: Maybe Text, -- TODO a type for "enabled"
    project_snippets_access_level :: Maybe Text, -- TODO a type for "enabled"
    project_pages_access_level :: Maybe Text, -- TODO a type for "enabled"
    project_emails_disabled :: Maybe Bool, -- check
    project_shared_runners_enabled :: Maybe Bool,
    project_lfs_enabled :: Maybe Bool,
    project_creator_id :: Maybe Int,
    project_forked_from_project :: Maybe Project,
    project_import_status :: Maybe String,
    project_open_issues_count :: Maybe Int,
    project_public_jobs :: Maybe Bool,
    project_build_timeout :: Maybe Int,
    project_auto_cancel_pending_pipelines :: Maybe Text, -- TODO a type for "enabled"
    project_ci_config_path :: Maybe Text, -- check null
    project_shared_with_groups :: Maybe [GroupShare],
    project_only_allow_merge_if_pipeline_succeeds :: Maybe Bool,
    project_allow_merge_on_skipped_pipeline :: Maybe Bool,
    project_restrict_user_defined_variables :: Maybe Bool,
    project_request_access_enabled :: Maybe Bool,
    project_only_allow_merge_if_all_discussions_are_resolved :: Maybe Bool,
    project_remove_source_branch_after_merge :: Maybe Bool,
    project_printing_merge_request_link_enabled :: Maybe Bool,
    project_printing_merge_requests_link_enabled :: Maybe Bool,
    project_merge_method :: Maybe Text, -- TODO type for "merge"
    project_squash_option :: Maybe Text, -- TODO type for "default_on"
    project_autoclose_referenced_issues :: Maybe Bool,
    project_suggestion_commit_message :: Maybe Text,
    project_marked_for_deletion_at :: Maybe Text, -- TODO "2020-04-03"
    project_marked_for_deletion_on :: Maybe Text, -- TODO "2020-04-03"
    project_compliance_frameworks :: Maybe [Text],
    project_statistics :: Maybe Statistics,
    project_permissions :: Maybe Permissions,
    project_container_registry_image_prefix :: Maybe Text,
    project__links :: Maybe Links,
    project_mirror :: Maybe Bool,
    project_mirror_overwrites_diverged_branches :: Maybe Bool,
    project_mirror_trigger_builds :: Maybe Bool,
    project_auto_devops_deploy_strategy :: Maybe Text,
    project_auto_devops_enabled :: Maybe Bool,
    project_service_desk_enabled :: Maybe Bool,
    project_approvals_before_merge :: Maybe Int,
    project_mirror_user_id :: Maybe Int,
    project_packages_enabled :: Maybe Bool,
    project_empty_repo :: Maybe Bool,
    project_only_mirror_protected_branches :: Maybe Bool,
    project_repository_storage :: Maybe Text -- TODO type for "default"
  }
  deriving (Show, Eq)

-- | Licenses.
data License = License
  { license_key :: Maybe Text,
    license_name :: Maybe Text,
    license_nickname :: Maybe Text,
    license_html_url :: Maybe Text,
    license_source_url :: Maybe Text
  }
  deriving (Show, Eq)

-- | Expiration policies.
data ExpirationPolicy = ExpirationPolicy
  { expiration_policy_cadence :: Maybe Text,
    expiration_policy_enabled :: Maybe Bool,
    expiration_policy_keep_n :: Maybe Int,
    expiration_policy_older_than :: Maybe Text,
    expiration_policy_name_regex :: Maybe Text,
    expiration_policy_name_regex_delete :: Maybe Value, -- TODO
    expiration_policy_name_regex_keep :: Maybe Value, -- TODO
    expiration_policy_next_run_at :: Maybe UTCTime
  }
  deriving (Show, Eq)

-- | Information about repository storage.
data RepositoryStorage = RepositoryStorage
  { repository_storage_project_id :: Int,
    repository_storage_disk_path :: Maybe Text,
    repository_storage_created_at :: Maybe UTCTime,
    repository_storage_repository_storage :: Maybe Text
  }
  deriving (Show, Eq)

-- | project statistics.
data Statistics = Statistics
  { statistics_commit_count :: Maybe Int,
    statistics_storage_size :: Int,
    statistics_repository_size :: Int,
    statistics_wiki_size :: Maybe Int,
    statistics_lfs_objects_size :: Maybe Int,
    statistics_job_artifacts_size :: Maybe Int,
    statistics_packages_size :: Maybe Int,
    statistics_uploads_size :: Maybe Int,
    statistics_snippets_size :: Maybe Int,
    statistics_pipeline_artifacts_size :: Maybe Int
  }
  deriving (Show, Eq)

-- | registered users.
data User = User
  { user_id :: Int,
    user_username :: Text,
    user_bio :: Maybe Text,
    user_two_factor_enabled :: Maybe Bool,
    user_last_sign_in_at :: Maybe UTCTime,
    user_current_sign_in_at :: Maybe UTCTime,
    user_last_activity_on :: Maybe Text, -- test current-user has '2012-05-23'
    user_skype :: Maybe Text,
    user_twitter :: Maybe Text,
    user_website_url :: Maybe Text,
    user_theme_id :: Maybe Int,
    user_color_scheme_id :: Maybe Int,
    user_external :: Maybe Bool,
    user_private_profile :: Maybe Bool,
    user_projects_limit :: Maybe Int,
    user_can_create_group :: Maybe Bool,
    user_can_create_project :: Maybe Bool,
    user_public_email :: Maybe Text,
    user_organization :: Maybe Text,
    user_job_title :: Maybe Text,
    user_pronouns :: Maybe Text,
    user_linkedin :: Maybe Text,
    user_confirmed_at :: Maybe UTCTime,
    user_identities :: Maybe [Identity],
    user_name :: Text,
    user_email :: Maybe Text,
    user_followers :: Maybe Int,
    user_bot :: Maybe Bool,
    user_following :: Maybe Int,
    user_state :: Maybe Text,
    user_avatar_url :: Maybe Text,
    user_web_url :: Maybe Text,
    user_location :: Maybe Text,
    user_extern_uid :: Maybe Int,
    user_group_id_for_saml :: Maybe Int,
    user_discussion_locked :: Maybe Bool, -- only for author of 'TODO' type
    user_created_at :: Maybe UTCTime,
    user_note :: Maybe Text, -- viewable to administrators only
    user_password :: Maybe Text, -- viewable to administrators only
    user_force_random_password :: Maybe Bool,
    user_providor :: Maybe Text,
    user_reset_password :: Maybe Bool,
    user_skip_confirmation :: Maybe Bool,
    user_view_diffs_file_by_file :: Maybe Bool
  }
  deriving (Show, Eq)

-- | milestone state.
data MilestoneState
  = MSActive
  | MSClosed
  deriving (Show, Eq)

instance FromJSON MilestoneState where
  parseJSON (String "active") = return MSActive
  parseJSON (String "closed") = return MSClosed
  parseJSON x = unexpected x

-- | milestones.
data Milestone = Milestone
  { milestone_project_id :: Maybe Int,
    milestone_group_id :: Maybe Int,
    milestone_description :: Maybe Text,
    milestone_state :: Maybe MilestoneState,
    milestone_due_date :: Maybe Text,
    milestone_start_date :: Maybe Text,
    milestone_iid :: Maybe Int,
    milestone_created_at :: Maybe UTCTime,
    milestone_closed_at :: Maybe UTCTime,
    milestone_title :: Text,
    milestone_id :: Int,
    milestone_updated_at :: Maybe UTCTime,
    milestone_web_url :: Maybe URL
  }
  deriving (Show, Eq)

-- instance FromJSON Milestone where
--   parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 10})

-- | time stats.
data TimeStats = TimeStats
  { time_stats_time_estimate :: Int,
    time_stats_total_time_spent :: Int,
    time_stats_human_time_estimate :: Maybe Int,
    time_stats_human_total_time_spent :: Maybe Int
  }
  deriving (Show, Eq)

-- | alias for project id
type ProjectId = Int

-- | alias for issue id
type IssueId = Int

-- | project issues.
data Issue = Issue
  { issue_state :: Text,
    issue_description :: Maybe Text,
    issue_health_status :: Maybe Text, -- TODO type for "on_track"
    issue_author :: Maybe User,
    issue_milestone :: Maybe Milestone,
    issue_project_id :: Maybe ProjectId,
    issue_assignees :: Maybe [User],
    issue_assignee :: Maybe User,
    issue_updated_at :: Maybe UTCTime,
    issue_closed_at :: Maybe Text,
    issue_closed_by :: Maybe User,
    issue_id :: IssueId,
    issue_title :: Text,
    issue_created_at :: Maybe UTCTime,
    issue_iid :: Int,
    -- TODO: what is the difference between the two below?
    issue_type :: Maybe Text, -- type for this e.g. "ISSUE"
    issue_issue_type :: Maybe Text, -- type for this e.g. "issue"
    issue_labels :: Maybe [Text],
    issue_upvotes :: Int,
    issue_downvotes :: Int,
    issue_merge_requests_count :: Maybe Int,
    issue_user_notes_count :: Maybe Int,
    issue_due_date :: Maybe Text,
    issue_web_url :: Text,
    issue_references :: Maybe References,
    issue_confidential :: Maybe Bool,
    issue_weight :: Maybe Text, -- Int?
    issue_epic :: Maybe Epic, -- Int?
    issue_discussion_locked :: Maybe Bool,
    issue_time_stats :: Maybe TimeStats,
    issue_has_tasks :: Maybe Bool,
    issue_task_status :: Maybe Text,
    issue__links :: Maybe Links,
    issue_task_completion_status :: Maybe TaskCompletionStatus,
    issue_blocking_issues_count :: Maybe Int,
    issue_subscribed :: Maybe Bool,
    issue_service_desk_reply_to :: Maybe Text
  }
  deriving (Show, Eq)

-- | GitLab epic.
data Epic = Epic
  { epic_id :: Int,
    epic_iid :: Int,
    epic_title :: Text,
    epic_url :: Text,
    epic_group_id :: Int
  }
  deriving (Show, Eq)

-- | project pipelines
data Pipeline = Pipeline
  { pipeline_id :: Int,
    pipeline_iid :: Maybe Int,
    pipeline_project_id :: Maybe Int,
    pipeline_sha :: Text,
    pipeline_ref :: Text,
    pipeline_status :: Text,
    pipeline_web_url :: Maybe Text,
    pipeline_before_sha :: Maybe Text,
    pipeline_tag :: Maybe Bool,
    pipeline_yaml_errors :: Maybe Text,
    pipeline_user :: Maybe User,
    pipeline_created_at :: Maybe UTCTime,
    pipeline_updated_at :: Maybe UTCTime,
    pipeline_started_at :: Maybe UTCTime,
    pipeline_finished_at :: Maybe UTCTime,
    pipelined_committed_at :: Maybe UTCTime,
    pipeline_duration :: Maybe Int,
    pipeline_queued_duration :: Maybe Double,
    pipeline_coverage :: Maybe Text,
    pipeline_detailed_status :: Maybe DetailedStatus
  }
  deriving (Show, Eq)

-- | project pipelines
data DetailedStatus = DetailedStatus
  { detailed_status_icon :: Maybe Text, -- "status_pending"
    detailed_status_text :: Maybe Text,
    detailed_status_label :: Maybe Text,
    detailed_status_group :: Maybe Text,
    detailed_status_tooltip :: Maybe Text,
    detailed_status_has_details :: Maybe Bool,
    detailed_status_details_path :: Maybe Text,
    detailed_status_illustration :: Maybe Text,
    detailed_status_favicon :: Maybe Text
  }
  deriving (Show, Eq)

-- | code commits.
data Commit = Commit
  { commit_id :: Text,
    commit_short_id :: Text,
    commit_title :: Text,
    commit_author_name :: Text,
    commit_author_email :: Text,
    commit_authored_date :: Maybe Text, -- ZonedTime ?
    commit_committer_name :: Maybe Text,
    commit_committer_email :: Maybe Text,
    commit_committed_date :: Maybe Text, -- ZonedTime ?
    commit_created_at :: Maybe Text, -- ZonedTime ?
    commit_message :: Text,
    commit_parent_ids :: Maybe [String],
    commit_last_pipeline :: Maybe Pipeline,
    commit_stats :: Maybe CommitStats,
    commit_status :: Maybe Text,
    commit_web_url :: Maybe Text
  }
  deriving (Show, Eq)

-- | summary of a code commit for TODOs.
data CommitTodo = CommitTodo
  { commit_todo_id :: Text,
    commit_todo_short_id :: Text,
    commit_todo_created_at :: UTCTime,
    commit_todo_parent_ids :: Maybe [String]
  }
  deriving (Show, Eq)

-- | repository contributors.
data Contributor = Contributor
  { contributor_name :: Text,
    contributor_email :: Text,
    contributor_commits :: Int,
    contributor_additions :: Int,
    contributor_deletions :: Int
  }
  deriving (Show, Eq)

-- | commit stats.
data CommitStats = CommitStats
  { commitstats_additions :: Int,
    commitstats_deletions :: Int,
    commitstats_total :: Int
  }
  deriving (Show, Eq)

-- | tags.
data Tag = Tag
  { tag_commit :: Commit,
    tag_release :: Maybe Release,
    tag_name :: Text,
    tag_target :: Text,
    tag_message :: Maybe Text,
    tag_protected :: Bool
  }
  deriving (Show, Eq)

-- | Release associated with a tag
data Release = Release
  { release_tag_name :: Text,
    release_description :: Text
  }
  deriving (Show, Eq)

-- | diff between two commits.
data Diff = Diff
  { diff_diff :: Text,
    diff_new_path :: Text,
    diff_old_path :: Text,
    diff_a_mode :: Maybe Text,
    diff_b_mode :: Maybe Text,
    diff_new_file :: Bool,
    diff_renamed_file :: Bool,
    diff_deleted_file :: Bool
  }
  deriving (Show, Eq)

-- | repositories.
data Repository = Repository
  { repository_id :: Maybe Text,
    repository_name :: Text,
    repository_type :: Maybe Text,
    repository_path :: Maybe Text,
    repository_mode :: Maybe Text
  }
  deriving (Show, Eq)

-- | artifacts.
data Artifact = Artifact
  { artifact_file_type :: Maybe Text,
    artifact_size :: Int,
    artifact_filename :: Text,
    artifact_file_format :: Maybe Text
  }
  deriving (Show, Eq)

-- | groups.
data Group = Group
  { group_id :: Int,
    group_name :: Text,
    group_path :: Maybe Text,
    group_description :: Maybe Text,
    group_visibility :: Maybe Text,
    group_share_with_group_lock :: Maybe Bool,
    group_require_two_factor_authentication :: Maybe Bool,
    group_two_factor_grace_period :: Maybe Int,
    group_project_creation_level :: Maybe Text, -- TODO type for "developer"
    group_auto_devops_enabled :: Maybe Bool,
    group_subgroup_creation_level :: Maybe Text, -- TODO type for "owner"
    group_emails_disabled :: Maybe Bool,
    group_mentions_disabled :: Maybe Bool,
    group_default_branch_protection :: Maybe Int,
    group_lfs_enabled :: Maybe Bool,
    group_avatar_url :: Maybe Text,
    group_web_url :: Maybe Text,
    group_request_access_enabled :: Maybe Bool,
    group_full_name :: Maybe Text,
    group_full_path :: Maybe Text,
    group_runners_token :: Maybe Text,
    group_file_template_project_id :: Maybe Int,
    group_parent_id :: Maybe Int,
    group_created_at :: Maybe UTCTime,
    group_statistics :: Maybe Statistics,
    group_shared_with_groups :: Maybe [GroupShare],
    group_prevent_sharing_groups_outside_hierarchy :: Maybe Bool
  }
  deriving (Show, Eq)

-- | response to sharing a project with a group.
data GroupShare = GroupShare
  { groupshare_id :: Maybe Int,
    groupshare_project_id :: Maybe Int,
    groupshare_group_id :: Int,
    groupshare_group_name :: Maybe Text,
    groupshare_group_full_path :: Maybe Text,
    groupshare_group_access_level :: Maybe Int, -- TODO change this to 'AccessLevel'
    groupshare_expires_at :: Maybe Text
  }
  deriving (Show, Eq)

-- | code branches.
data Branch = Branch
  { branch_name :: Text,
    branch_merged :: Bool,
    branch_protected :: Bool,
    branch_default :: Bool,
    branch_developers_can_push :: Bool,
    branch_developers_can_merge :: Bool,
    branch_can_push :: Bool,
    branch_web_url :: Maybe Text,
    branch_commit :: Commit
  }
  deriving (Show, Eq)

-- | files in a repository.
data RepositoryFile = RepositoryFile
  { repository_file_file_name :: Text,
    repository_file_file_path :: Text,
    repository_file_size :: Int,
    repository_file_encoding :: Text,
    repository_file_content :: Text,
    repository_file_content_sha256 :: Text,
    repository_file_ref :: Text,
    repository_file_blob_id :: Text,
    repository_file_commit_id :: Text,
    repository_file_last_commit_id :: Text,
    repository_file_execute_filemode :: Maybe Bool
  }
  deriving (Show, Eq)

-- | files in a repository.
data RepositoryFileSimple = RepositoryFileSimple
  { repository_file_simple_file_path :: Text,
    repository_file_simple_branch :: Text
  }
  deriving (Show, Eq)

-- | files in a repository.
data RepositoryFileBlame = RepositoryFileBlame
  { repository_file_blame_commit :: Commit,
    repository_file_blame_lines :: [Text]
  }
  deriving (Show, Eq)

-- | project merge requests.
data MergeRequest = MergeRequest
  { merge_request_id :: Int,
    merge_request_iid :: Int,
    merge_request_project_id :: Int,
    merge_request_title :: Text,
    merge_request_description :: Text,
    merge_request_state :: Text, -- TODO make a type e.g. 'reopened'
    merge_request_created_at :: UTCTime,
    merge_request_updated_at :: UTCTime,
    merge_request_target_branch :: Text,
    merge_request_source_branch :: Text,
    merge_request_upvotes :: Int,
    merge_request_downvotes :: Int,
    merge_request_author :: User,
    merge_request_assignee :: Maybe User,
    merge_request_assignees :: Maybe [User],
    merge_request_reviewers :: Maybe [User],
    merge_request_source_project_id :: Int,
    merge_request_target_project_id :: Int,
    merge_request_labels :: [Text],
    merge_request_draft :: Maybe Bool,
    merge_request_work_in_progress :: Bool,
    merge_request_milestone :: Maybe Milestone,
    merge_request_merge_when_pipeline_succeeds :: Bool,
    merge_request_merge_status :: Text, -- create type e.g. for "can_be_merged"
    merge_request_merge_error :: Maybe Text,
    merge_request_sha :: Maybe Text,
    merge_request_merge_commit_sha :: Maybe Text,
    merge_request_squash_commit_sha :: Maybe Text,
    merge_request_user_notes_count :: Int,
    merge_request_discussion_locked :: Maybe Bool,
    merge_request_should_remove_source_branch :: Maybe Bool,
    merge_request_force_remove_source_branch :: Maybe Bool,
    merge_request_allow_collaboration :: Maybe Bool,
    merge_request_allow_maintainer_to_push :: Maybe Bool,
    merge_request_web_url :: Maybe Text,
    merge_request_time_stats :: Maybe TimeStats,
    merge_request_squash :: Maybe Bool,
    merge_request_subscribed :: Maybe Bool,
    merge_request_changes_count :: Maybe String,
    merge_request_merged_by :: Maybe User,
    merge_request_merged_at :: Maybe UTCTime,
    merge_request_closed_by :: Maybe User,
    merge_request_closed_at :: Maybe UTCTime,
    merge_request_latest_build_started_at :: Maybe UTCTime,
    merge_request_latest_build_finished_at :: Maybe UTCTime,
    merge_request_first_deployed_to_production_at :: Maybe UTCTime,
    merge_request_pipeline :: Maybe Pipeline,
    merge_request_head_pipeline :: Maybe Pipeline,
    merge_request_diverged_commits_count :: Maybe Int,
    merge_request_rebase_in_progress :: Maybe Bool,
    merge_request_first_contribution :: Maybe Bool,
    merge_request_has_conflicts :: Maybe Bool,
    merge_request_blocking_discussions_resolved :: Maybe Bool,
    merge_request_approvals_before_merge :: Maybe Int,
    merge_request_mirror :: Maybe Bool,
    merge_request_task_completion_status :: Maybe TaskCompletionStatus,
    merge_request_reference :: Maybe Text,
    merge_request_references :: Maybe References,
    merge_request_changes :: Maybe [Change],
    merge_request_overflow :: Maybe Bool,
    merge_request_diff_refs :: Maybe DiffRefs
  }
  deriving (Show, Eq)

-- | monitors a task completion status.
data TaskCompletionStatus = TaskCompletionStatus
  { task_completion_status_count :: Int,
    task_completion_status_completed_count :: Maybe Int
  }
  deriving (Show, Eq)

-- | references.
data References = References
  { references_short :: Text,
    references_relative :: Text,
    references_full :: Text
  }
  deriving (Show, Eq)

-- | Change between commits.
data Change = Change
  { change_old_path :: Text,
    change_new_path :: Text,
    change_a_mode :: Text, -- find type for "100644"
    change_b_mode :: Text, -- find type for "100644"
    change_diff :: Text, -- find type for "--- a/VERSION\\ +++ b/VERSION\\ @@ -1 +1 @@\\ -1.9.7\\ +1.9.8"
    change_new_file :: Bool,
    change_renamed_file :: Bool,
    change_deleted_file :: Bool
  }
  deriving (Show, Eq)

-- | diff references.
data DiffRefs = DiffRefs
  { diff_refs_base_sha :: Text,
    diff_refs_head_sha :: Text,
    diff_refs_start_sha :: Text
  }
  deriving (Show, Eq)

{- TODO for MergeRequest

  "references": {
    "short": "!1",
    "relative": "!1",
    "full": "my-group/my-project!1"
  },

  "changes": [
    {
    "old_path": "VERSION",
    "new_path": "VERSION",
    "a_mode": "100644",
    "b_mode": "100644",
    "diff": "--- a/VERSION\\ +++ b/VERSION\\ @@ -1 +1 @@\\ -1.9.7\\ +1.9.8",
    "new_file": false,
    "renamed_file": false,
    "deleted_file": false
    }

  "overflow": false

  "diff_refs": {
    "base_sha": "c380d3acebd181f13629a25d2e2acca46ffe1e00",
    "head_sha": "2be7ddb704c7b6b83732fdd5b9f09d5a397b5f8f",
    "start_sha": "c380d3acebd181f13629a25d2e2acca46ffe1e00"
  },

-}

-- | TODO action.
data TodoAction
  = TAAssigned
  | TAMentioned
  | TABuildFailed
  | TAMarked
  | TAApprovalRequired
  | TAUnmergeable
  | TADirectlyAddressed
  deriving (Show, Eq)

instance FromJSON TodoAction where
  parseJSON (String "assigned") = return TAAssigned
  parseJSON (String "mentioned") = return TAMentioned
  parseJSON (String "build_failed") = return TABuildFailed
  parseJSON (String "marked") = return TAMarked
  parseJSON (String "approval_required") = return TAApprovalRequired
  parseJSON (String "unmergeable") = return TAUnmergeable
  parseJSON (String "directly_addressed") = return TADirectlyAddressed
  parseJSON x = unexpected x

-- | TODO targets.
data TodoTarget
  = TTIssue Issue
  | TTMergeRequest MergeRequest
  | TTCommit CommitTodo
  deriving (Show, Eq)

-- | URL is a synonym for 'Text'.
type URL = Text

-- | TODO states.
data TodoState
  = TSPending
  | TSDone
  deriving (Show, Eq)

-- | A project TODO.
data TodoProject = TodoProject
  { todo_project_id :: Int,
    todo_project_description :: Maybe Text,
    todo_project_name :: Text,
    todo_project_name_with_namespace :: Text,
    todo_project_path :: Text,
    todo_project_path_with_namespace :: Text,
    todo_project_created_at :: Maybe UTCTime
  }
  deriving (Show, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "todo_project_"), omitNothingFields = True} ''TodoProject)

-- | TODOs.
data Todo = Todo
  { todo_id :: Int,
    todo_project :: TodoProject,
    todo_author :: User,
    todo_action_name :: TodoAction,
    todo_target_type :: TodoTargetType,
    todo_target :: TodoTarget,
    todo_target_url :: URL,
    todo_body :: Text,
    todo_state :: TodoState,
    todo_created_at :: UTCTime,
    todo_updated_at :: Maybe UTCTime
  }
  deriving (Show, Eq)

-- | Target type of a TODO.
data TodoTargetType
  = MergeRequestTarget
  | IssueTarget
  | CommitTarget
  deriving (Show, Eq)

-- | Type of a TODO.
data TodoType
  = TodoTypeIssue
  | TodoTypeMergeRequest
  | TodoTypeCommit
  | TodoTypeEpic
  | TodoTypeDesign
  | TodoTypeAlert

instance Show TodoType where
  show TodoTypeIssue = "Issue"
  show TodoTypeMergeRequest = "MergeRequest"
  show TodoTypeCommit = "Commit"
  show TodoTypeEpic = "Epic"
  show TodoTypeDesign = "DesignManagement::Design"
  show TodoTypeAlert = "AlertManagement::Alert"

-- | version of the GitLab instance.
data Version = Version
  { version_version :: Text,
    version_revision :: Text
  }
  deriving (Show, Eq)

-- | An edit issue request.
data EditIssueReq = EditIssueReq
  { edit_issue_id :: ProjectId,
    edit_issue_issue_iid :: IssueId,
    edit_issue_title :: Maybe Text,
    edit_issue_description :: Maybe Text,
    edit_issue_confidential :: Maybe Bool,
    edit_issue_assignee_ids :: Maybe [Int],
    edit_issue_milestone_id :: Maybe Int,
    edit_issue_labels :: Maybe [Text],
    edit_issue_state_event :: Maybe Text,
    edit_issue_updated_at :: Maybe UTCTime,
    edit_issue_due_date :: Maybe Text,
    edit_issue_weight :: Maybe Int,
    edit_issue_discussion_locked :: Maybe Bool,
    edit_issue_epic_id :: Maybe Int,
    edit_issue_epic_iid :: Maybe Int
  }
  deriving (Show)

-- | Discussions https://docs.gitlab.com/ee/api/discussions.html
data Discussion = Discussion
  { discussion_id :: Text,
    discussion_individual_note :: Bool,
    discussion_notes :: [Note]
  }
  deriving (Show, Eq)

-- | Note attached to a commit.
data CommitNote = CommitNote
  { commitnote_note :: Text,
    commitnote_author :: User
  }
  deriving (Show, Eq)

-- | Notes
data Note = Note
  { note_id :: Int,
    note_title :: Maybe Text, -- for snippets
    note_file_name :: Maybe Text, -- for snippets
    -- https://docs.gitlab.com/ee/api/discussions.html#list-project-commit-discussion-items
    note_type :: Maybe Text, -- TODO create type for this, e.g. from "DiscussionNote"
    note_body :: Maybe Text,
    note_attachment :: Maybe Text,
    note_author :: Owner,
    note_created_at :: UTCTime,
    note_updated_at :: Maybe UTCTime,
    note_system :: Maybe Bool,
    note_noteable_id :: Maybe Int,
    note_noteable_type :: Maybe Text, -- create type e.g. from "Commit"
    note_noteable_iid :: Maybe Int,
    note_commands_changes :: Maybe CommandsChanges,
    note_resolved :: Maybe Bool,
    note_resolvable :: Maybe Bool,
    note_confidential :: Maybe Bool,
    note_resolved_by :: Maybe User -- TODO check
  }
  deriving (Show, Eq)

-- | has a change been promoted to an epic.
newtype CommandsChanges = CommanandsChanges
  { commands_changes_promote_to_epic :: Bool
  }
  deriving (Show, Eq)

-- | Statistics and an issue
newtype IssueStatistics = IssueStatistics
  { issue_statistics_stats :: IssueStats
  }
  deriving (Show, Eq)

-- | Issue statistics
newtype IssueStats = IssueStats
  { issue_stats_issue_counts :: IssueCounts
  }
  deriving (Show, Eq)

-- | A count of all, open and closed issues against a project
data IssueCounts = IssueCounts
  { issue_counts__all :: Int,
    issue_counts_closed :: Int,
    issue_counts_opened :: Int
  }
  deriving (Show, Eq)

-- | Project issue boards https://docs.gitlab.com/ee/user/project/issue_board.html
data IssueBoard = IssueBoard
  { board_id :: Int,
    board_name :: Text,
    board_project :: Project,
    board_milestone :: Maybe Milestone,
    board_lists :: [BoardIssue],
    board_group :: Maybe Text, -- not sure, documentation doesn't indicate type
    board_assignee :: Maybe Owner,
    board_labels :: Maybe [BoardIssueLabel],
    board_weight :: Maybe Int
  }
  deriving (Show, Eq)

-- | Issues associated with a project issue board
data BoardIssue = BoardIssue
  { board_issue_id :: Int,
    board_issue_label :: BoardIssueLabel,
    board_issue_position :: Int,
    board_issue_max_issue_count :: Int,
    board_issue_max_issue_weight :: Int,
    -- TODO, the docs don't say what type this should be
    board_issue_limit_metric :: Maybe Int
  }
  deriving (Show, Eq)

-- | Label of an issues for a project issue board
data BoardIssueLabel = BoardIssueLabel
  { board_issue_label_id :: Maybe Int,
    board_issue_label_name :: Text,
    board_issue_label_color :: Text, -- parse into type from e.g. "#F0AD4E"
    board_issue_label_description :: Maybe Text
  }
  deriving (Show, Eq)

-- |  Project visibility.
data Visibility
  = Public
  | Private
  | Internal
  deriving (Show, Eq)

instance FromJSON Visibility where
  parseJSON (String "public") = return Public
  parseJSON (String "private") = return Private
  parseJSON (String "internal") = return Internal
  parseJSON (Number 0) = return Private
  parseJSON (Number 10) = return Internal
  parseJSON (Number 20) = return Public
  parseJSON n = error (show n)

-- | Unit test reports for a CI pipeline https://docs.gitlab.com/ee/ci/unit_test_reports.html
data TestReport = TestReport
  { test_report_total_time :: Double,
    test_report_total_count :: Int,
    test_report_success_count :: Int,
    test_report_failed_count :: Int,
    test_report_skipped_count :: Int,
    test_report_error_count :: Int,
    test_report_test_suites :: [TestSuite]
  }
  deriving (Show, Eq)

-- | Testsuites associated with a test report
data TestSuite = TestSuite
  { testsuite_name :: Text,
    testsuite_total_time :: Double,
    testsuite_success_count :: Int,
    testsuite_failed_count :: Int,
    testsuite_skipped_count :: Int,
    testsuite_error_count :: Int,
    testsuite_test_cases :: [TestCase]
  }
  deriving (Show, Eq)

-- | Test case associated with a testsuite
data TestCase = TestCase
  { testcase_status :: Text, -- could turn this into a type e.g. for "success"
    testcase_name :: Text,
    testcase_classname :: Text,
    testcase_execution_time :: Double,
    testcase_system_output :: Maybe Text,
    testcase_stack_trace :: Maybe Text
  }
  deriving (Show, Eq)

-- | Estimated humand and total time spent.
data TimeEstimate = TimeEstimate
  { time_estimate_human_time_estimate :: Maybe Text,
    time_estimate_human_total_time_spent :: Maybe Text,
    time_estimate_time_estimate :: Maybe Int,
    time_estimate_total_time_spent :: Maybe Int
  }
  deriving (Show, Eq)

-- | Email information.
data Email = Email
  { email_id :: Int,
    email_email :: Text,
    email_confirmed_at :: Maybe UTCTime
  }
  deriving (Show, Eq)

-- | User preferences.
data UserPrefs = UserPrefs
  { user_prefs_id :: Int,
    user_prefs_user_id :: Int,
    user_prefs_view_diffs_file_by_file :: Bool,
    user_prefs_show_whitespace_in_diffs :: Bool
  }
  deriving (Show, Eq)

-- | SSH key information.
data Key = Key
  { key_id :: Maybe Int,
    key_title :: Maybe Text,
    key_key :: Text,
    key_created_at :: Maybe UTCTime,
    key_expires_at :: Maybe UTCTime
  }
  deriving (Show, Eq)

-- | User status.
data UserStatus = UserStatus
  { user_status_emoji :: Maybe Text, -- TODO type for "coffee"
    user_status_availability :: Maybe Text, -- TODO type for "busy"
    user_status_message :: Maybe Text,
    user_status_message_html :: Maybe Text, -- TODO type for HTML content
    user_status_clear_status_at :: Maybe UTCTime
  }
  deriving (Show, Eq)

-- | Tracks counts for a user's activity.
data UserCount = UserCount
  { user_count_merge_requests :: Int, -- TODO type for "coffee"
    user_count_assigned_issues :: Int, -- TODO type for "busy"
    user_count_assigned_merge_requests :: Int,
    user_count_review_requested_merge_requests :: Int, -- TODO type for HTML content
    user_count_todos :: Int
  }
  deriving (Show, Eq)

-- TODO this data type could be improved to remove redundant Maybe
-- values. E.g. the push_data field will only be populated for the
-- "pushed" action_name, but would be Nothing for all action_name
-- values. Same for 'commented on' and the existence of a 'event_note'
-- field value.

-- | Events https://docs.gitlab.com/ee/api/events.html
data Event = Event
  { event_id :: Int,
    event_title :: Maybe Text,
    event_project_id :: Int,
    event_action_name :: EventActionName,
    event_target_id :: Maybe Int,
    event_target_iid :: Maybe Int,
    event_target_type :: Maybe EventTargetType,
    event_author_id :: Int,
    event_target_title :: Maybe Text,
    event_created_at :: Maybe UTCTime,
    event_author :: User,
    event_author_username :: Text,
    event_push_data :: Maybe PushData,
    event_note :: Maybe Note
  }
  deriving (Show, Eq)

-- | Information about a git push.
data PushData = PushData
  { push_data_commit_count :: Int,
    push_data_action :: EventActionName,
    push_data_ref_type :: Text, -- TODO type for "branch"
    push_data_commit_from :: Text, -- sha hash
    push_data_commit_to :: Text, -- sha hash
    push_data_ref :: Text,
    push_data_commit_title :: Text
  }
  deriving (Show, Eq)

-- | Tracks whether an action is open, closed, pushed or commented on.
data EventActionName
  = ANOpened
  | ANClosed
  | ANPushed
  | ANCommentedOn
  deriving (Show, Eq)

instance ToJSON EventActionName where
  toJSON ANOpened = String "opened"
  toJSON ANClosed = String "closed"
  toJSON ANPushed = String "pushed"
  toJSON ANCommentedOn = String "commented on"

instance FromJSON EventActionName where
  parseJSON (String "opened") = return ANOpened
  parseJSON (String "closed") = return ANClosed
  parseJSON (String "pushed") = return ANPushed
  parseJSON (String "commented on") = return ANCommentedOn
  parseJSON x = unexpected x

-- | Associates an event with a particular target.
data EventTargetType
  = ETTIssue
  | ETTMilestone
  | ETTMergeRequest
  | ETTNote
  | ETTProject
  | ETTSnippet
  | ETTUser
  deriving (Show, Eq)

instance ToJSON EventTargetType where
  toJSON ETTIssue = String "Issue"
  toJSON ETTMilestone = String "Milestone"
  toJSON ETTMergeRequest = String "MergeRequest"
  toJSON ETTNote = String "Note"
  toJSON ETTProject = String "Project"
  toJSON ETTSnippet = String "Snippet"
  toJSON ETTUser = String "User"

instance FromJSON EventTargetType where
  parseJSON (String "Issue") = return ETTIssue
  parseJSON (String "Milestone") = return ETTMilestone
  parseJSON (String "MergeRequest") = return ETTMergeRequest
  parseJSON (String "Note") = return ETTNote
  parseJSON (String "Project") = return ETTProject
  parseJSON (String "Snippet") = return ETTSnippet
  parseJSON (String "User") = return ETTUser
  parseJSON x = unexpected x

-- | Events https://docs.gitlab.com/ee/api/events.html
data Job = Job
  { job_commit :: Commit,
    job_coverage :: Maybe Text, -- ??
    job_allow_failure :: Bool,
    job_created_at :: UTCTime,
    job_started_at :: Maybe UTCTime,
    job_finished_at :: Maybe UTCTime,
    job_duration :: Maybe Double,
    job_queued_duration :: Double,
    job_artifacts_file :: Maybe Artifact,
    job_artifacts :: Maybe [Artifact],
    job_artifacts_expire_at :: Maybe UTCTime,
    job_tag_list :: Maybe [Text],
    job_id :: Int,
    job_name :: Text,
    job_pipeline :: Maybe Pipeline,
    job_ref :: Text,
    job_stage :: Maybe Text,
    job_status :: Text, -- TODO type for "failed" and others
    job_failure_reason :: Maybe Text, -- TODO type for "script_failure" and others
    job_tag :: Bool,
    job_web_url :: Text, -- TODO type for URL like "https://example.com/foo/bar/-/jobs/7"
    job_user :: Maybe User,
    job_downstream_pipeline :: Maybe Pipeline
  }
  deriving (Show, Eq)

-----------------------------
-- JSON GitLab parsers below
-----------------------------

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "time_stats_"), omitNothingFields = True} ''TimeStats)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "saml_identity_"), omitNothingFields = True} ''SamlIdentity)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "identity_"), omitNothingFields = True} ''Identity)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "user_"), omitNothingFields = True} ''User)

instance ToJSON MilestoneState where
  toJSON MSActive = String "active"
  toJSON MSClosed = String "closed"

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "milestone_"), omitNothingFields = True} ''Milestone)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "references_"), omitNothingFields = True} ''References)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "epic_"), omitNothingFields = True} ''Epic)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "links_"), omitNothingFields = True} ''Links)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "task_completion_status_"), omitNothingFields = True} ''TaskCompletionStatus)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "issue_"), omitNothingFields = True} ''Issue)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "detailed_status_"), omitNothingFields = True} ''DetailedStatus)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "pipeline_"), omitNothingFields = True} ''Pipeline)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "commitstats_"), omitNothingFields = True} ''CommitStats)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "commit_"), omitNothingFields = True} ''Commit)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "release_"), omitNothingFields = True} ''Release)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "tag_"), omitNothingFields = True} ''Tag)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "contributor_"), omitNothingFields = True} ''Contributor)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "member_"), omitNothingFields = True} ''Member)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "permissions_"), omitNothingFields = True} ''Permissions)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "owner_"), omitNothingFields = True} ''Owner)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "namespace_"), omitNothingFields = True} ''Namespace)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "expiration_policy_"), omitNothingFields = True} ''ExpirationPolicy)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "license_"), omitNothingFields = True} ''License)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "groupshare_"), omitNothingFields = True} ''GroupShare)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "statistics_"), omitNothingFields = True} ''Statistics)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "project_"), omitNothingFields = True} ''Project)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "repository_"), omitNothingFields = True} ''Repository)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "artifact_"), omitNothingFields = True} ''Artifact)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "job_"), omitNothingFields = True} ''Job)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "branch_"), omitNothingFields = True} ''Branch)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "repository_file_"), omitNothingFields = True} ''RepositoryFile)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "repository_file_simple_"), omitNothingFields = True} ''RepositoryFileSimple)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "repository_file_blame_"), omitNothingFields = True} ''RepositoryFileBlame)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "change_"), omitNothingFields = True} ''Change)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "diff_refs_"), omitNothingFields = True} ''DiffRefs)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "merge_request_"), omitNothingFields = True} ''MergeRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "diff_"), omitNothingFields = True} ''Diff)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "version_"), omitNothingFields = True} ''Version)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "edit_issue_"), omitNothingFields = True} ''EditIssueReq)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "commands_changes_"), omitNothingFields = True} ''CommandsChanges)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "note_"), omitNothingFields = True} ''Note)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "discussion_"), omitNothingFields = True} ''Discussion)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "issue_counts_"), omitNothingFields = True} ''IssueCounts)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "issue_stats_"), omitNothingFields = True} ''IssueStats)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "issue_statistics_"), omitNothingFields = True} ''IssueStatistics)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "board_issue_label_"), omitNothingFields = True} ''BoardIssueLabel)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "board_issue_"), omitNothingFields = True} ''BoardIssue)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "board_"), omitNothingFields = True} ''IssueBoard)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "testcase_"), omitNothingFields = True} ''TestCase)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "testsuite_"), omitNothingFields = True} ''TestSuite)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "test_report_"), omitNothingFields = True} ''TestReport)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "time_estimate_"), omitNothingFields = True} ''TimeEstimate)

instance ToJSON TodoAction where
  toJSON TAAssigned = String "assigned"
  toJSON TAMentioned = String "mentioned"
  toJSON TABuildFailed = String "build_build"
  toJSON TAMarked = String "marked"
  toJSON TAApprovalRequired = String "approval_required"
  toJSON TAUnmergeable = String "unmergeable"
  toJSON TADirectlyAddressed = String "directly_addressed"

instance FromJSON TodoState where
  parseJSON (String "pending") = return TSPending
  parseJSON (String "done") = return TSDone
  parseJSON x = unexpected x

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "commit_todo_"), omitNothingFields = True} ''CommitTodo)

instance FromJSON TodoTargetType where
  parseJSON (String "MergeRequest") = return MergeRequestTarget
  parseJSON (String "Issue") = return IssueTarget
  parseJSON (String "Commit") = return CommitTarget
  parseJSON x = unexpected x

instance FromJSON Todo where
  parseJSON = withObject "Todo" $ \v ->
    Todo
      <$> v .: "id"
      <*> v .: "project"
      <*> v .: "author"
      <*> v .: "action_name"
      <*> v .: "target_type"
      <*> ( v .: "target_type" >>= \case
              "MergeRequest" -> TTMergeRequest <$> v .: "target"
              "Issue" -> TTIssue <$> v .: "target"
              "Commit" -> TTCommit <$> v .: "target"
              (_ :: Text) -> fail ""
          )
      <*> v .: "target_url"
      <*> v .: "body"
      <*> v .: "state"
      <*> v .: "created_at"
      <*> v .:? "updated_at"

$(deriveToJSON defaultOptions {fieldLabelModifier = drop (T.length "todo_"), omitNothingFields = True} ''Todo)

instance ToJSON TodoTargetType where
  toJSON MergeRequestTarget = String "MergeRequest"
  toJSON IssueTarget = String "Issue"
  toJSON CommitTarget = String "Commit"

instance ToJSON TodoState where
  toJSON TSPending = String "pending"
  toJSON TSDone = String "done"

instance ToJSON TodoTarget where
  toJSON (TTIssue x) = toJSON x
  toJSON (TTMergeRequest x) = toJSON x
  toJSON (TTCommit x) = toJSON x

-- | User who is the starrer of a project.
data Starrer = Starrer
  { starrer_starred_since :: UTCTime,
    starrer_user :: User
  }
  deriving (Show, Eq)

-- | Avatar for a project.
newtype ProjectAvatar = ProjectAvatar
  { project_avatar_avatar_url :: Text
  }
  deriving (Show, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "repository_storage_"), omitNothingFields = True} ''RepositoryStorage)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "starrer_"), omitNothingFields = True} ''Starrer)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "project_avatar_"), omitNothingFields = True} ''ProjectAvatar)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "group_"), omitNothingFields = True} ''Group)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "commitnote_"), omitNothingFields = True} ''CommitNote)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "email_"), omitNothingFields = True} ''Email)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "key_"), omitNothingFields = True} ''Key)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "user_prefs_"), omitNothingFields = True} ''UserPrefs)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "user_status_"), omitNothingFields = True} ''UserStatus)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "user_count_"), omitNothingFields = True} ''UserCount)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "push_data_"), omitNothingFields = True} ''PushData)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "event_"), omitNothingFields = True} ''Event)
