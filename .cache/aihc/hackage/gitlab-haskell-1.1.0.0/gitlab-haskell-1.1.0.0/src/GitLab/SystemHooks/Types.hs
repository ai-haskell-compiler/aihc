{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : GitLab.SystemHooks.Types
-- Description : Haskell records corresponding to JSON data from GitLab system hook events
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2020
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.SystemHooks.Types
  ( Rule (..),
    SystemHook (..),
    ProjectCreate (..),
    ProjectDestroy (..),
    ProjectRename (..),
    ProjectTransfer (..),
    ProjectUpdate (..),
    ProjectChanges (..),
    UserAddToTeam (..),
    UserUpdateForTeam (..),
    UserRemoveFromTeam (..),
    UserCreate (..),
    UserRemove (..),
    UserFailedLogin (..),
    UserRename (..),
    KeyCreate (..),
    KeyRemove (..),
    GroupCreate (..),
    GroupRemove (..),
    GroupRename (..),
    NewGroupMember (..),
    GroupMemberRemove (..),
    GroupMemberUpdate (..),
    Push (..),
    TagPush (..),
    ProjectEvent (..),
    RepositoryEvent (..),
    RepositoryUpdate (..),
    CommitEvent (..),
    CommitAuthorEvent (..),
    Visibility (..),
    MergeRequestEvent (..),
    Label (..),
    MergeRequestChanges (..),
    MergeRequestChange (..),
    MergeRequestObjectAttributes (..),
    MergeParams (..),
    UserEvent (..),
    BuildEvent (..),
    BuildCommit (..),
    BuildProject (..),
    PipelineEvent (..),
    PipelineObjectAttributes (..),
    PipelineBuild (..),
    IssueEvent (..),
    IssueEventObjectAttributes (..),
    IssueEventChanges (..),
    IssueChangesAuthorId (..),
    IssueChangesCreatedAt (..),
    IssueChangesDescription (..),
    IssueChangesId (..),
    IssueChangesIid (..),
    IssueChangesProjectId (..),
    IssueChangesTitle (..),
    IssueChangesClosedAt (..),
    IssueChangesStateId (..),
    IssueChangesUpdatedAt (..),
    Runner (..),
    ArtifactsFile (..),
    NoteEvent (..),
    NoteObjectAttributes (..),
    WikiPageEvent (..),
    Wiki (..),
    WikiPageObjectAttributes (..),
    WorkItemEvent (..),
    WorkItemObjectAttributes (..),
    parseEvent,
  )
where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Typeable
import GHC.Generics
import GitLab.Types

-- | Pattern matching rules on GitLab hook events.
data Rule where
  Match :: (Typeable a, SystemHook a) => String -> (a -> GitLab ()) -> Rule
  MatchIf :: (Typeable a, SystemHook a) => String -> (a -> GitLab Bool) -> (a -> GitLab ()) -> Rule

-- | A typeclass for GitLab hook events.
class (FromJSON a) => SystemHook a where
  match :: String -> (a -> GitLab ()) -> Rule
  matchIf :: String -> (a -> GitLab Bool) -> (a -> GitLab ()) -> Rule

-- | Parse JSON data into GitLab events.
parseEvent :: (FromJSON a) => Text -> Maybe a
parseEvent eventText =
  case eitherDecodeStrict (T.encodeUtf8 eventText) of
    Left _error -> Nothing
    Right event -> Just event

instance SystemHook ProjectCreate where
  match = Match
  matchIf = MatchIf

-- | GitLab project creation.
data ProjectCreate = ProjectCreate
  { projectCreate_created_at :: Text,
    projectCreate_updated_at :: Text,
    projectCreate_action :: Text,
    projectCreate_name :: Text,
    projectCreate_owner_email :: Text,
    projectCreate_owner_name :: Text,
    projectCreate_path :: Text,
    projectCreate_path_with_namespace :: Text,
    projectCreate_project_id :: Int,
    projectCreate_project_visibility :: Visibility
  }
  deriving (Typeable, Show, Eq)

instance SystemHook ProjectDestroy where
  match = Match
  matchIf = MatchIf

-- | Removal of a GitLab removal.
data ProjectDestroy = ProjectDestroy
  { projectDestroy_created_at :: Text,
    projectDestroy_updated_at :: Text,
    projectDestroy_action :: Text,
    projectDestroy_name :: Text,
    projectDestroy_owner_email :: Text,
    projectDestroy_owner_name :: Text,
    projectDestroy_path :: Text,
    projectDestroy_path_with_namespace :: Text,
    projectDestroy_project_id :: Int,
    projectDestroy_project_visibility :: Visibility
  }
  deriving (Typeable, Show, Eq)

instance SystemHook ProjectRename where
  match = Match
  matchIf = MatchIf

-- | Renaming of a GitLab project.
data ProjectRename = ProjectRename
  { projectRename_created_at :: Text,
    projectRename_updated_at :: Text,
    projectRename_event_name :: Text,
    projectRename_name :: Text,
    projectRename_path :: Text,
    projectRename_path_with_namespace :: Text,
    projectRename_project_id :: Int,
    projectRename_owner_name :: Text,
    projectRename_owner_email :: Text,
    projectRename_project_visibility :: Visibility,
    projectRename_old_path_with_namespace :: Text
  }
  deriving (Typeable, Show, Eq)

instance SystemHook ProjectTransfer where
  match = Match
  matchIf = MatchIf

-- | A project has been transferred.
data ProjectTransfer = ProjectTransfer
  { projectTransfer_created_at :: Text,
    projectTransfer_updated_at :: Text,
    projectTransfer_event_name :: Text,
    projectTransfer_name :: Text,
    projectTransfer_path :: Text,
    projectTransfer_path_with_namespace :: Text,
    projectTransfer_project_id :: Int,
    projectTransfer_owner_name :: Text,
    projectTransfer_owner_email :: Text,
    projectTransfer_project_visibility :: Visibility,
    projectTransfer_old_path_with_namespace :: Text
  }
  deriving (Typeable, Show, Eq)

instance SystemHook ProjectUpdate where
  match = Match
  matchIf = MatchIf

-- | A project has been updated.
data ProjectUpdate = ProjectUpdate
  { projectUpdate_created_at :: Text,
    projectUpdate_updated_at :: Text,
    projectUpdate_event_name :: Text,
    projectUpdate_name :: Text,
    projectUpdate_owner_email :: Text,
    projectUpdate_owner_name :: Text,
    projectUpdate_path :: Text,
    projectUpdate_path_with_namespace :: Text,
    projectUpdate_project_id :: Int,
    projectUpdate_project_visibility :: Visibility
  }
  deriving (Typeable, Show, Eq)

instance SystemHook UserAddToTeam where
  match = Match
  matchIf = MatchIf

-- | A user has been added to a team.
data UserAddToTeam = UserAddToTeam
  { userAddTeam_created_at :: Text, -- todo improve: date
    userAddTeam_updated_at :: Text, -- todo improve: date
    userAddTeam_event_name :: Text,
    userAddTeam_access_level :: Text, -- todo improve: Maintainer/...
    userAddTeam_project_id :: Int,
    userAddTeam_project_name :: Text,
    userAddTeam_project_path :: Text,
    userAddTeam_project_path_with_namespace :: Text,
    userAddTeam_user_email :: Text,
    userAddTeam_user_name :: Text,
    userAddTeam_user_username :: Text,
    userAddTeam_user_id :: Int,
    userAddTeam_project_visibility :: Visibility
  }
  deriving (Typeable, Show, Eq)

instance SystemHook UserUpdateForTeam where
  match = Match
  matchIf = MatchIf

-- | A user in a team has been updated.
data UserUpdateForTeam = UserUpdateForTeam
  { userUpdateTeam_created_at :: Text, -- todo improve: date
    userUpdateTeam_updated_at :: Text, -- todo improve: date
    userUpdateTeam_event_name :: Text,
    userUpdateTeam_access_level :: Text, -- todo improve: Maintainer/...
    userUpdateTeam_project_id :: Int,
    userUpdateTeam_project_name :: Text,
    userUpdateTeam_project_path :: Text,
    userUpdateTeam_project_path_with_namespace :: Text,
    userUpdateTeam_user_email :: Text,
    userUpdateTeam_user_name :: Text,
    userUpdateTeam_user_username :: Text,
    userUpdateTeam_user_id :: Int,
    userUpdateTeam_project_visibility :: Visibility
  }
  deriving (Typeable, Show, Eq)

instance SystemHook UserRemoveFromTeam where
  match = Match
  matchIf = MatchIf

-- | A user has been removed from a team.
data UserRemoveFromTeam = UserRemoveFromTeam
  { userRemoveTeam_created_at :: Text, -- todo improve: date
    userRemoveTeam_updated_at :: Text, -- todo improve: date
    userRemoveTeam_event_name :: Text,
    userRemoveTeam_access_level :: Text, -- todo improve: Maintainer/...
    userRemoveTeam_project_id :: Int,
    userRemoveTeam_project_name :: Text,
    userRemoveTeam_project_path :: Text,
    userRemoveTeam_project_path_with_namespace :: Text,
    userRemoveTeam_user_email :: Text,
    userRemoveTeam_user_name :: Text,
    userRemoveTeam_user_username :: Text,
    userRemoveTeam_user_id :: Int,
    userRemoveTeam_project_visibility :: Visibility
  }
  deriving (Typeable, Show, Eq)

instance SystemHook UserCreate where
  match = Match
  matchIf = MatchIf

-- | A user has been created.
data UserCreate = UserCreate
  { userCreate_created_at :: Text, -- todo improve: date
    userCreate_updated_at :: Text, -- todo improve: date
    userCreate_email :: Text,
    userCreate_event_name :: Text,
    userCreate_name :: Text,
    userCreate_username :: Text,
    userCreate_user_id :: Int
  }
  deriving (Typeable, Show, Eq)

instance SystemHook UserRemove where
  match = Match
  matchIf = MatchIf

-- | A user has been removed.
data UserRemove = UserRemove
  { userRemove_created_at :: Text, -- todo improve: date
    userRemove_updated_at :: Text, -- todo improve: date
    userRemove_email :: Text,
    userRemove_event_name :: Text,
    userRemove_name :: Text,
    userRemove_username :: Text,
    userRemove_user_id :: Int
  }
  deriving (Typeable, Show, Eq)

instance SystemHook UserFailedLogin where
  match = Match
  matchIf = MatchIf

-- | A user has failed to log in.
data UserFailedLogin = UserFailedLogin
  { userFailedLogin_event_name :: Text,
    userFailedLogin_created_at :: Text, -- todo improve: date
    userFailedLogin_updated_at :: Text, -- todo improve: date
    userFailedLogin_name :: Text,
    userFailedLogin_email :: Text,
    userFailedLogin_user_id :: Int,
    userFailedLogin_username :: Text,
    -- create Haskell sum type for this
    userFailedLogin_state :: Text
  }
  deriving (Typeable, Show, Eq)

instance SystemHook UserRename where
  match = Match
  matchIf = MatchIf

-- | A user has been renamed.
data UserRename = UserRename
  { userRename_event_name :: Text,
    userRename_created_at :: Text, -- todo improve: date
    userRename_updated_at :: Text, -- todo improve: date
    userRename_name :: Text,
    userRename_email :: Text,
    userRename_user_id :: Int,
    userRename_username :: Text,
    userRename_old_username :: Text
  }
  deriving (Typeable, Show, Eq)

instance SystemHook KeyCreate where
  match = Match
  matchIf = MatchIf

-- | A key has been created.
data KeyCreate = KeyCreate
  { keyCreate_event_name :: Text,
    keyCreate_created_at :: Text, -- todo improve: date
    keyCreate_updated_at :: Text, -- todo improve: date
    keyCreate_username :: Text,
    keyCreate_key :: Text,
    keyCreate_id :: Int
  }
  deriving (Typeable, Show, Eq)

instance SystemHook KeyRemove where
  match = Match
  matchIf = MatchIf

-- | A key has been removed.
data KeyRemove = KeyRemove
  { keyRemove_event_name :: Text,
    keyRemove_created_at :: Text, -- todo improve: date
    keyRemove_updated_at :: Text, -- todo improve: date
    keyRemove_username :: Text,
    keyRemove_key :: Text,
    keyRemove_id :: Int
  }
  deriving (Typeable, Show, Eq)

instance SystemHook GroupCreate where
  match = Match
  matchIf = MatchIf

-- | A group has been created.
data GroupCreate = GroupCreate
  { groupCreate_created_at :: Text, -- todo improve: date
    groupCreate_updated_at :: Text, -- todo improve: date
    groupCreate_event_name :: Text,
    groupCreate_name :: Text,
    groupCreate_owner_email :: Maybe Text,
    groupCreate_owner_name :: Maybe Text,
    groupCreate_path :: Text,
    groupCreate_group_id :: Int
  }
  deriving (Typeable, Show, Eq)

instance SystemHook GroupRemove where
  match = Match
  matchIf = MatchIf

-- | A group has been removed.
data GroupRemove = GroupRemove
  { groupRemove_created_at :: Text, -- todo improve: date
    groupRemove_updated_at :: Text, -- todo improve: date
    groupRemove_event_name :: Text,
    groupRemove_name :: Text,
    groupRemove_owner_email :: Maybe Text,
    groupRemove_owner_name :: Maybe Text,
    groupRemove_path :: Text,
    groupRemove_group_id :: Int
  }
  deriving (Typeable, Show, Eq)

instance SystemHook GroupRename where
  match = Match
  matchIf = MatchIf

-- | A group has been renamed.
data GroupRename = GroupRename
  { groupRename_event_name :: Text,
    groupRename_created_at :: Text, -- todo improve: date
    groupRename_updated_at :: Text, -- todo improve: date
    groupRename_name :: Text,
    groupRename_path :: Text,
    groupRename_full_path :: Text,
    groupRename_group_id :: Int,
    groupRename_owner_name :: Maybe Text,
    groupRename_owner_email :: Maybe Text,
    groupRename_old_path :: Text,
    groupRename_old_full_path :: Text
  }
  deriving (Typeable, Show, Eq)

instance SystemHook NewGroupMember where
  match = Match
  matchIf = MatchIf

-- | A user has been added to a group.
data NewGroupMember = NewGroupMember
  { newGroupMember_created_at :: Text, -- todo improve: date
    newGroupMember_updated_at :: Text, -- todo improve: date
    newGroupMember_event_name :: Text,
    newGroupMember_group_access :: Text, -- todo Haskell type for this
    newGroupMember_group_id :: Int,
    newGroupMember_group_name :: Text,
    newGroupMember_group_path :: Text,
    newGroupMember_user_email :: Text,
    newGroupMember_user_name :: Text,
    newGroupMember_user_username :: Text,
    newGroupMember_user_id :: Int
  }
  deriving (Typeable, Show, Eq)

instance SystemHook GroupMemberRemove where
  match = Match
  matchIf = MatchIf

-- | A user has been removed from a group.
data GroupMemberRemove = GroupMemberRemove
  { groupMemberRemove_created_at :: Text, -- todo improve: date
    groupMemberRemove_updated_at :: Text, -- todo improve: date
    groupMemberRemove_event_name :: Text,
    groupMemberRemove_group_access :: Text, -- todo Haskell type for this
    groupMemberRemove_group_id :: Int,
    groupMemberRemove_group_name :: Text,
    groupMemberRemove_group_path :: Text,
    groupMemberRemove_user_email :: Text,
    groupMemberRemove_user_name :: Text,
    groupMemberRemove_user_username :: Text,
    groupMemberRemove_user_id :: Int
  }
  deriving (Typeable, Show, Eq)

instance SystemHook GroupMemberUpdate where
  match = Match
  matchIf = MatchIf

-- | A group member has been updated.
data GroupMemberUpdate = GroupMemberUpdate
  { groupMemberUpdate_created_at :: Text, -- todo improve: date
    groupMemberUpdate_updated_at :: Text, -- todo improve: date
    groupMemberUpdate_event_name :: Text,
    groupMemberUpdate_group_access :: Text, -- todo Haskell type for this
    groupMemberUpdate_group_id :: Int,
    groupMemberUpdate_group_name :: Text,
    groupMemberUpdate_group_path :: Text,
    groupMemberUpdate_user_email :: Text,
    groupMemberUpdate_user_name :: Text,
    groupMemberUpdate_user_username :: Text,
    groupMemberUpdate_user_id :: Int
  }
  deriving (Typeable, Show, Eq)

instance SystemHook Push where
  match = Match
  matchIf = MatchIf

-- | Commits have been pushed to the server.
data Push = Push
  { push_event_name :: Text,
    push_before :: Text,
    push_after :: Text,
    push_ref :: Text,
    push_checkout_sha :: Maybe Text,
    push_user_id :: Int,
    push_user_name :: Text,
    push_user_username :: Maybe Text,
    push_user_email :: Maybe Text,
    push_user_avatar :: Text,
    push_project_id :: Int,
    push_project :: ProjectEvent,
    push_repository :: RepositoryEvent,
    push_commits :: [CommitEvent],
    push_total_commits_count :: Int
  }
  deriving (Typeable, Show, Eq)

instance SystemHook TagPush where
  match = Match
  matchIf = MatchIf

-- | Tags have been pushed to the server.
data TagPush = TagPush
  { tagPush_event_name :: Text,
    tagPush_before :: Text,
    tagPush_after :: Text,
    tagPush_ref :: Text,
    tagPush_checkout_sha :: Text,
    tagPush_user_id :: Int,
    tagPush_user_name :: Text,
    tagPush_user_avatar :: Text,
    tagPush_project_id :: Int,
    tagPush_project :: ProjectEvent,
    tagPush_repository :: RepositoryEvent,
    tagPush_commits :: [CommitEvent],
    tagPush_total_commits_count :: Int
  }
  deriving (Typeable, Show, Eq)

instance SystemHook RepositoryUpdate where
  match = Match
  matchIf = MatchIf

-- | Tags have been pushed to the server.
data RepositoryUpdate = RepositoryUpdate
  { repositoryUpdate_event_name :: Text,
    repositoryUpdate_user_id :: Int,
    repositoryUpdate_user_name :: Text,
    repositoryUpdate_user_email :: Text,
    repositoryUpdate_user_avatar :: Text,
    repositoryUpdate_project_id :: Int,
    repositoryUpdate_project :: ProjectEvent,
    repositoryUpdate_changes :: [ProjectChanges],
    repositoryUpdate_refs :: [Text]
  }
  deriving (Typeable, Show, Eq)

-- | A project event.
data ProjectEvent = ProjectEvent
  { projectEvent_id :: Maybe Int,
    projectEvent_name :: Text,
    projectEvent_description :: Maybe Text,
    projectEvent_web_url :: Text,
    projectEvent_avatar_url :: Maybe Text,
    projectEvent_git_ssh_url :: Text,
    projectEvent_git_http_url :: Text,
    projectEvent_namespace :: Text,
    projectEvent_visibility_level :: Visibility,
    projectEvent_path_with_namespace :: Text,
    projectEvent_default_branch :: Text,
    -- projectEvent_ci_config_path :: Maybe Text,
    projectEvent_homepage :: Maybe Text,
    projectEvent_url :: Text,
    projectEvent_ssh_url :: Text,
    projectEvent_http_url :: Text
  }
  deriving (Typeable, Show, Eq, Generic)

-- | A project event.
data ProjectChanges = ProjectChanges
  { projectChanges_before :: Text,
    projectChanges_after :: Text,
    projectChanges_ref :: Text
  }
  deriving (Typeable, Show, Eq, Generic)

-- | A repository event.
data RepositoryEvent = RepositoryEvent
  { repositoryEvent_name :: Text,
    repositoryEvent_url :: Text,
    repositoryEvent_description :: Maybe Text,
    repositoryEvent_homepage :: Maybe Text,
    -- these three not in the merge_request event example
    -- in the GitLab documentation. Is the merge_request documentation
    -- out dated?
    repositoryEvent_git_http_url :: Maybe Text,
    repositoryEvent_git_ssh_url :: Maybe Text,
    repositoryEvent_visibility_level :: Maybe Visibility
  }
  deriving (Typeable, Show, Eq, Generic)

-- | A commit event.
data CommitEvent = CommitEvent
  { commitEvent_id :: Text,
    commitEvent_message :: Text,
    commitEvent_timestamp :: Text, -- TODO improve.
    commitEvent_url :: Text,
    commitEvent_author :: CommitAuthorEvent
  }
  deriving (Typeable, Show, Eq, Generic)

-- | Commit author information.
data CommitAuthorEvent = CommitAuthorEvent
  { commitAuthorEvent_name :: Text,
    commitAuthorEvent_email :: Text
  }
  deriving (Typeable, Show, Eq, Generic)

instance SystemHook MergeRequestEvent where
  match = Match
  matchIf = MatchIf

-- | Merge request (named so, since 'MergeRequest' type already used
-- in GitLab.Types.
data MergeRequestEvent = MergeRequestEvent
  { mergeRequest_object_kind :: Text,
    mergeRequest_event_type :: Text,
    mergeRequest_user :: UserEvent,
    mergeRequest_project :: ProjectEvent,
    mergeRequest_object_attributes :: MergeRequestObjectAttributes,
    mergeRequest_labels :: Maybe [Label],
    mergeRequest_changes :: MergeRequestChanges,
    mergeRequest_repository :: RepositoryEvent
  }
  deriving (Typeable, Show, Eq, Generic)

-- | Label associated with a merge request
data Label = Label
  { label_id :: Maybe Int,
    label_title :: Maybe Text,
    label_color :: Maybe Text,
    label_project_id :: Maybe Int,
    label_created_at :: Maybe Text, -- TODO date from e.g. "2013-12-03T17:15:43Z"
    label_updated_at :: Maybe Text, -- TODO date
    label_template :: Maybe Bool,
    label_description :: Maybe Text,
    label_type :: Maybe Text, -- TODO type from "ProjectLabel"
    label_group_id :: Maybe Int
  }
  deriving (Typeable, Show, Eq, Generic)

-- | Changes that a merge request will make
data MergeRequestChanges = MergeRequestChanges
  { mergeRequestChanges_author_id :: Maybe (MergeRequestChange Int),
    mergeRequestChanges_created_at :: Maybe (MergeRequestChange Text),
    mergeRequestChanges_description :: Maybe (MergeRequestChange Text),
    mergeRequestChanges_id :: Maybe (MergeRequestChange Int),
    mergeRequestChanges_iid :: Maybe (MergeRequestChange Int),
    mergeRequestChanges_source_branch :: Maybe (MergeRequestChange Text),
    mergeRequestChanges_source_project_id :: Maybe (MergeRequestChange Int),
    mergeRequestChanges_target_branch :: Maybe (MergeRequestChange Text),
    mergeRequestChanges_target_project_id :: Maybe (MergeRequestChange Int),
    mergeRequestChanges_title :: Maybe (MergeRequestChange Text),
    mergeRequestChanges_updated_at :: Maybe (MergeRequestChange Text)
  }
  deriving (Typeable, Show, Eq, Generic)

-- | The change between for a given GitLab data field a merge request
-- will make
data MergeRequestChange a = MergeRequestChange
  { mergeRequestChange_previous :: Maybe a,
    mergeRequestChange_current :: Maybe a
  }
  deriving (Typeable, Show, Eq, Generic)

-- | Attributes associated with a merge request
data MergeRequestObjectAttributes = MergeRequestObjectAttributes
  { objectAttributes_id :: Int,
    objectAttributes_target_branch :: Text,
    objectAttributes_source_branch :: Text,
    objectAttributes_source_project_id :: Int,
    objectAttributes_author_id :: Maybe Int,
    objectAttributes_assignee_id :: Maybe Int,
    objectAttributes_assignee_ids :: Maybe [Int],
    objectAttributes_title :: Text,
    objectAttributes_created_at :: Text,
    objectAttributes_updated_at :: Text,
    objectAttributes_milestone_id :: Maybe Int,
    objectAttributes_state :: Text,
    objectAttributes_state_id :: Maybe Int,
    objectAttributes_merge_status :: Text,
    objectAttributes_target_project_id :: Int,
    objectAttributes_iid :: Int,
    objectAttributes_description :: Text,
    objectAttributes_updated_by_id :: Maybe Int,
    objectAttributes_merge_error :: Maybe Text,
    objectAttributes_merge_params :: Maybe MergeParams,
    objectAttributes_merge_when_pipeline_succeeds :: Maybe Bool,
    objectAttributes_merge_user_id :: Maybe Int,
    objectAttributes_merge_commit_sha :: Maybe Text,
    objectAttributes_deleted_at :: Maybe Text,
    objectAttributes_in_progress_merge_commit_sha :: Maybe Text,
    objectAttributes_lock_version :: Maybe Int,
    objectAttributes_time_estimate :: Maybe Int,
    objectAttributes_last_edited_at :: Maybe Text,
    objectAttributes_last_edited_by_id :: Maybe Int,
    objectAttributes_head_pipeline_id :: Maybe Int,
    objectAttributes_ref_fetched :: Maybe Bool,
    objectAttributes_merge_jid :: Maybe Int,
    objectAttributes_source :: ProjectEvent,
    objectAttributes_target :: ProjectEvent,
    objectAttributes_last_commit :: CommitEvent,
    objectAttributes_work_in_progress :: Bool,
    objectAttributes_total_time_spent :: Maybe Int,
    objectAttributes_human_total_time_spent :: Maybe Int,
    objectAttributes_human_time_estimate :: Maybe Int,
    objectAttributes_action :: Maybe Text
  }
  deriving (Typeable, Show, Eq, Generic)

-- | Merge parameters associated with a merge request
newtype MergeParams = MergeParams
  { mergeParams_force_remove_source_branch :: Maybe Text
  }
  deriving (Typeable, Show, Eq, Generic)

-- | User associated with a merge request
data UserEvent = UserEvent
  { userEvent_id :: Maybe Int,
    userEvent_name :: Text,
    userEvent_username :: Text,
    userEvent_avatar_url :: Text,
    userEvent_email :: Maybe Text
  }
  deriving (Typeable, Show, Eq, Generic)

data ProjectAction
  = ProjectCreated
  | ProjectDestroyed
  | ProjectRenamed
  | ProjectTransferred
  | ProjectUpdated
  | UserAddedToTeam
  | UserUpdatedForTeam
  | UserRemovedFromTeam
  | UserCreated
  | UserRemoved
  | UserFailedToLogin
  | UserRenamed
  | KeyCreated
  | KeyRemoved
  | GroupCreated
  | GroupRemoved
  | GroupRenamed
  | GroupMemberAdded
  | GroupMemberRemoved
  | GroupMemberUpdated
  | Pushed
  | TagPushed
  | RepositoryUpdated
  | MergeRequested
  | Built
  | Pipelined
  | Noted
  | WikiPaged
  | WorkItemed
  deriving (Show, Eq)

-- | CI build
data BuildEvent = BuildEvent
  { build_event_object_kind :: Text,
    build_event_ref :: Text,
    build_event_tag :: Bool,
    build_event_before_sha :: Text,
    build_event_sha :: Text,
    build_event_retries_count :: Int,
    build_event_build_event_id :: Int,
    build_event_build_event_name :: Text,
    build_event_build_event_stage :: Text,
    build_event_build_event_status :: Text,
    build_event_created_at :: Text,
    build_event_started_at :: Maybe Text,
    build_event_finished_at :: Maybe Text,
    build_event_duration :: Maybe Double,
    build_event_queued_duration :: Maybe Double,
    build_event_allow_failure :: Bool,
    build_event_failure_reason :: Maybe Text,
    build_event_pipeline_id :: Int,
    build_event_runner :: Maybe Runner,
    build_event_project_id :: Int,
    build_event_project_name :: Text,
    build_event_user :: User,
    build_event_commit :: BuildCommit,
    build_event_repository :: Repository,
    build_event_project :: BuildProject,
    build_event_environment :: Maybe Text
  }
  deriving (Typeable, Show, Eq, Generic)

instance SystemHook BuildEvent where
  match = Match
  matchIf = MatchIf

-- | CI build commit
data BuildCommit = BuildCommit
  { build_commit_id :: Int,
    build_commit_name :: Maybe Text,
    build_commit_sha :: Text,
    build_commit_message :: Text,
    build_commit_author_name :: Text,
    build_commit_author_email :: Text,
    build_commit_author_url :: Text,
    build_commit_status :: Text,
    build_commit_duration :: Maybe Int,
    build_commit_started_at :: Maybe Text,
    build_commit_finished_at :: Maybe Text
  }
  deriving (Typeable, Show, Eq, Generic)

-- | CI build commit
data BuildProject = BuildProject
  { build_project_project_id :: Int,
    build_project_project_name :: Text,
    build_project_description :: Maybe Text,
    build_project_web_url :: Text,
    build_project_avatar_url :: Maybe Text,
    build_project_git_ssh_url :: Text,
    build_project_git_http_url :: Text,
    build_project_namespace :: Text,
    build_project_visibility_level :: Int,
    build_project_path_with_namespace :: Text,
    build_project_default_branch :: Text,
    build_project_ci_config_path :: Maybe Text
  }
  deriving (Typeable, Show, Eq, Generic)

-- | CI pipelines
data PipelineEvent = PipelineEvent
  { pipeline_event_object_kind :: Text,
    pipeline_event_object_attributes :: PipelineObjectAttributes,
    pipeline_event_merge_request :: Maybe Text,
    pipeline_event_user :: User,
    pipeline_event_project :: BuildProject,
    pipeline_event_commit :: CommitEvent,
    pipeline_event_builds :: [PipelineBuild]
  }
  deriving (Typeable, Show, Eq, Generic)

instance SystemHook PipelineEvent where
  match = Match
  matchIf = MatchIf

-- | CI pipeline attributes
data PipelineObjectAttributes = PipelineObjectAttributes
  { pipeline_object_attributes_id :: Int,
    pipeline_object_attributes_iid :: Int,
    pipeline_object_attributes_name :: Maybe Text,
    pipeline_object_attributes_ref :: Text,
    pipeline_object_attributes_tag :: Bool,
    pipeline_object_attributes_sha :: Text,
    pipeline_object_attributes_before_sha :: Text,
    pipeline_object_attributes_source :: Text,
    pipeline_object_attributes_status :: Text,
    pipeline_object_attributes_detailed_status :: Text,
    pipeline_object_attributes_stages :: [Text],
    pipeline_object_attributes_created_at :: Text,
    pipeline_object_attributes_finished_at :: Maybe Text,
    pipeline_object_attributes_duration :: Maybe Double,
    pipeline_object_attributes_queued_duration :: Maybe Double,
    pipeline_object_attributes_variables :: [Text],
    pipeline_object_attributes_url :: Text
  }
  deriving (Typeable, Show, Eq, Generic)

-- | CI pipeline attributes
data PipelineBuild = PipelineBuild
  { pipeline_build_id :: Int,
    pipeline_build_stage :: Text,
    pipeline_build_name :: Text,
    pipeline_build_status :: Text,
    pipeline_build_created_at :: Text,
    pipeline_build_started_at :: Maybe Text,
    pipeline_build_finished_at :: Maybe Text,
    pipeline_build_duration :: Maybe Double,
    pipeline_build_queued_duration :: Double,
    pipeline_build_failure_reason :: Maybe Text,
    pipeline_build_when :: Text, -- "on_success"
    pipeline_build_manual :: Bool,
    pipeline_build_allow_failure :: Bool,
    pipeline_build_user :: UserEvent,
    pipeline_build_runner :: Maybe Runner,
    pipeline_build_artifacts_file :: ArtifactsFile,
    pipeline_build_environment :: Maybe Text
  }
  deriving (Typeable, Show, Eq, Generic)

-- | CI runner
data Runner = Runner
  { runner_id :: Int,
    runner_description :: Text,
    runner_runner_type :: Text, -- "instance_type"
    runner_active :: Bool,
    runner_is_shared :: Bool,
    runner_tags :: [Text]
  }
  deriving (Typeable, Show, Eq, Generic)

-- | CI artifacts file
data ArtifactsFile = ArtifactsFile
  { artifacts_file_filename :: Maybe Text,
    artifacts_file_size :: Maybe Int
  }
  deriving (Typeable, Show, Eq, Generic)

-- | Issue event
data IssueEvent = IssueEvent
  { issue_event_event_type :: Text,
    issue_event_user :: Maybe UserEvent,
    issue_event_project :: Maybe ProjectEvent,
    issue_event_object_attributes :: Maybe IssueEventObjectAttributes,
    issue_event_labels :: Maybe [Label],
    issue_event_changes :: Maybe IssueEventChanges,
    issue_event_repository :: Maybe RepositoryEvent,
    issue_event_assignees :: Maybe [UserEvent]
  }
  deriving (Typeable, Show, Eq, Generic)

instance SystemHook IssueEvent where
  match = Match
  matchIf = MatchIf

-- | Issue event object attributes
data IssueEventObjectAttributes = IssueEventObjectAttributes
  { issue_event_object_attributes_author_id :: Int,
    issue_event_object_attributes_closed_at :: Maybe Text, -- change to UTCTime
    issue_event_object_attributes_confidential :: Bool,
    issue_event_object_attributes_created_at :: Text, -- change to UTCTime
    issue_event_object_attributes_description :: Maybe Text,
    issue_event_object_attributes_discussion_locked :: Maybe Bool,
    issue_event_object_attributes_due_date :: Maybe Text, -- change to UTCTime
    issue_event_object_attributes_id :: Int,
    issue_event_object_attributes_iid :: Int,
    issue_event_object_attributes_last_edited_at :: Maybe Text, -- change to UTCTime
    issue_event_object_attributes_last_edited_by_id :: Maybe Int,
    issue_event_object_attributes_milestone_id :: Maybe Int,
    issue_event_object_attributes_move_to_id :: Maybe Int,
    issue_event_object_attributes_duplicated_to_id :: Maybe Int,
    issue_event_object_attributes_project_id :: Int,
    issue_event_object_attributes_relative_position :: Maybe Int,
    issue_event_object_attributes_state_id :: Int,
    issue_event_object_attributes_time_estimate :: Int,
    issue_event_object_attributes_title :: Text,
    issue_event_object_attributes_updated_at :: Text, -- change to UTCTime
    issue_event_object_attributes_updated_by_id :: Maybe Int,
    issue_event_object_attributes_type :: Text, -- "Issue"
    issue_event_object_attributes_url :: Text,
    issue_event_object_attributes_total_time_spent :: Int,
    issue_event_object_attributes_time_change :: Int,
    issue_event_object_attributes_human_total_time_spent :: Maybe Int,
    issue_event_object_attributes_human_time_change :: Maybe Int,
    issue_event_object_attributes_human_time_estimate :: Maybe Int
  }
  deriving (Typeable, Show, Eq, Generic)

-- | Issue event changes
data IssueEventChanges = IssueEventChanges
  { issue_event_changes_author_id :: Maybe IssueChangesAuthorId,
    issue_event_changes_created_at :: Maybe IssueChangesCreatedAt,
    issue_event_changes_description :: Maybe IssueChangesDescription,
    issue_event_changes_id :: Maybe IssueChangesId,
    issue_event_changes_iid :: Maybe IssueChangesIid,
    issue_event_changes_project_id :: Maybe IssueChangesProjectId,
    issue_event_changes_title :: Maybe IssueChangesTitle,
    issue_event_changes_closed_at :: Maybe IssueChangesClosedAt,
    issue_event_changes_state_id :: Maybe IssueChangesStateId,
    issue_event_changes_updated_at :: Maybe IssueChangesUpdatedAt
  }
  deriving (Typeable, Show, Eq, Generic)

-- | Issue event author ID
data IssueChangesAuthorId = IssueChangesAuthorId
  { issue_event_author_id_previous :: Maybe Int,
    issue_event_author_id_current :: Int
  }
  deriving (Typeable, Show, Eq, Generic)

-- | Issue event created at
data IssueChangesCreatedAt = IssueChangesCreatedAt
  { issue_event_created_at_previous :: Maybe Text, -- change to URLTime
    issue_event_created_at_current :: Text -- change to URLTime
  }
  deriving (Typeable, Show, Eq, Generic)

-- | Issue event description
data IssueChangesDescription = IssueChangesDescription
  { issue_event_description_previous :: Maybe Text,
    issue_event_description_current :: Text
  }
  deriving (Typeable, Show, Eq, Generic)

-- | Issue event ID
data IssueChangesId = IssueChangesId
  { issue_event_id_previous :: Maybe Int,
    issue_event_id_current :: Int
  }
  deriving (Typeable, Show, Eq, Generic)

-- | Issue event IID
data IssueChangesIid = IssueChangesIid
  { issue_event_iid_previous :: Maybe Int,
    issue_event_iid_current :: Int
  }
  deriving (Typeable, Show, Eq, Generic)

-- | Issue event project ID
data IssueChangesProjectId = IssueChangesProjectId
  { issue_event_project_id_previous :: Maybe Int,
    issue_event_project_id_current :: Int
  }
  deriving (Typeable, Show, Eq, Generic)

-- | Issue event title
data IssueChangesTitle = IssueChangesTitle
  { issue_event_title_previous :: Maybe Text,
    issue_event_title_current :: Text
  }
  deriving (Typeable, Show, Eq, Generic)

-- | Issue event closed at
data IssueChangesClosedAt = IssueChangesClosedAt
  { issue_event_closed_at_previous :: Maybe Text, -- change to URLTime
    issue_event_closed_at_current :: Maybe Text -- change to URLTime
  }
  deriving (Typeable, Show, Eq, Generic)

-- | Issue event state id
data IssueChangesStateId = IssueChangesStateId
  { issue_event_state_id_previous :: Maybe Int,
    issue_event_state_id_current :: Int
  }
  deriving (Typeable, Show, Eq, Generic)

-- | Issue event updated at
data IssueChangesUpdatedAt = IssueChangesUpdatedAt
  { issue_event_updated_at_previous :: Maybe Text, -- change to URLTime
    issue_event_updated_at_current :: Text -- change to URLTime
  }
  deriving (Typeable, Show, Eq, Generic)

-- -- | Issue event state id
-- data IssueEventUpdatedAt = IssueEventUpdatedAt
--   { issue_event_closed_at_previous :: Maybe Text, -- change to URLTime
--     issue_event_closed_at_current :: Text -- change to URLTime
--   }
--   deriving (Typeable, Show, Eq, Generic)

-- | Note event
data NoteEvent = NoteEvent
  { note_event_object_kind :: Text,
    note_event_event_type :: Text,
    note_event_user :: User,
    note_event_project_id :: Int,
    note_event_project :: ProjectEvent,
    note_event_object_attributes :: NoteObjectAttributes,
    note_event_repository :: RepositoryEvent,
    note_event_issue :: Maybe IssueEventObjectAttributes
  }
  deriving (Typeable, Show, Eq, Generic)

instance SystemHook NoteEvent where
  match = Match
  matchIf = MatchIf

-- | CI pipeline attributes
data NoteObjectAttributes = NoteObjectAttributes
  { note_object_attributes_attachment :: Maybe Text, -- ?
    note_object_attributes_author_id :: Int,
    note_object_attributes_change_position :: Maybe Text, -- ?
    note_object_attributes_commit_id :: Maybe String,
    note_object_attributes_created_at :: Text, -- change to date
    note_object_attributes_discussion_id :: Text,
    note_object_attributes_id :: Int,
    note_object_attributes_line_code :: Maybe Int, -- ?
    note_object_attributes_note :: Text,
    note_object_attributes_noteable_id :: Maybe Int,
    note_object_attributes_noteable_type :: Text, -- "Issue"
    note_object_attributes_original_position :: Maybe Int, -- ?
    note_object_attributes_position :: Maybe Int, -- ?
    note_object_attributes_project_id :: Int,
    note_object_attributes_resolved_at :: Maybe Text, -- date?
    note_object_attributes_resolved_by_id :: Maybe Int,
    note_object_attributes_resolved_by_push :: Maybe Int, -- ?
    note_object_attributes_st_diff :: Maybe Text, -- ?
    note_object_attributes_system :: Bool,
    note_object_attributes_type :: Maybe Text, -- ?
    note_object_attributes_updated_at :: Maybe Text, -- date?
    note_object_attributes_updated_by_id :: Maybe Int, -- ?
    note_object_attributes_description :: Text,
    note_object_attributes_url :: Text,
    note_object_attributes_action :: Text -- "create"
  }
  deriving (Typeable, Show, Eq, Generic)

-- | Note event
data WikiPageEvent = WikiPageEvent
  { wiki_page_event_object_kind :: Text,
    wiki_page_event_user :: User,
    -- wiki_page_event_project :: Maybe Project, -- this will not parse but the JSON elements are too different from Project type
    wiki_page_event_wiki :: Wiki,
    wiki_page_event_object_attributes :: WikiPageObjectAttributes
  }
  deriving (Typeable, Show, Eq, Generic)

instance SystemHook WikiPageEvent where
  match = Match
  matchIf = MatchIf

data Wiki = Wiki
  { wiki_web_url :: Maybe Text, -- use URL related type in future for these
    wiki_git_ssh_url :: Maybe Text,
    wiki_git_http_url :: Maybe Text,
    wiki_path_with_namespace :: Maybe Text,
    wiki_default_branch :: Maybe Text
  }
  deriving (Typeable, Show, Eq, Generic)

data WikiPageObjectAttributes = WikiPageObjectAttributes
  { wiki_page_object_attributes_slug :: Maybe Text,
    wiki_page_object_attributes_title :: Maybe Text,
    wiki_page_object_attributes_format :: Maybe Text,
    wiki_page_object_attributes_message :: Maybe Text,
    wiki_page_object_attributes_version_id :: Maybe Text,
    wiki_page_object_attributes_url :: Maybe Text,
    wiki_page_object_attributes_action :: Maybe Text, -- 'update' .. better type in future?
    wiki_page_object_attributes_diff_url :: Maybe Text
  }
  deriving (Typeable, Show, Eq, Generic)

-- TODO remove object_kind fields, they can be discarded after checking their values.

data WorkItemEvent = WorkItemEvent
  { work_item_event_user :: User,
    work_item_event_object_attributes :: WorkItemObjectAttributes,
    work_item_event_labels :: [Label],
    -- work_item_event_changes :: Change,
    work_item_event_repository :: Repository,
    work_item_event_assignees :: [User]
  }
  deriving (Typeable, Show, Eq, Generic)

instance SystemHook WorkItemEvent where
  match = Match
  matchIf = MatchIf

data WorkItemObjectAttributes = WorkItemObjectAttributes
  { work_item_object_author_id :: Int,
    work_item_object_closed_at :: Text, -- change to date type
    work_item_object_confidential :: Bool,
    work_item_object_created_at :: Text, -- change to date type
    work_item_object_description :: Maybe Text,
    work_item_object_discussion_locked :: Maybe Bool,
    work_item_object_due_date :: Maybe Text, -- chanage to date type
    work_item_object_id :: Int,
    work_item_object_iid :: Int,
    work_item_object_last_edited_at :: Maybe Text,
    work_item_object_last_edited_by_id :: Maybe Int,
    work_item_object_milestone_id :: Maybe Int,
    work_item_object_moved_to_id :: Maybe Int,
    work_item_object_duplicated_to_id :: Maybe Int,
    work_item_object_project_id :: Int,
    work_item_object_relative_position :: Int,
    work_item_object_state_id :: Int,
    work_item_object_time_estimate :: Int,
    work_item_object_title :: Text,
    work_item_object_updated_at :: Text,
    work_item_object_updated_by_id :: Int,
    work_item_object_type :: Text, -- change to own type e.g. "Task"
    work_item_object_url :: Text,
    work_item_object_total_time_spent :: Int,
    work_item_object_time_change :: Int,
    work_item_object_human_total_time_spent :: Maybe Int,
    work_item_object_human_time_change :: Maybe Int,
    work_item_object_human_time_estimate :: Maybe Int,
    work_item_object_assignee_ids :: [Int],
    work_item_object_assignee_id :: Int,
    work_item_object_labels :: [Label],
    work_item_object_state :: Text, -- change to own type e.g. "closed"
    work_item_object_severity :: Text, -- change to own type e.g. "unknown"
    work_item_object_customer_relations_contacts :: [Text],
    work_item_object_action :: Text -- change to own type e.g. "close"
  }
  deriving (Typeable, Show, Eq, Generic)

instance FromJSON ProjectCreate where
  parseJSON =
    withObject "ProjectCreate" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            ProjectCreated ->
              ProjectCreate
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "name"
                <*> v .: "owner_email"
                <*> v .: "owner_name"
                <*> v .: "path"
                <*> v .: "path_with_namespace"
                <*> v .: "project_id"
                <*> v .: "project_visibility"
            _unexpected -> fail "project_create parsing failed"
        _unexpected -> fail "project_create parsing failed"

instance FromJSON ProjectDestroy where
  parseJSON =
    withObject "ProjectDestroy" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            ProjectDestroyed ->
              ProjectDestroy
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "name"
                <*> v .: "owner_email"
                <*> v .: "owner_name"
                <*> v .: "path"
                <*> v .: "path_with_namespace"
                <*> v .: "project_id"
                <*> v .: "project_visibility"
            _unexpected -> fail "project_destroy parsing failed"
        _unexpected -> fail "project_destroy parsing failed"

instance FromJSON ProjectRename where
  parseJSON =
    withObject "ProjectRename" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            ProjectRenamed ->
              ProjectRename
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "name"
                <*> v .: "path"
                <*> v .: "path_with_namespace"
                <*> v .: "project_id"
                <*> v .: "owner_name"
                <*> v .: "owner_email"
                <*> v .: "project_visibility"
                <*> v .: "old_path_with_namespace"
            _unexpected -> fail "project_rename parsing failed"
        _unexpected -> fail "project_rename parsing failed"

instance FromJSON ProjectTransfer where
  parseJSON =
    withObject "ProjectTransfer" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            ProjectTransferred ->
              ProjectTransfer
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "name"
                <*> v .: "path"
                <*> v .: "path_with_namespace"
                <*> v .: "project_id"
                <*> v .: "owner_name"
                <*> v .: "owner_email"
                <*> v .: "project_visibility"
                <*> v .: "old_path_with_namespace"
            _unexpected -> fail "project_transfer parsing failed"
        _unexpected -> fail "project_transfer parsing failed"

instance FromJSON ProjectUpdate where
  parseJSON =
    withObject "ProjectUpdate" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            ProjectUpdated ->
              ProjectUpdate
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "name"
                <*> v .: "owner_email"
                <*> v .: "owner_name"
                <*> v .: "path"
                <*> v .: "path_with_namespace"
                <*> v .: "project_id"
                <*> v .: "project_visibility"
            _unexpected -> fail "project_update parsing failed"
        _unexpected -> fail "project_update parsing failed"

instance FromJSON UserAddToTeam where
  parseJSON =
    withObject "UserAddToTeam" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            UserAddedToTeam ->
              UserAddToTeam
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "access_level"
                <*> v .: "project_id"
                <*> v .: "project_name"
                <*> v .: "project_path"
                <*> v .: "project_path_with_namespace"
                <*> v .: "user_email"
                <*> v .: "user_name"
                <*> v .: "user_username"
                <*> v .: "user_id"
                <*> v .: "project_visibility"
            _unexpected -> fail "user_add_to_team parsing failed"
        _unexpected -> fail "user_add_to_team parsing failed"

instance FromJSON UserUpdateForTeam where
  parseJSON =
    withObject "UserUpdateForTeam" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            UserUpdatedForTeam ->
              UserUpdateForTeam
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "access_level"
                <*> v .: "project_id"
                <*> v .: "project_name"
                <*> v .: "project_path"
                <*> v .: "project_path_with_namespace"
                <*> v .: "user_email"
                <*> v .: "user_name"
                <*> v .: "user_username"
                <*> v .: "user_id"
                <*> v .: "project_visibility"
            _unexpected -> fail "user_update_for_team parsing failed"
        _unexpected -> fail "user_update_for_team parsing failed"

instance FromJSON UserRemoveFromTeam where
  parseJSON =
    withObject "UserRemoveFromTeam" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            UserRemovedFromTeam ->
              UserRemoveFromTeam
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "access_level"
                <*> v .: "project_id"
                <*> v .: "project_name"
                <*> v .: "project_path"
                <*> v .: "project_path_with_namespace"
                <*> v .: "user_email"
                <*> v .: "user_name"
                <*> v .: "user_username"
                <*> v .: "user_id"
                <*> v .: "project_visibility"
            _unexpected -> fail "user_remove_from_team parsing failed"
        _unexpected -> fail "user_remove_from_team parsing failed"

instance FromJSON UserCreate where
  parseJSON =
    withObject "UserCreate" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            UserCreated ->
              UserCreate
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "email"
                <*> v .: "event_name"
                <*> v .: "name"
                <*> v .: "username"
                <*> v .: "user_id"
            _unexpected -> fail "user_create parsing failed"
        _unexpected -> fail "user_create parsing failed"

instance FromJSON UserRemove where
  parseJSON =
    withObject "UserRemove" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            UserRemoved ->
              UserRemove
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "email"
                <*> v .: "event_name"
                <*> v .: "name"
                <*> v .: "username"
                <*> v .: "user_id"
            _unexpected -> fail "user_destroy parsing failed"
        _unexpected -> fail "user_destroy parsing failed"

instance FromJSON UserFailedLogin where
  parseJSON =
    withObject "UserFailedLogin" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            UserFailedToLogin ->
              UserFailedLogin
                <$> v .: "event_name"
                <*> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "name"
                <*> v .: "email"
                <*> v .: "user_id"
                <*> v .: "username"
                <*> v .: "state"
            _unexpected -> fail "user_failed_login parsing failed"
        _unexpected -> fail "user_failed_login parsing failed"

instance FromJSON UserRename where
  parseJSON =
    withObject "UserRename" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            UserRenamed ->
              UserRename
                <$> v .: "event_name"
                <*> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "name"
                <*> v .: "email"
                <*> v .: "user_id"
                <*> v .: "username"
                <*> v .: "old_username"
            _unexpected -> fail "user_rename parsing failed"
        _unexpected -> fail "user_rename parsing failed"

instance FromJSON KeyCreate where
  parseJSON =
    withObject "KeyCreate" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            KeyCreated ->
              KeyCreate
                <$> v .: "event_name"
                <*> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "username"
                <*> v .: "key"
                <*> v .: "id"
            _unexpected -> fail "key_create parsing failed"
        _unexpected -> fail "key_create parsing failed"

instance FromJSON KeyRemove where
  parseJSON =
    withObject "KeyRemove" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            KeyRemoved ->
              KeyRemove
                <$> v .: "event_name"
                <*> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "username"
                <*> v .: "key"
                <*> v .: "id"
            _unexpected -> fail "key_destroy parsing failed"
        _unexpected -> fail "key_destroy parsing failed"

instance FromJSON GroupCreate where
  parseJSON =
    withObject "GroupCreate" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            GroupCreated ->
              GroupCreate
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "name"
                <*> v .:? "owner_email"
                <*> v .:? "owner_name"
                <*> v .: "path"
                <*> v .: "group_id"
            _unexpected -> fail "group_create parsing failed"
        _unexpected -> fail "group_create parsing failed"

instance FromJSON GroupRemove where
  parseJSON =
    withObject "GroupRemove" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            GroupRemoved ->
              GroupRemove
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "name"
                <*> v .: "owner_email"
                <*> v .: "owner_name"
                <*> v .: "path"
                <*> v .: "group_id"
            _unexpected -> fail "group_remove parsing failed"
        _unexpected -> fail "group_remove parsing failed"

instance FromJSON GroupRename where
  parseJSON =
    withObject "GroupRename" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            GroupRenamed ->
              GroupRename
                <$> v .: "event_name"
                <*> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "name"
                <*> v .: "path"
                <*> v .: "full_path"
                <*> v .: "group_id"
                <*> v .: "owner_name"
                <*> v .: "owner_email"
                <*> v .: "old_path"
                <*> v .: "old_full_path"
            _unexpected -> fail "group_rename parsing failed"
        _unexpected -> fail "group_rename parsing failed"

instance FromJSON NewGroupMember where
  parseJSON =
    withObject "NewGroupMember" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            GroupMemberAdded ->
              NewGroupMember
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "group_access"
                <*> v .: "group_id"
                <*> v .: "group_name"
                <*> v .: "group_path"
                <*> v .: "user_email"
                <*> v .: "user_name"
                <*> v .: "user_username"
                <*> v .: "user_id"
            _unexpected -> fail "user_add_to_group parsing failed"
        _unexpected -> fail "user_add_to_group parsing failed"

instance FromJSON GroupMemberRemove where
  parseJSON =
    withObject "GroupMemberRemove" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            GroupMemberRemoved ->
              GroupMemberRemove
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "group_access"
                <*> v .: "group_id"
                <*> v .: "group_name"
                <*> v .: "group_path"
                <*> v .: "user_email"
                <*> v .: "user_name"
                <*> v .: "user_username"
                <*> v .: "user_id"
            _unexpected -> fail "user_remove_from_group parsing failed"
        _unexpected -> fail "user_remove_from_group parsing failed"

instance FromJSON GroupMemberUpdate where
  parseJSON =
    withObject "GroupMemberUpdate" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            GroupMemberUpdated ->
              GroupMemberUpdate
                <$> v .: "created_at"
                <*> v .: "updated_at"
                <*> v .: "event_name"
                <*> v .: "group_access"
                <*> v .: "group_id"
                <*> v .: "group_name"
                <*> v .: "group_path"
                <*> v .: "user_email"
                <*> v .: "user_name"
                <*> v .: "user_username"
                <*> v .: "user_id"
            _unexpected -> fail "user_update_for_group parsing failed"
        _unexpected -> fail "user_update_for_group parsing failed"

instance FromJSON Push where
  parseJSON =
    withObject "Push" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            Pushed ->
              Push
                <$> v .: "event_name"
                <*> v .: "before"
                <*> v .: "after"
                <*> v .: "ref"
                <*> v .: "checkout_sha"
                <*> v .: "user_id"
                <*> v .: "user_name"
                <*> v .: "user_username"
                <*> v .: "user_email"
                <*> v .: "user_avatar"
                <*> v .: "project_id"
                <*> v .: "project"
                <*> v .: "repository"
                <*> v .: "commits"
                <*> v .: "total_commits_count"
            _unexpected -> fail "push parsing failed"
        _unexpected -> fail "push parsing failed"

instance FromJSON TagPush where
  parseJSON =
    withObject "TagPush" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            TagPushed ->
              TagPush
                <$> v .: "event_name"
                <*> v .: "before"
                <*> v .: "after"
                <*> v .: "ref"
                <*> v .: "checkout_sha"
                <*> v .: "user_id"
                <*> v .: "user_name"
                <*> v .: "user_avatar"
                <*> v .: "project_id"
                <*> v .: "project"
                <*> v .: "repository"
                <*> v .: "commits"
                <*> v .: "total_commits_count"
            _unexpected -> fail "tag_push parsing failed"
        _unexpected -> fail "tag_push parsing failed"

instance FromJSON RepositoryUpdate where
  parseJSON =
    withObject "RepositoryUpdate" $ \v -> do
      isProjectEvent <- v .:? "event_name"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            RepositoryUpdated ->
              RepositoryUpdate
                <$> v .: "event_name"
                <*> v .: "user_id"
                <*> v .: "user_name"
                <*> v .: "user_email"
                <*> v .: "user_avatar"
                <*> v .: "project_id"
                <*> v .: "project"
                <*> v .: "changes"
                <*> v .: "refs"
            _unexpected -> fail "repository_update parsing failed"
        _unexpected -> fail "repository_update parsing failed"

instance FromJSON MergeRequestEvent where
  parseJSON =
    withObject "MergeRequestEvent" $ \v -> do
      -- Note: it's `event_name` in all other examples, but the GitLab
      -- documentation for MergeRequests says `object_kind`.
      --
      -- `object_kind` has been tried.
      --
      -- Bug in GitLab system hooks documentation?
      isProjectEvent <- v .:? "object_kind"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            MergeRequested ->
              MergeRequestEvent
                <$> v .: "object_kind"
                <*> v .: "event_type"
                <*> v .: "user"
                <*> v .: "project"
                <*> v .: "object_attributes"
                <*> v .: "labels"
                <*> v .: "changes"
                <*> v .: "repository"
            _unexpected -> fail "merge_request parsing failed"
        _unexpected -> fail "merge_request parsing failed"

instance FromJSON BuildEvent where
  parseJSON =
    withObject "BuildEvent" $ \v -> do
      isProjectEvent <- v .:? "object_kind"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            Built ->
              BuildEvent
                <$> v .: "object_kind"
                <*> v .: "ref"
                <*> v .: "tag"
                <*> v .: "before_sha"
                <*> v .: "sha"
                <*> v .: "retries_count"
                <*> v .: "build_id"
                <*> v .: "build_name"
                <*> v .: "build_stage"
                <*> v .: "build_status"
                <*> v .: "build_created_at"
                <*> v .: "build_started_at"
                <*> v .: "build_finished_at"
                <*> v .: "build_duration"
                <*> v .: "build_queued_duration"
                <*> v .: "build_allow_failure"
                <*> v .: "build_failure_reason"
                <*> v .: "pipeline_id"
                <*> v .: "runner"
                <*> v .: "project_id"
                <*> v .: "project_name"
                <*> v .: "user"
                <*> v .: "commit"
                <*> v .: "repository"
                <*> v .: "project"
                <*> v .: "environment"
            unexpected -> fail ("build parsing failed: " <> show unexpected)
        unexpected -> fail ("build parsing failed: " <> show unexpected)

instance FromJSON PipelineEvent where
  parseJSON =
    withObject "Pipeline" $ \v -> do
      isProjectEvent <- v .:? "object_kind"
      case isProjectEvent of
        Just theEvent ->
          case theEvent of
            Pipelined ->
              PipelineEvent
                <$> v .: "object_kind"
                <*> v .: "object_attributes"
                <*> v .: "merge_request"
                <*> v .: "user"
                <*> v .: "project"
                <*> v .: "commit"
                <*> v .: "builds"
            _unexpected -> fail "build parsing failed"
        _unexpected -> fail "build parsing failed"

instance FromJSON NoteEvent where
  parseJSON =
    withObject "NoteEvent" $ \v -> do
      isNoteEvent <- v .:? "object_kind"
      case isNoteEvent of
        Just theEvent ->
          case theEvent of
            Noted ->
              NoteEvent
                <$> v .: "object_kind"
                <*> v .: "event_type"
                <*> v .: "user"
                <*> v .: "project_id"
                <*> v .: "project"
                <*> v .: "object_attributes"
                <*> v .: "repository"
                <*> v .:? "issue"
            _unexpected -> fail "note parsing failed"
        _unexpected -> fail "note parsing failed"

instance FromJSON WikiPageEvent where
  parseJSON =
    withObject "WikiPageEvent" $ \v -> do
      isWikiPageEvent <- v .:? "object_kind"
      case isWikiPageEvent of
        Just theEvent ->
          case theEvent of
            WikiPaged ->
              WikiPageEvent
                <$> v .: "object_kind"
                <*> v .: "user"
                <*> v .: "wiki"
                <*> v .: "object_attributes"
            _unexpected -> fail "wiki page parsing failed"
        _unexpected -> fail "wiki page parsing failed"

instance FromJSON WorkItemEvent where
  parseJSON =
    withObject "WorkItemEvent" $ \v -> do
      isWorkItemEvent <- v .:? "object_kind"
      case isWorkItemEvent of
        Just theEvent ->
          case theEvent of
            WorkItemed ->
              WorkItemEvent
                <$> v .: "user"
                <*> v .: "object_attributes"
                <*> v .: "labels"
                -- <*> v .: "changes"
                <*> v .: "repository"
                <*> v .: "assignees"
            _unexpected -> fail "work item parsing failed"
        _unexpected -> fail "work item parsing failed"

-- TODO: replace bodyNoPrefix with a template Haskell based approach in src/GitLab/Types.hs
-- e.g.

-- $(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "event_"), omitNothingFields = True} ''Event)

bodyNoPrefix :: String -> String
bodyNoPrefix "projectEvent_id" = "id"
bodyNoPrefix "projectEvent_name" = "name"
bodyNoPrefix "projectEvent_description" = "description"
bodyNoPrefix "projectEvent_web_url" = "web_url"
bodyNoPrefix "projectEvent_avatar_url" = "avatar_url"
bodyNoPrefix "projectEvent_git_ssh_url" = "git_ssh_url"
bodyNoPrefix "projectEvent_git_http_url" = "git_http_url"
bodyNoPrefix "projectEvent_namespace" = "namespace"
bodyNoPrefix "projectEvent_visibility_level" = "visibility_level"
bodyNoPrefix "projectEvent_path_with_namespace" = "path_with_namespace"
bodyNoPrefix "projectEvent_default_branch" = "default_branch"
-- bodyNoPrefix "projectEvent_ci_config_path" = "ci_config_path"
bodyNoPrefix "projectEvent_homepage" = "homepage"
bodyNoPrefix "projectEvent_url" = "url"
bodyNoPrefix "projectEvent_ssh_url" = "ssh_url"
bodyNoPrefix "projectEvent_http_url" = "http_url"
bodyNoPrefix "projectChanges_before" = "before"
bodyNoPrefix "projectChanges_after" = "after"
bodyNoPrefix "projectChanges_ref" = "ref"
bodyNoPrefix "repositoryEvent_name" = "name"
bodyNoPrefix "repositoryEvent_url" = "url"
bodyNoPrefix "repositoryEvent_description" = "description"
bodyNoPrefix "repositoryEvent_homepage" = "homepage"
bodyNoPrefix "repositoryEvent_git_http_url" = "git_http_url"
bodyNoPrefix "repositoryEvent_git_ssh_url" = "git_ssh_url"
bodyNoPrefix "repositoryEvent_visibility_level" = "visibility_level"
bodyNoPrefix "commitEvent_id" = "id"
bodyNoPrefix "commitEvent_message" = "message"
bodyNoPrefix "commitEvent_timestamp" = "timestamp"
bodyNoPrefix "commitEvent_url" = "url"
bodyNoPrefix "commitEvent_author" = "author"
bodyNoPrefix "commitAuthorEvent_name" = "name"
bodyNoPrefix "commitAuthorEvent_email" = "email"
bodyNoPrefix "mergeParams_force_remove_source_branch" = "force_remove_source_branch"
bodyNoPrefix "userEvent_name" = "name"
bodyNoPrefix "userEvent_username" = "username"
bodyNoPrefix "userEvent_avatar_url" = "avatar_url"
bodyNoPrefix "objectAttributes_id" = "id"
bodyNoPrefix "objectAttributes_target_branch" = "target_branch"
bodyNoPrefix "objectAttributes_source_branch" = "source_branch"
bodyNoPrefix "objectAttributes_source_project_id" = "source_project_id"
bodyNoPrefix "objectAttributes_author_id" = "author_id"
bodyNoPrefix "objectAttributes_assignee_id" = "assignee_id"
bodyNoPrefix "objectAttributes_assignee_ids" = "assignee_ids"
bodyNoPrefix "objectAttributes_title" = "title"
bodyNoPrefix "objectAttributes_created_at" = "created_at"
bodyNoPrefix "objectAttributes_updated_at" = "updated_at"
bodyNoPrefix "objectAttributes_milestone_id" = "milestone_id"
bodyNoPrefix "objectAttributes_state" = "state"
bodyNoPrefix "objectAttributes_state_id" = "state_id"
bodyNoPrefix "objectAttributes_merge_status" = "merge_status"
bodyNoPrefix "objectAttributes_target_project_id" = "target_project_id"
bodyNoPrefix "objectAttributes_iid" = "iid"
bodyNoPrefix "objectAttributes_description" = "description"
bodyNoPrefix "objectAttributes_updated_by_id" = "updated_by_id"
bodyNoPrefix "objectAttributes_merge_error" = "merge_error"
bodyNoPrefix "objectAttributes_merge_params" = "merge_params"
bodyNoPrefix "objectAttributes_merge_when_pipeline_succeeds" = "merge_when_pipeline_succeeds"
bodyNoPrefix "objectAttributes_merge_user_id" = "merge_user_id"
bodyNoPrefix "objectAttributes_merge_commit_sha" = "merge_commit_sha"
bodyNoPrefix "objectAttributes_deleted_at" = "deleted_at"
bodyNoPrefix "objectAttributes_in_progress_merge_commit_sha" = "in_progress_merge_commit_sha"
bodyNoPrefix "objectAttributes_lock_version" = "lock_version"
bodyNoPrefix "objectAttributes_time_estimate" = "time_estimate"
bodyNoPrefix "objectAttributes_last_edited_at" = "last_edited_at"
bodyNoPrefix "objectAttributes_last_edited_by_id" = "last_edited_by_id"
bodyNoPrefix "objectAttributes_head_pipeline_id" = "head_pipeline_id"
bodyNoPrefix "objectAttributes_ref_fetched" = "ref_fetched"
bodyNoPrefix "objectAttributes_merge_jid" = "merge_jid"
bodyNoPrefix "objectAttributes_source" = "source"
bodyNoPrefix "objectAttributes_target" = "target"
bodyNoPrefix "objectAttributes_last_commit" = "last_commit"
bodyNoPrefix "objectAttributes_work_in_progress" = "work_in_progress"
bodyNoPrefix "objectAttributes_total_time_spent" = "total_time_spent"
bodyNoPrefix "objectAttributes_human_total_time_spent" = "human_total_time_spent"
bodyNoPrefix "objectAttributes_human_time_estimate" = "human_time_estimate"
bodyNoPrefix "objectAttributes_action" = "action"
bodyNoPrefix "mergeRequestChanges_author_id" = "author_id"
bodyNoPrefix "mergeRequestChanges_created_at" = "created_at"
bodyNoPrefix "mergeRequestChanges_description" = "description"
bodyNoPrefix "mergeRequestChanges_id" = "id"
bodyNoPrefix "mergeRequestChanges_iid" = "iid"
bodyNoPrefix "mergeRequestChanges_source_branch" = "source_branch"
bodyNoPrefix "mergeRequestChanges_source_project_id" = "source_project_id"
bodyNoPrefix "mergeRequestChanges_target_branch" = "target_branch"
bodyNoPrefix "mergeRequestChanges_target_project_id" = "target_project_id"
bodyNoPrefix "mergeRequestChanges_title" = "title"
bodyNoPrefix "mergeRequestChanges_updated_at" = "updated_at"
bodyNoPrefix "mergeRequestChange_previous" = "previous"
bodyNoPrefix "mergeRequestChange_current" = "current"
bodyNoPrefix "build_commit_id" = "id"
bodyNoPrefix "build_commit_name" = "name"
bodyNoPrefix "build_commit_sha" = "sha"
bodyNoPrefix "build_commit_message" = "message"
bodyNoPrefix "build_commit_author_name" = "author_name"
bodyNoPrefix "build_commit_author_email" = "author_email"
bodyNoPrefix "build_commit_author_url" = "author_url"
bodyNoPrefix "build_commit_status" = "status"
bodyNoPrefix "build_commit_duration" = "duration"
bodyNoPrefix "build_commit_started_at" = "started_at"
bodyNoPrefix "build_commit_finished_at" = "finished_at"
bodyNoPrefix "build_project_project_id" = "id"
bodyNoPrefix "build_project_project_name" = "name"
bodyNoPrefix "build_project_description" = "description"
bodyNoPrefix "build_project_web_url" = "web_url"
bodyNoPrefix "build_project_avatar_url" = "avatar_url"
bodyNoPrefix "build_project_git_ssh_url" = "git_ssh_url"
bodyNoPrefix "build_project_git_http_url" = "git_http_url"
bodyNoPrefix "build_project_namespace" = "namespace"
bodyNoPrefix "build_project_visibility_level" = "visibility_level"
bodyNoPrefix "build_project_path_with_namespace" = "path_with_namespace"
bodyNoPrefix "build_project_default_branch" = "default_branch"
bodyNoPrefix "build_project_ci_config_path" = "ci_config_path"
bodyNoPrefix "pipeline_object_attributes_id" = "id"
bodyNoPrefix "pipeline_object_attributes_iid" = "iid"
bodyNoPrefix "pipeline_object_attributes_name" = "name"
bodyNoPrefix "pipeline_object_attributes_ref" = "ref"
bodyNoPrefix "pipeline_object_attributes_tag" = "tag"
bodyNoPrefix "pipeline_object_attributes_sha" = "sha"
bodyNoPrefix "pipeline_object_attributes_before_sha" = "before_sha"
bodyNoPrefix "pipeline_object_attributes_source" = "source"
bodyNoPrefix "pipeline_object_attributes_status" = "status"
bodyNoPrefix "pipeline_object_attributes_detailed_status" = "detailed_status"
bodyNoPrefix "pipeline_object_attributes_stages" = "stages"
bodyNoPrefix "pipeline_object_attributes_created_at" = "created_at"
bodyNoPrefix "pipeline_object_attributes_finished_at" = "finished_at"
bodyNoPrefix "pipeline_object_attributes_duration" = "duration"
bodyNoPrefix "pipeline_object_attributes_queued_duration" = "queued_duration"
bodyNoPrefix "pipeline_object_attributes_variables" = "variables"
bodyNoPrefix "pipeline_object_attributes_url" = "url"
bodyNoPrefix "pipeline_build_id" = "id"
bodyNoPrefix "pipeline_build_stage" = "stage"
bodyNoPrefix "pipeline_build_name" = "name"
bodyNoPrefix "pipeline_build_status" = "status"
bodyNoPrefix "pipeline_build_created_at" = "created_at"
bodyNoPrefix "pipeline_build_started_at" = "started_at"
bodyNoPrefix "pipeline_build_finished_at" = "finished_at"
bodyNoPrefix "pipeline_build_duration" = "duration"
bodyNoPrefix "pipeline_build_queued_duration" = "queued_duration"
bodyNoPrefix "pipeline_build_failure_reason" = "failure_reason"
bodyNoPrefix "pipeline_build_when" = "when"
bodyNoPrefix "pipeline_build_manual" = "manual"
bodyNoPrefix "pipeline_build_allow_failure" = "allow_failure"
bodyNoPrefix "pipeline_build_user" = "user"
bodyNoPrefix "pipeline_build_runner" = "runner"
bodyNoPrefix "pipeline_build_artifacts_file" = "artifacts_file"
bodyNoPrefix "pipeline_build_environment" = "environment"
bodyNoPrefix "runner_id" = "id"
bodyNoPrefix "runner_description" = "description"
bodyNoPrefix "runner_runner_type" = "runner_type"
bodyNoPrefix "runner_active" = "active"
bodyNoPrefix "runner_is_shared" = "is_shared"
bodyNoPrefix "runner_tags" = "tags"
bodyNoPrefix "artifacts_file_filename" = "filename"
bodyNoPrefix "artifacts_file_size" = "size"
bodyNoPrefix "issue_event_event_type" = "event_type"
bodyNoPrefix "issue_event_user" = "user"
bodyNoPrefix "issue_event_project" = "project"
bodyNoPrefix "issue_event_object_attributes" = "object_attributes"
bodyNoPrefix "issue_event_labels" = "labels"
bodyNoPrefix "issue_event_changes" = "changes"
bodyNoPrefix "issue_event_repository" = "repository"
bodyNoPrefix "issue_event_assignees" = "assignees"
bodyNoPrefix "issue_event_object_attributes_author_id" = "author_id"
bodyNoPrefix "issue_event_object_attributes_closed_at" = "closed_at"
bodyNoPrefix "issue_event_object_attributes_confidential" = "confidential"
bodyNoPrefix "issue_event_object_attributes_created_at" = "created_at"
bodyNoPrefix "issue_event_object_attributes_description" = "description"
bodyNoPrefix "issue_event_object_attributes_discussion_locked" = "discussion_locked"
bodyNoPrefix "issue_event_object_attributes_due_date" = "due_date"
bodyNoPrefix "issue_event_object_attributes_id" = "id"
bodyNoPrefix "issue_event_object_attributes_iid" = "iid"
bodyNoPrefix "issue_event_object_attributes_last_edited_at" = "last_edited_at"
bodyNoPrefix "issue_event_object_attributes_last_edited_by" = "last_edited_by"
bodyNoPrefix "issue_event_object_attributes_milestone_id" = "milestone_id"
bodyNoPrefix "issue_event_object_attributes_move_to_id" = "move_to_id"
bodyNoPrefix "issue_event_object_attributes_duplicated_to_id" = "duplicated_to_id"
bodyNoPrefix "issue_event_object_attributes_project_id" = "project_id"
bodyNoPrefix "issue_event_object_attributes_relative_position" = "relative_position"
bodyNoPrefix "issue_event_object_attributes_state_id" = "state_id"
bodyNoPrefix "issue_event_object_attributes_time_estimate" = "time_estimate"
bodyNoPrefix "issue_event_object_attributes_title" = "title"
bodyNoPrefix "issue_event_object_attributes_updated_at" = "updated_at"
bodyNoPrefix "issue_event_object_attributes_updated_by_id" = "updated_by_id"
bodyNoPrefix "issue_event_object_attributes_type" = "type"
bodyNoPrefix "issue_event_object_attributes_url" = "url"
bodyNoPrefix "issue_event_object_attributes_total_time_spent" = "total_time_spent"
bodyNoPrefix "issue_event_object_attributes_time_change" = "time_change"
bodyNoPrefix "issue_event_object_attributes_human_total_time_spent" = "human_total_time_spent"
bodyNoPrefix "issue_event_object_attributes_human_time_change" = "human_time_change"
bodyNoPrefix "issue_event_object_attributes_human_time_estimate" = "human_time_estimate"
bodyNoPrefix "issue_event_changes_author_id" = "author_id"
bodyNoPrefix "issue_event_author_id_previous" = "previous"
bodyNoPrefix "issue_event_author_id_current" = "current"
bodyNoPrefix "issue_event_changes_created_at" = "created_at"
bodyNoPrefix "issue_event_created_at_previous" = "previous"
bodyNoPrefix "issue_event_created_at_current" = "current"
bodyNoPrefix "issue_event_changes_description" = "description"
bodyNoPrefix "issue_event_description_previous" = "previous"
bodyNoPrefix "issue_event_description_current" = "current"
bodyNoPrefix "issue_event_changes_id" = "id"
bodyNoPrefix "issue_event_id_previous" = "previous"
bodyNoPrefix "issue_event_id_current" = "current"
bodyNoPrefix "issue_event_changes_iid" = "iid"
bodyNoPrefix "issue_event_iid_previous" = "previous"
bodyNoPrefix "issue_event_iid_current" = "current"
bodyNoPrefix "issue_event_changes_project_id" = "project_id"
bodyNoPrefix "issue_event_project_id_previous" = "previous"
bodyNoPrefix "issue_event_project_id_current" = "current"
bodyNoPrefix "issue_event_changes_title" = "title"
bodyNoPrefix "issue_event_title_previous" = "previous"
bodyNoPrefix "issue_event_title_current" = "current"
bodyNoPrefix "issue_event_changes_updated_at" = "updated_at"
bodyNoPrefix "issue_event_updated_at_previous" = "previous"
bodyNoPrefix "issue_event_updated_at_current" = "current"
bodyNoPrefix "issue_event_changes_closed_at" = "closed_at"
bodyNoPrefix "issue_event_closed_at_previous" = "previous"
bodyNoPrefix "issue_event_closed_at_current" = "current"
bodyNoPrefix "issue_event_changes_state_id" = "state_id"
bodyNoPrefix "issue_event_state_id_previous" = "previous"
bodyNoPrefix "issue_event_state_id_current" = "current"
bodyNoPrefix "issue_event_changes_updated_at" = "updated_at"
bodyNoPrefix "issue_event_updated_at_previous" = "previous"
bodyNoPrefix "issue_event_updated_at_current" = "current"
bodyNoPrefix "note_object_attributes_attachment" = "attachment"
bodyNoPrefix "note_object_attributes_author_id" = "author_id"
bodyNoPrefix "note_object_attributes_change_position" = "change_position"
bodyNoPrefix "note_object_attributes_commit_id" = "commit_id"
bodyNoPrefix "note_object_attributes_created_at" = "created_at"
bodyNoPrefix "note_object_attributes_discussion_id" = "discussion_id"
bodyNoPrefix "note_object_attributes_id" = "id"
bodyNoPrefix "note_object_attributes_line_code" = "line_code"
bodyNoPrefix "note_object_attributes_note" = "note"
bodyNoPrefix "note_object_attributes_noteable_id" = "noteable_id"
bodyNoPrefix "note_object_attributes_noteable_type" = "noteable_type"
bodyNoPrefix "note_object_attributes_original_position" = "original_position"
bodyNoPrefix "note_object_attributes_position" = "position"
bodyNoPrefix "note_object_attributes_project_id" = "project_id"
bodyNoPrefix "note_object_attributes_resolved_at" = "resolved_at"
bodyNoPrefix "note_object_attributes_resolved_by_id" = "resolved_by_id"
bodyNoPrefix "note_object_attributes_resolved_by_push" = "resolved_by_push"
bodyNoPrefix "note_object_attributes_st_diff" = "st_diff"
bodyNoPrefix "note_object_attributes_system" = "system"
bodyNoPrefix "note_object_attributes_type" = "type"
bodyNoPrefix "note_object_attributes_updated_at" = "updated_at"
bodyNoPrefix "note_object_attributes_updated_by_id" = "updated_by_id"
bodyNoPrefix "note_object_attributes_description" = "description"
bodyNoPrefix "note_object_attributes_url" = "url"
bodyNoPrefix "note_object_attributes_action" = "action"
bodyNoPrefix "wiki_web_url" = "web_url"
bodyNoPrefix "wiki_git_ssh_url" = "git_ssh_url"
bodyNoPrefix "wiki_git_http_url" = "git_http_url"
bodyNoPrefix "wiki_path_with_namespace" = "path_with_namespace"
bodyNoPrefix "wiki_default_branch" = "default_branch"
bodyNoPrefix "wiki_page_object_attributes_slug" = "slug"
bodyNoPrefix "wiki_page_object_attributes_title" = "title"
bodyNoPrefix "wiki_page_object_attributes_format" = "format"
bodyNoPrefix "wiki_page_object_attributes_message" = "message"
bodyNoPrefix "wiki_page_object_attributes_version_id" = "version_id"
bodyNoPrefix "wiki_page_object_attributes_url" = "url"
bodyNoPrefix "wiki_page_object_attributes_action" = "action"
bodyNoPrefix "wiki_page_object_attributes_diff_url" = "diff_url"
bodyNoPrefix "work_item_object_author_id" = "author_id"
bodyNoPrefix "work_item_object_closed_at" = "closed_at"
bodyNoPrefix "work_item_object_confidential" = "confidential"
bodyNoPrefix "work_item_object_created_at" = "created_at"
bodyNoPrefix "work_item_object_description" = "description"
bodyNoPrefix "work_item_object_discussion_locked" = "discussion_locked"
bodyNoPrefix "work_item_object_due_date" = "due_date"
bodyNoPrefix "work_item_object_id" = "id"
bodyNoPrefix "work_item_object_iid" = "iid"
bodyNoPrefix "work_item_object_last_edited_at" = "last_edited_at"
bodyNoPrefix "work_item_object_last_edited_at_by_id" = "edited_at_by_id"
bodyNoPrefix "work_item_object_milestone_id" = "milestone_id"
bodyNoPrefix "work_item_object_moved_to_id" = "moved_to_id"
bodyNoPrefix "work_item_object_duplicated_to_id" = "duplicated_to_id"
bodyNoPrefix "work_item_object_project_id" = "project_id"
bodyNoPrefix "work_item_object_relative_position" = "relative_position"
bodyNoPrefix "work_item_object_state_id" = "state_id"
bodyNoPrefix "work_item_object_time_estimate" = "time_estimate"
bodyNoPrefix "work_item_object_title" = "title"
bodyNoPrefix "work_item_object_updated_at" = "updated_at"
bodyNoPrefix "work_item_object_updated_by_id" = "updated_by_id"
bodyNoPrefix "work_item_object_type" = "type"
bodyNoPrefix "work_item_object_url" = "url"
bodyNoPrefix "work_item_object_total_time_spent" = "total_time_spent"
bodyNoPrefix "work_item_object_time_change" = "time_change"
bodyNoPrefix "work_item_object_human_total_time_spent" = "human_total_time_spent"
bodyNoPrefix "work_item_object_human_time_change" = "human_time_change"
bodyNoPrefix "work_item_object_human_time_estimate" = "human_time_estimate"
bodyNoPrefix "work_item_object_assignee_ids" = "assignee_ids"
bodyNoPrefix "work_item_object_assignee_id" = "assignee_id"
bodyNoPrefix "work_item_object_labels" = "labels"
bodyNoPrefix "work_item_object_state" = "state"
bodyNoPrefix "work_item_object_severity" = "severity"
bodyNoPrefix "work_item_object_customer_relations_contacts" = "customer_relations_contacts"
bodyNoPrefix "work_item_object_action" = "action"
bodyNoPrefix s = fail ("uexpected JSON field prefix: " <> s)

instance FromJSON ProjectEvent where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON ProjectChanges where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON CommitEvent where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON RepositoryEvent where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON CommitAuthorEvent where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON MergeRequestObjectAttributes where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON MergeParams where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON UserEvent where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON MergeRequestChanges where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON BuildCommit where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON BuildProject where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance (FromJSON a) => FromJSON (MergeRequestChange a) where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON PipelineObjectAttributes where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON PipelineBuild where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON Runner where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON ArtifactsFile where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON IssueEvent where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON IssueEventObjectAttributes where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON IssueEventChanges where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON IssueChangesAuthorId where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON IssueChangesCreatedAt where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON IssueChangesDescription where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON IssueChangesId where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON IssueChangesIid where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON IssueChangesProjectId where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON IssueChangesTitle where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON IssueChangesClosedAt where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON IssueChangesStateId where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON IssueChangesUpdatedAt where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON NoteObjectAttributes where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON Wiki where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON WikiPageObjectAttributes where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON WorkItemObjectAttributes where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON Label where
  parseJSON = withObject "Label" $ \obj -> do
    labelId <- obj .:? "id"
    labelTitle <- obj .:? "title"
    labelColor <- obj .:? "color"
    labelProjectId <- obj .:? "project_id"
    labelCreatedAt <- obj .:? "created_at"
    labelUpdatedAt <- obj .:? "updated_at"
    labelTemplate <- obj .:? "template"
    labelDescription <- obj .:? "description"
    labelType <- obj .:? "type"
    labelGroupId <- obj .:? "group_id"
    return $
      Label
        { label_id = labelId,
          label_title = labelTitle,
          label_color = labelColor,
          label_project_id = labelProjectId,
          label_created_at = labelCreatedAt,
          label_updated_at = labelUpdatedAt,
          label_template = labelTemplate,
          label_description = labelDescription,
          label_type = labelType,
          label_group_id = labelGroupId
        }

instance FromJSON ProjectAction where
  parseJSON (String "project_create") = return ProjectCreated
  parseJSON (String "project_destroy") = return ProjectDestroyed
  parseJSON (String "project_rename") = return ProjectRenamed
  parseJSON (String "project_transfer") = return ProjectTransferred
  parseJSON (String "project_update") = return ProjectUpdated
  parseJSON (String "user_add_to_team") = return UserAddedToTeam
  parseJSON (String "user_update_for_team") = return UserUpdatedForTeam
  parseJSON (String "user_remove_from_team") = return UserRemovedFromTeam
  parseJSON (String "user_create") = return UserCreated
  parseJSON (String "user_destroy") = return UserRemoved
  parseJSON (String "user_failed_login") = return UserFailedToLogin
  parseJSON (String "user_rename") = return UserRenamed
  parseJSON (String "key_create") = return KeyCreated
  parseJSON (String "key_destroy") = return KeyRemoved
  parseJSON (String "group_create") = return GroupCreated
  parseJSON (String "group_destroy") = return GroupRemoved
  parseJSON (String "group_rename") = return GroupRenamed
  parseJSON (String "user_add_to_group") = return GroupMemberAdded
  parseJSON (String "user_remove_from_group") = return GroupMemberRemoved
  parseJSON (String "user_update_for_group") = return GroupMemberUpdated
  parseJSON (String "push") = return Pushed
  parseJSON (String "tag_push") = return TagPushed
  parseJSON (String "repository_update") = return RepositoryUpdated
  parseJSON (String "merge_request") = return MergeRequested
  parseJSON (String "build") = return Built
  parseJSON (String "pipeline") = return Pipelined
  parseJSON (String "note") = return Noted
  parseJSON (String "wiki_page") = return WikiPaged
  parseJSON (String "work_item") = return WorkItemed
  parseJSON s = fail ("unexpected system hook event: " <> show s)
