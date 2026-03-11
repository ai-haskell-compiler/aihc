{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Projects
-- Description : Queries about projects
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Projects
  ( -- * List all projects
    projects,

    -- * Get single project
    project,

    -- * Get project users
    projectUsers,

    -- * User projects
    userProjects,

    -- * starredProjects
    starredProjects,

    -- * project groups
    projectGroups,

    -- * create project
    createProject,
    createProjectForUser,

    -- * edit project
    editProject,

    -- * fork project
    forkProject,

    -- * forks of project
    projectForks,

    -- * starring projects
    starProject,
    unstarProject,
    projectStarrers,

    -- * archving projects
    archiveProject,
    unarchiveProject,

    -- * delete project
    deleteProject,

    -- * share projects with groups
    shareProjectWithGroup,
    unshareProjectWithGroup,

    -- * impport project members
    importMembersFromProject,

    -- * fork relationship
    forkRelation,
    unforkRelation,

    -- * Search for projects
    projectsWithName,
    projectWithPathAndName,

    -- * housekeeping
    houseKeeping,

    -- * Transfer projects
    transferProject,

    -- * Additional functionality beyond the GitLab Projects API
    multipleCommitters,
    commitsEmailAddresses,
    projectOfIssue,
    -- issuesCreatedByUser,
    -- issuesOnForks,
    -- projectMemebersCount,
    projectCISuccess,
    -- namespacePathToUserId,
    projectDiffs,
    addGroupToProject,
    -- transferProject,
    -- transferProject',
    defaultProjectAttrs,
    defaultProjectSearchAttrs,
    ProjectAttrs (..),
    ProjectSearchAttrs (..),
    EnabledDisabled (..),
    AutoDeployStrategy (..),
    GitStrategy (..),
    ProjectSettingAccessLevel (..),
    MergeMethod (..),
    SquashOption (..),
  )
where

import qualified Data.ByteString.Lazy as BSL
import Data.Either
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import GHC.Generics
import GitLab.API.Commits
import GitLab.API.Members
import GitLab.API.Pipelines
import GitLab.API.Users
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client
import Network.HTTP.Types.URI

-- | Get a list of all visible projects across GitLab for the
-- authenticated user. When accessed without authentication, only
-- public projects with simple fields are returned.
projects ::
  -- | project filters
  ProjectSearchAttrs ->
  GitLab [Project]
projects attrs =
  fromRight (error "projects error")
    <$> gitlabGetMany
      "/projects"
      (projectSearchAttrsParams attrs)

-- | Get a specific project. This endpoint can be accessed without
-- authentication if the project is publicly accessible.
project ::
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Project))
project pId = do
  gitlabGetOne urlPath []
  where
    urlPath =
      "/projects/"
        <> T.pack (show pId)

-- | Get the users list of a project.
projectUsers ::
  Project ->
  GitLab (Either (Response BSL.ByteString) [User])
projectUsers prj = do
  projectUsers' (project_id prj)

-- | Get the users list of a project.
projectUsers' ::
  Int ->
  GitLab (Either (Response BSL.ByteString) [User])
projectUsers' pId = do
  gitlabGetMany urlPath []
  where
    urlPath =
      "/projects/"
        <> T.pack (show pId)
        <> "/users"

-- | gets all projects for a user given their username.
--
-- > userProjects "harry"
userProjects' :: Text -> ProjectSearchAttrs -> GitLab (Maybe [Project])
userProjects' username attrs = do
  userMaybe <- searchUser username
  case userMaybe of
    Nothing -> return Nothing
    Just usr -> do
      result <-
        gitlabGetMany
          (urlPath (user_id usr))
          (projectSearchAttrsParams attrs)
      case result of
        Left _ -> error "userProjects' error"
        Right projs -> return (Just projs)
  where
    urlPath usrId = "/users/" <> T.pack (show usrId) <> "/projects"

-- | gets all projects for a user.
--
-- > userProjects myUser
userProjects :: User -> ProjectSearchAttrs -> GitLab (Maybe [Project])
userProjects theUser =
  userProjects' (user_username theUser)

-- | Get a list of visible projects starred by the given user. When
-- accessed without authentication, only public projects are returned.
--
-- > userProjects myUser
starredProjects :: User -> ProjectSearchAttrs -> GitLab [Project]
starredProjects usr attrs = do
  fromRight (error "starredProjects error")
    <$> gitlabGetMany
      ( "/users/"
          <> T.pack (show (user_id usr))
          <> "/starred_projects"
      )
      (projectSearchAttrsParams attrs)

-- | Get a list of ancestor groups for this project.
projectGroups ::
  Project ->
  GitLab (Either (Response BSL.ByteString) [Group])
projectGroups prj = do
  projectGroups' (project_id prj)

-- | Get a list of ancestor groups for this project.
projectGroups' ::
  Int ->
  GitLab (Either (Response BSL.ByteString) [Group])
projectGroups' gId = do
  gitlabGetMany urlPath []
  where
    urlPath =
      "/projects/"
        <> T.pack (show gId)
        <> "/groups"

-- | Creates a new project owned by the authenticated user.
createProject ::
  Text ->
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Project))
createProject nameTxt pathTxt = do
  gitlabPost newProjectAddr [("name", Just (T.encodeUtf8 nameTxt)), ("path", Just (T.encodeUtf8 pathTxt))]
  where
    newProjectAddr :: Text
    newProjectAddr =
      "/projects"

-- | Creates a new project owned by the specified user. Available only
-- for administrators.
createProjectForUser ::
  -- | user to create the project for
  User ->
  -- | name of the new project
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Project))
createProjectForUser usrId nameTxt = do
  gitlabPost newProjectAddr [("name", Just (T.encodeUtf8 nameTxt))]
  where
    newProjectAddr :: Text
    newProjectAddr =
      "/projects"
        <> "/user/"
        <> T.pack (show usrId)

-- | Edit a project. The 'defaultProjectAttrs' value has default project
-- search values, which is a record that can be modified with 'Just'
-- values.
--
-- For example to disable project specific email notifications:
--
-- > editProject myProject (defaultProjectAttrs { project_edit_emails_disabled = Just True })
editProject ::
  -- | project
  Project ->
  -- | project attributes
  ProjectAttrs ->
  GitLab (Either (Response BSL.ByteString) Project)
editProject prj = editProject' (project_id prj)

-- | Edit a project. The 'defaultProjectAttrs' value has default project
-- search values, which is a record that can be modified with 'Just'
-- values.
--
-- For example to disable project specific email notifications for a
-- project with project ID 11744514:
--
-- > editProject' 11744514 (defaultProjectAttrs { project_edit_emails_disabled = Just True })
editProject' ::
  -- | project ID
  Int ->
  -- | project attributes
  ProjectAttrs ->
  GitLab (Either (Response BSL.ByteString) Project)
editProject' projId attrs = do
  let urlPath =
        "/projects/"
          <> T.pack (show projId)
  result <-
    gitlabPut
      urlPath
      (projectAttrsParams attrs)
  case result of
    Left resp -> return (Left resp)
    Right Nothing -> error "editProject error"
    Right (Just proj) -> return (Right proj)

-- | Forks a project into the user namespace of the authenticated user
-- or the one provided.
forkProject ::
  -- project to fork
  Project ->
  GitLab (Either (Response BSL.ByteString) (Maybe Project))
forkProject prj =
  gitlabPost addr params
  where
    params :: [GitLabParam]
    params = []
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/fork"

-- | List the projects accessible to the calling user that have an
-- established, forked relationship with the specified project
--
-- > projectForks "project1"
-- > projectForks "group1/project1"
projectForks ::
  -- | name or namespace of the project
  Text ->
  GitLab (Either (Response BSL.ByteString) [Project])
projectForks projectName = do
  let urlPath =
        "/projects/"
          <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 projectName))
          <> "/forks"
  gitlabGetMany urlPath []

-- | Stars a given project.
starProject ::
  -- project to star
  Project ->
  GitLab (Either (Response BSL.ByteString) (Maybe Project))
starProject prj =
  gitlabPost addr params
  where
    params :: [GitLabParam]
    params = []
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/star"

-- | Stars a given project.
unstarProject ::
  -- project to star
  Project ->
  GitLab (Either (Response BSL.ByteString) (Maybe Project))
unstarProject prj =
  gitlabPost addr params
  where
    params :: [GitLabParam]
    params = []
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/unstar"

-- | List the users who starred the specified project.
projectStarrers ::
  Project ->
  GitLab (Either (Response BSL.ByteString) [Group])
projectStarrers prj = do
  gitlabGetMany urlPath []
  where
    urlPath =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/starrers"

-- | Archives the project if the user is either an administrator or
-- the owner of this project.
archiveProject ::
  -- project to archive
  Project ->
  GitLab (Either (Response BSL.ByteString) (Maybe Project))
archiveProject prj =
  gitlabPost addr params
  where
    params :: [GitLabParam]
    params = []
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/archive"

-- | Unarchives the project if the user is either an administrator or
-- the owner of this project.
unarchiveProject ::
  -- project to unarchive
  Project ->
  GitLab (Either (Response BSL.ByteString) (Maybe Project))
unarchiveProject prj =
  gitlabPost addr params
  where
    params :: [GitLabParam]
    params = []
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/unarchive"

-- | Deletes a project including all associated resources.
deleteProject ::
  Project ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteProject prj = do
  gitlabDelete projAddr []
  where
    projAddr :: Text
    projAddr =
      "/projects/"
        <> T.pack (show (project_id prj))

-- | Allow to share project with group.
shareProjectWithGroup ::
  -- | group ID
  Int ->
  -- | project
  Project ->
  -- | level of access granted
  AccessLevel ->
  GitLab (Either (Response BSL.ByteString) (Maybe Project))
shareProjectWithGroup groupId prj access =
  gitlabPost addr params
  where
    params :: [GitLabParam]
    params =
      [ ("group_id", Just (T.encodeUtf8 (T.pack (show groupId)))),
        ("group_access", Just (T.encodeUtf8 (T.pack (show access))))
      ]
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/share"

-- | Unshare the project from the group.
unshareProjectWithGroup ::
  -- | group ID
  Int ->
  -- | project
  Project ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
unshareProjectWithGroup groupId prj =
  gitlabDelete addr []
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/share/"
        <> T.pack (show groupId)

-- | Import members from another project.
importMembersFromProject ::
  -- | project to receive memvers
  Project ->
  -- | source project to import members from
  Project ->
  GitLab
    (Either (Response BSL.ByteString) (Maybe Project))
importMembersFromProject toPrj fromPrj =
  gitlabPost addr []
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id toPrj))
        <> "/import_project_members/"
        <> T.pack (show (project_id fromPrj))

-- | Allows modification of the forked relationship between existing
-- projects. Available only for project owners and administrators.
forkRelation ::
  -- | forked project
  Project ->
  -- | project that was forked from
  Project ->
  GitLab
    (Either (Response BSL.ByteString) (Maybe Project))
forkRelation toPrj fromPrj =
  gitlabPost addr []
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id toPrj))
        <> "/fork/"
        <> T.pack (show (project_id fromPrj))

-- | Delete an existing forked from relationship.
unforkRelation ::
  -- | forked project
  Project ->
  GitLab
    (Either (Response BSL.ByteString) (Maybe ()))
unforkRelation prj =
  gitlabDelete addr []
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/fork"

-- | gets all projects with the given project name. It only returns
-- projects with an exact match with the project path.
--
-- > projectsWithName "project1"
projectsWithName ::
  -- | project name being searched for.
  Text ->
  GitLab [Project]
projectsWithName projectName = do
  foundProjects <-
    fromRight (error "projectsWithName error")
      <$> gitlabGetMany
        "/projects"
        [("search", Just (T.encodeUtf8 projectName))]
  return $
    filter (\prj -> projectName == project_path prj) foundProjects

-- | gets a project with the given name for the given full path of the
--   namespace. E.g.
--
-- > projectWithPathAndName "user1" "project1"
--
-- looks for "user1/project1"
--
-- > projectWithPathAndName "group1/subgroup1" "project1"
--
-- looks for "project1" within the namespace with full path "group1/subgroup1"
projectWithPathAndName :: Text -> Text -> GitLab (Either (Response BSL.ByteString) (Maybe Project))
projectWithPathAndName namespaceFullPath projectName = do
  gitlabGetOne
    ( "/projects/"
        <> T.decodeUtf8
          ( urlEncode
              False
              ( T.encodeUtf8
                  (namespaceFullPath <> "/" <> projectName)
              )
          )
    )
    [("statistics", Just "true")]

-- | Start the Housekeeping task for a project.
houseKeeping ::
  -- | the project
  Project ->
  GitLab (Either (Response BSL.ByteString) (Maybe Project))
houseKeeping prj =
  gitlabPost addr []
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/housekeeping"

-- | transfer a project to a new namespace.
transferProject ::
  -- | project
  Project ->
  -- | namespace where to transfer project to
  Text ->
  GitLab (Either (Response BSL.ByteString) Project)
transferProject prj = transferProject' (project_id prj)

-- | edit a project.
transferProject' ::
  -- | project ID
  Int ->
  -- | namespace where to transfer project to
  Text ->
  GitLab (Either (Response BSL.ByteString) Project)
transferProject' projId namespaceString = do
  let urlPath =
        "/projects/"
          <> T.pack (show projId)
          <> "/transfer"
  result <-
    gitlabPut
      urlPath
      [ ("id", Just (T.encodeUtf8 (T.pack (show projId)))),
        ("namespace", Just (T.encodeUtf8 namespaceString))
      ]
  case result of
    Left resp -> return (Left resp)
    Right Nothing -> error "transferProject error"
    Right (Just proj) -> return (Right proj)

--------------------
-- Additional functionality beyond the GitLab Projects API

-- | Returns 'True' is a project has multiple email addresses
-- associated with all commits in a project, 'False' otherwise.
multipleCommitters :: Project -> GitLab Bool
multipleCommitters prj = do
  emailAddresses <- commitsEmailAddresses prj
  return $
    case nub emailAddresses of
      [] -> False
      [_] -> False
      (_ : _) -> True

-- | gets the email addresses in the author information in all commit
-- for a project.
commitsEmailAddresses :: Project -> GitLab [Text]
commitsEmailAddresses prj = do
  commits <- repoCommits prj
  return (map commit_author_email commits)

-- | gets the 'GitLab.Types.Project' against which the given 'Issue'
-- was created.
projectOfIssue :: Issue -> GitLab Project
projectOfIssue iss = do
  let prId = fromMaybe (error "projectOfIssue error") (issue_project_id iss)
  result <- project prId
  case fromRight (error "projectOfIssue error") result of
    Nothing -> error "projectOfIssue error"
    Just proj -> return proj

-- | gets all diffs in a project for a given commit SHA.
projectDiffs :: Project -> Text -> GitLab (Either (Response BSL.ByteString) [Diff])
projectDiffs proj =
  projectDiffs' (project_id proj)

-- | gets all diffs in a project for a given project ID, for a given
-- commit SHA.
projectDiffs' :: Int -> Text -> GitLab (Either (Response BSL.ByteString) [Diff])
projectDiffs' projId commitSha =
  gitlabGetMany
    ( "/projects/"
        <> T.pack (show projId)
        <> "/repository/commits/"
        <> commitSha
        <> "/diff/"
    )
    []

-- | add a group to a project.
addGroupToProject ::
  -- | group ID
  Int ->
  -- | project ID
  Int ->
  -- | level of access granted
  AccessLevel ->
  GitLab (Either (Response BSL.ByteString) (Maybe GroupShare))
addGroupToProject groupId projectId access =
  gitlabPost addr params
  where
    params :: [GitLabParam]
    params =
      [ ("group_id", Just (T.encodeUtf8 (T.pack (show groupId)))),
        ("group_access", Just (T.encodeUtf8 (T.pack (show access))))
      ]
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/share"

-- | A default set of project attributes to override with the
-- 'editProject' functions. Only the project ID value is set is a
-- search parameter, all other search parameters are not set and can
-- be overwritten.
defaultProjectAttrs ::
  -- | project ID
  Int ->
  ProjectAttrs
defaultProjectAttrs projId =
  ProjectAttrs Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing projId Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

projectAttrsParams :: ProjectAttrs -> [GitLabParam]
projectAttrsParams filters =
  catMaybes
    [ (\b -> Just ("allow_merge_on_skipped_pipeline", textToBS (showBool b))) =<< project_edit_allow_merge_on_skipped_pipeline filters,
      (\x -> Just ("analytics_access_level", textToBS (T.pack (show x)))) =<< project_edit_analytics_access_level filters,
      (\i -> Just ("approvals_before_merge", textToBS (T.pack (show i)))) =<< project_edit_approvals_before_merge filters,
      (\x -> Just ("auto_cancel_pending_pipelines", textToBS (T.pack (show x))))
        =<< project_edit_auto_cancel_pending_pipelines
          filters,
      (\x -> Just ("auto_devops_deploy_strategy", textToBS (T.pack (show x))))
        =<< project_edit_auto_devops_deploy_strategy filters,
      (\b -> Just ("auto_devops_enabled", textToBS (showBool b))) =<< project_edit_auto_devops_enabled filters,
      (\b -> Just ("autoclose_referenced_issues", textToBS (showBool b))) =<< project_edit_autoclose_referenced_issues filters,
      (\t -> Just ("build_coverage_regex", textToBS t)) =<< project_edit_build_coverage_regex filters,
      (\x -> Just ("build_git_strategy", textToBS (T.pack (show x)))) =<< project_edit_build_git_strategy filters,
      (\i -> Just ("build_timeout", textToBS (T.pack (show i)))) =<< project_edit_build_timeout filters,
      (\x -> Just ("builds_access_level", textToBS (T.pack (show x)))) =<< project_edit_builds_access_level filters,
      (\t -> Just ("ci_config_path", textToBS t)) =<< project_edit_ci_config_path filters,
      (\i -> Just ("ci_default_git_depth", textToBS (T.pack (show i)))) =<< project_edit_ci_default_git_depth filters,
      (\b -> Just ("ci_forward_deployment_enabled", textToBS (showBool b))) =<< project_edit_ci_forward_deployment_enabled filters,
      (\x -> Just ("container_registry_access_level", textToBS (T.pack (show x)))) =<< project_edit_container_registry_access_level filters,
      (\t -> Just ("default_branch", textToBS t)) =<< project_edit_default_branch filters,
      (\t -> Just ("description", textToBS t)) =<< project_edit_description filters,
      (\b -> Just ("emails_disabled", textToBS (showBool b))) =<< project_edit_emails_disabled filters,
      (\t -> Just ("external_authorization_classification_label", textToBS t)) =<< project_edit_external_authorization_classification_label filters,
      (\x -> Just ("forking_access_level", textToBS (T.pack (show x)))) =<< project_edit_forking_access_level filters,
      Just ("id", textToBS (T.pack (show (project_edit_id filters)))),
      (\t -> Just ("import_url", textToBS t)) =<< project_edit_import_url filters,
      (\x -> Just ("issues_access_level", textToBS (T.pack (show x)))) =<< project_edit_issues_access_level filters,
      (\b -> Just ("lfs_enabled", textToBS (showBool b))) =<< project_edit_lfs_enabled filters,
      (\x -> Just ("merge_method", textToBS (T.pack (show x)))) =<< project_edit_merge_method filters,
      (\x -> Just ("merge_requests_access_level", textToBS (T.pack (show x)))) =<< project_edit_merge_requests_access_level filters,
      (\b -> Just ("mirror_overwrites_diverged_branches", textToBS (showBool b))) =<< project_edit_mirror_overwrites_diverged_branches filters,
      (\b -> Just ("mirror_trigger_builds", textToBS (showBool b))) =<< project_edit_mirror_trigger_builds filters,
      (\i -> Just ("mirror_user_id", textToBS (T.pack (show i)))) =<< project_edit_mirror_user_id filters,
      (\b -> Just ("mirror", textToBS (showBool b))) =<< project_edit_mirror filters,
      (\t -> Just ("name", textToBS t)) =<< project_edit_name filters,
      (\x -> Just ("operations_access_level", textToBS (T.pack (show x)))) =<< project_edit_operations_access_level filters,
      (\b -> Just ("only_allow_merge_if_all_discussions_are_resolved", textToBS (showBool b))) =<< project_edit_only_allow_merge_if_all_discussions_are_resolved filters,
      (\b -> Just ("only_allow_merge_if_pipeline_succeeds", textToBS (showBool b))) =<< project_edit_only_allow_merge_if_pipeline_succeeds filters,
      (\b -> Just ("only_mirror_protected_branches", textToBS (showBool b))) =<< project_edit_only_mirror_protected_branches filters,
      (\b -> Just ("packages_enabled", textToBS (showBool b))) =<< project_edit_packages_enabled filters,
      (\x -> Just ("pages_access_level", textToBS (T.pack (show x)))) =<< project_edit_pages_access_level filters,
      (\x -> Just ("requirements_access_level", textToBS (T.pack (show x)))) =<< project_edit_requirements_access_level filters,
      (\b -> Just ("restrict_user_defined_variables", textToBS (showBool b))) =<< project_edit_restrict_user_defined_variables filters,
      (\t -> Just ("path", textToBS t)) =<< project_edit_path filters,
      (\b -> Just ("public_builds", textToBS (showBool b))) =<< project_edit_public_builds filters,
      (\b -> Just ("remove_source_branch_after_merge", textToBS (showBool b))) =<< project_edit_remove_source_branch_after_merge filters,
      (\x -> Just ("repository_access_level", textToBS (T.pack (show x)))) =<< project_edit_repository_access_level filters,
      (\t -> Just ("repository_storage", textToBS t)) =<< project_edit_repository_storage filters,
      (\b -> Just ("request_access_enabled", textToBS (showBool b))) =<< project_edit_request_access_enabled filters,
      (\b -> Just ("resolve_outdated_diff_discussions", textToBS (showBool b))) =<< project_edit_resolve_outdated_diff_discussions filters,
      (\b -> Just ("service_desk_enabled", textToBS (showBool b))) =<< project_edit_service_desk_enabled filters,
      (\b -> Just ("shared_runners_enabled", textToBS (showBool b))) =<< project_edit_shared_runners_enabled filters,
      (\b -> Just ("show_default_award_emojis", textToBS (showBool b))) =<< project_edit_show_default_award_emojis filters,
      (\x -> Just ("snippets_access_level", textToBS (T.pack (show x)))) =<< project_edit_snippets_access_level filters,
      (\x -> Just ("squash_option", textToBS (T.pack (show x)))) =<< project_edit_squash_option filters,
      (\t -> Just ("suggestion_commit_message", textToBS t)) =<< project_edit_suggestion_commit_message filters,
      (\x -> Just ("visibility", textToBS (T.pack (show x)))) =<< project_edit_visibility filters,
      (\x -> Just ("wiki_access_level", textToBS (T.pack (show x)))) =<< project_edit_wiki_access_level filters,
      (\t -> Just ("issues_template", textToBS t)) =<< project_edit_issues_template filters,
      (\t -> Just ("merge_requests_template", textToBS t)) =<< project_edit_merge_requests_template filters,
      (\b -> Just ("keep_latest_artifact", textToBS (showBool b))) =<< project_edit_keep_latest_artifact filters
    ]
  where
    textToBS = Just . T.encodeUtf8
    showBool :: Bool -> Text
    showBool True = "true"
    showBool False = "false"

-- | Attributes for updating when editing a project with the
-- 'editProject' functions.
data ProjectAttrs = ProjectAttrs
  { -- | Set whether or not merge requests can be merged with skipped jobs.
    project_edit_allow_merge_on_skipped_pipeline :: Maybe Bool,
    -- | One of disabled, private or enabled.
    project_edit_analytics_access_level :: Maybe Text,
    -- | How many approvers should approve merge request by default.
    project_edit_approvals_before_merge :: Maybe Int,
    -- | Auto-cancel pending pipelines.
    project_edit_auto_cancel_pending_pipelines :: Maybe EnabledDisabled,
    -- | Auto Deploy strategy (continuous, manual, or timed_incremental).
    project_edit_auto_devops_deploy_strategy :: Maybe AutoDeployStrategy,
    -- | Enable Auto DevOps for this project.
    project_edit_auto_devops_enabled :: Maybe Bool,
    -- | Set whether auto-closing referenced issues on default branch.
    project_edit_autoclose_referenced_issues :: Maybe Bool,
    -- | Test coverage parsing.
    project_edit_build_coverage_regex :: Maybe Text,
    -- | The Git strategy. Defaults to fetch.
    project_edit_build_git_strategy :: Maybe GitStrategy,
    -- | The maximum amount of time, in seconds, that a job can run.
    project_edit_build_timeout :: Maybe Int,
    -- | One of disabled, private, or enabled.
    project_edit_builds_access_level :: Maybe ProjectSettingAccessLevel,
    -- | The path to CI configuration file.
    project_edit_ci_config_path :: Maybe Text,
    -- | Default number of revisions for shallow cloning.
    project_edit_ci_default_git_depth :: Maybe Int,
    -- | When a new deployment job starts, skip older deployment jobs that are still pending.
    project_edit_ci_forward_deployment_enabled :: Maybe Bool,
    -- | Set visibility of container registry, for this project, to one of disabled, private or enabled.
    project_edit_container_registry_access_level :: Maybe ProjectSettingAccessLevel,
    -- | The default branch name.
    project_edit_default_branch :: Maybe Text,
    -- | Short project description.
    project_edit_description :: Maybe Text,
    -- | Disable email notifications.
    project_edit_emails_disabled :: Maybe Bool,
    -- | The classification label for the project.
    project_edit_external_authorization_classification_label :: Maybe Text,
    -- | One of disabled, private, or enabled.
    project_edit_forking_access_level :: Maybe ProjectSettingAccessLevel,
    -- | The ID or URL-encoded path of the project.
    project_edit_id :: Int,
    -- | URL to import repository from.
    project_edit_import_url :: Maybe Text,
    -- | One of disabled, private, or enabled.
    project_edit_issues_access_level :: Maybe ProjectSettingAccessLevel,
    -- | Enable LFS.
    project_edit_lfs_enabled :: Maybe Bool,
    -- | Set the merge method used.
    project_edit_merge_method :: Maybe MergeMethod,
    -- | One of disabled, private, or enabled.
    project_edit_merge_requests_access_level :: Maybe ProjectSettingAccessLevel,
    -- | Pull mirror overwrites diverged branches.
    project_edit_mirror_overwrites_diverged_branches :: Maybe Bool,
    -- | Pull mirroring triggers builds.
    project_edit_mirror_trigger_builds :: Maybe Bool,
    -- | User responsible for all the activity surrounding a pull mirror event. (admins only)
    project_edit_mirror_user_id :: Maybe Int,
    -- | Enables pull mirroring in a project.
    project_edit_mirror :: Maybe Bool,
    -- | The name of the project.
    project_edit_name :: Maybe Text,
    -- | One of disabled, private, or enabled.
    project_edit_operations_access_level :: Maybe ProjectSettingAccessLevel,
    -- | Set whether merge requests can only be merged when all the discussions are resolved.
    project_edit_only_allow_merge_if_all_discussions_are_resolved :: Maybe Bool,
    -- | Set whether merge requests can only be merged with successful jobs.
    project_edit_only_allow_merge_if_pipeline_succeeds :: Maybe Bool,
    -- | Only mirror protected branches.
    project_edit_only_mirror_protected_branches :: Maybe Bool,
    -- | Enable or disable packages repository feature.
    project_edit_packages_enabled :: Maybe Bool,
    -- | One of disabled, private, enabled, or public.
    project_edit_pages_access_level :: Maybe ProjectSettingAccessLevel,
    -- | One of disabled, private, enabled or public.
    project_edit_requirements_access_level :: Maybe ProjectSettingAccessLevel,
    -- | Allow only maintainers to pass user-defined variables when triggering a pipeline. For example when the pipeline is triggered in the UI, with the API, or by a trigger token.
    project_edit_restrict_user_defined_variables :: Maybe Bool,
    -- | Custom repository name for the project. By default generated based on name.
    project_edit_path :: Maybe Text,
    -- | If true, jobs can be viewed by non-project members.
    project_edit_public_builds :: Maybe Bool,
    -- | Enable Delete source branch option by default for all new merge requests.
    project_edit_remove_source_branch_after_merge :: Maybe Bool,
    -- | One of disabled, private, or enabled.
    project_edit_repository_access_level :: Maybe ProjectSettingAccessLevel,
    -- | Which storage shard the repository is on. (admins only)
    project_edit_repository_storage :: Maybe Text,
    -- | Allow users to request member access.
    project_edit_request_access_enabled :: Maybe Bool,
    -- | Automatically resolve merge request diffs discussions on lines changed with a push.
    project_edit_resolve_outdated_diff_discussions :: Maybe Bool,
    -- | Enable or disable Service Desk feature.
    project_edit_service_desk_enabled :: Maybe Bool,
    -- | Enable shared runners for this project.
    project_edit_shared_runners_enabled :: Maybe Bool,
    -- | Show default award emojis.
    project_edit_show_default_award_emojis :: Maybe Bool,
    -- | One of disabled, private, or enabled.
    project_edit_snippets_access_level :: Maybe ProjectSettingAccessLevel,
    -- | One of never, always, default_on, or default_off.
    project_edit_squash_option :: Maybe SquashOption,
    -- | The commit message used to apply merge request suggestions.
    project_edit_suggestion_commit_message :: Maybe Text,
    project_edit_visibility :: Maybe Visibility,
    -- | One of disabled, private, or enabled.
    project_edit_wiki_access_level :: Maybe ProjectSettingAccessLevel,
    -- | Default description for Issues. Description is parsed with GitLab Flavored Markdown. See Templates for issues and merge requests.
    project_edit_issues_template :: Maybe Text,
    -- | Default description for Merge Requests. Description is parsed with GitLab Flavored Markdown.
    project_edit_merge_requests_template :: Maybe Text,
    -- | Disable or enable the ability to keep the latest artifact for this project.
    project_edit_keep_latest_artifact :: Maybe Bool
  }
  deriving (Generic, Show, Eq)

-- | Is auto-cancel pending pipelines enabled or disabled
data EnabledDisabled
  = Enabled
  | Disabled
  deriving (Eq)

instance Show EnabledDisabled where
  show Enabled = "enabled"
  show Disabled = "disabled"

-- | Auto Deploy strategy: continuous, manual, or timed_incremental,
-- for the 'editProject' functions
data AutoDeployStrategy
  = Continuous
  | Manual
  | TimedIncremental
  deriving (Eq)

instance Show AutoDeployStrategy where
  show Continuous = "continuous"
  show Manual = "manual"
  show TimedIncremental = "timed_incremental"

-- | The Git strategy, defaults to fetch, for the 'editProject' functions
data GitStrategy
  = Clone
  | Fetch
  | None
  deriving (Eq)

instance Show GitStrategy where
  show Clone = "clone"
  show Fetch = "fetch"
  show None = "none"

-- | The project access level setting, for the 'editProject' functions
data ProjectSettingAccessLevel
  = DisabledAccess
  | PrivateAccess
  | EnabledAccess
  | PublicAccess
  deriving (Eq)

instance Show ProjectSettingAccessLevel where
  show DisabledAccess = "disabled"
  show PrivateAccess = "private"
  show EnabledAccess = "enabled"
  show PublicAccess = "public"

-- | The project git merge method, for the 'editProject' functions
data MergeMethod
  = Merge
  | RebaseMerge
  | FF
  deriving (Eq)

instance Show MergeMethod where
  show Merge = "merge"
  show RebaseMerge = "rebase_merge"
  show FF = "ff"

-- | The project git merge squash option, for the 'editProject' functions
data SquashOption
  = NeverSquash
  | AlwaysSquash
  | DefaultOnSquash
  | DefaultOffSquash
  deriving (Eq)

instance Show SquashOption where
  show NeverSquash = "never"
  show AlwaysSquash = "always"
  show DefaultOnSquash = "default_on"
  show DefaultOffSquash = "default_off"

-- | A default set of project searc filters where no project filters
-- are applied, thereby returning all projects.
defaultProjectSearchAttrs :: ProjectSearchAttrs
defaultProjectSearchAttrs =
  ProjectSearchAttrs Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Attributes related to a group
data ProjectSearchAttrs = ProjectSearchAttrs
  { -- | Limit by archived status.
    projectSearchFilter_archived :: Maybe Bool,
    -- | Limit results to projects with IDs greater than the specified
    -- ID.
    projectSearchFilter_id_after :: Maybe Int,
    -- | Limit results to projects with IDs less than the specified
    -- ID.
    projectSearchFilter_id_before :: Maybe Bool,
    -- | Limit results to projects which were imported from external
    -- systems by current user.
    projectSearchFilter_imported :: Maybe Bool,
    -- | Limit results to projects with last_activity after specified
    -- time.
    projectSearchFilter_last_activity_after :: Maybe UTCTime,
    -- | Limit results to projects with last_activity before specified
    -- time.
    projectSearchFilter_last_activity_before :: Maybe UTCTime,
    -- | Limit by projects that the current user is a member of.
    projectSearchFilter_membership :: Maybe Bool,
    -- | Limit by current user minimal access level.
    projectSearchFilter_min_access_level :: Maybe AccessLevel,
    -- | Return projects ordered by a given criteria.
    projectSearchFilter_order_by :: Maybe OrderBy,
    -- | Limit by projects explicitly owned by the current user.
    projectSearchFilter_owned :: Maybe Bool,
    -- | Limit projects where the repository checksum calculation has
    -- failed.
    projectSearchFilter_repository_checksum_failed :: Maybe Bool,
    -- | Limit results to projects stored on
    -- repository_storage. (administrators only).
    projectSearchFilter_repository_storage :: Maybe Text,
    -- | Include ancestor namespaces when matching search
    -- criteria. Default is false.
    projectSearchFilter_search_namespaces :: Maybe Bool,
    -- | Return list of projects matching the search criteria.
    projectSearchFilter_search :: Maybe Text,
    -- | Return only limited fields for each project. This is a no-op
    -- without authentication as then only simple fields are returned.
    projectSearchFilter_simple :: Maybe Bool,
    -- | Return projects sorted in asc or desc order. Default is desc.
    projectSearchFilter_sort :: Maybe SortBy,
    -- | Limit by projects starred by the current user.
    projectSearchFilter_starred :: Maybe Bool,
    -- | Include project statistics. Only available to Reporter or
    -- higher level role members.
    projectSearchFilter_statistics :: Maybe Bool,
    -- | Comma-separated topic names. Limit results to projects that
    -- match all of given topics.
    projectSearchFilter_topic :: Maybe Text,
    -- | Limit results to projects with the assigned topic given by
    -- the topic ID.
    projectSearchFilter_topic_id :: Maybe Int,
    -- | Limit by visibility.
    projectSearchFilter_visibility :: Maybe Visibility,
    -- | Include custom attributes in response. (administrator only).
    projectSearchFilter_with_custom_attributes :: Maybe Bool,
    -- | Limit by enabled issues feature.
    projectSearchFilter_with_issues_enabled :: Maybe Bool,
    -- | Limit by enabled merge requests feature.
    projectSearchFilter_with_merge_requests_enabled :: Maybe Bool,
    -- | Limit by projects which use the given programming language.
    projectSearchFilter_with_programming_language :: Maybe Text
  }

projectSearchAttrsParams :: ProjectSearchAttrs -> [GitLabParam]
projectSearchAttrsParams filters =
  catMaybes
    [ (\b -> Just ("archived", textToBS (showBool b))) =<< projectSearchFilter_archived filters,
      (\i -> Just ("id_after", textToBS (T.pack (show i)))) =<< projectSearchFilter_id_after filters,
      (\i -> Just ("id_before", textToBS (T.pack (show i)))) =<< projectSearchFilter_id_before filters,
      (\b -> Just ("imported", textToBS (showBool b))) =<< projectSearchFilter_imported filters,
      (\x -> Just ("last_activity_after", textToBS (T.pack (show x)))) =<< projectSearchFilter_last_activity_after filters,
      (\x -> Just ("last_activity_before", textToBS (T.pack (show x)))) =<< projectSearchFilter_last_activity_before filters,
      (\b -> Just ("membership", textToBS (showBool b))) =<< projectSearchFilter_membership filters,
      (\x -> Just ("min_access_level", textToBS (T.pack (show x)))) =<< projectSearchFilter_min_access_level filters,
      (\x -> Just ("order_by", textToBS (T.pack (show x)))) =<< projectSearchFilter_order_by filters,
      (\b -> Just ("owned", textToBS (showBool b))) =<< projectSearchFilter_owned filters,
      (\b -> Just ("repository_checksum_failed", textToBS (showBool b))) =<< projectSearchFilter_repository_checksum_failed filters,
      (\t -> Just ("repository_storage", textToBS t)) =<< projectSearchFilter_repository_storage filters,
      (\b -> Just ("search_namespaces", textToBS (showBool b))) =<< projectSearchFilter_search_namespaces filters,
      (\t -> Just ("search", textToBS t)) =<< projectSearchFilter_search filters,
      (\b -> Just ("simple", textToBS (showBool b))) =<< projectSearchFilter_simple filters,
      (\i -> Just ("sort", textToBS (T.pack (show i)))) =<< projectSearchFilter_sort filters,
      (\b -> Just ("starred", textToBS (showBool b))) =<< projectSearchFilter_starred filters,
      (\b -> Just ("statistics", textToBS (showBool b))) =<< projectSearchFilter_statistics filters,
      (\t -> Just ("topic", textToBS t)) =<< projectSearchFilter_topic filters,
      (\i -> Just ("topic_id", textToBS (T.pack (show i)))) =<< projectSearchFilter_topic_id filters,
      (\i -> Just ("visibility", textToBS (T.pack (show i)))) =<< projectSearchFilter_visibility filters,
      (\b -> Just ("with_custom_attributes", textToBS (showBool b))) =<< projectSearchFilter_with_custom_attributes filters,
      (\b -> Just ("with_issues_enabled", textToBS (showBool b))) =<< projectSearchFilter_with_issues_enabled filters,
      (\b -> Just ("with_merge_requests_enabled", textToBS (showBool b))) =<< projectSearchFilter_with_merge_requests_enabled filters,
      (\t -> Just ("with_programming_language", textToBS t)) =<< projectSearchFilter_with_programming_language filters
    ]
  where
    textToBS = Just . T.encodeUtf8
    showBool :: Bool -> Text
    showBool True = "true"
    showBool False = "false"

------------------
-- functions below are candidates for deletion

-- -- | finds all issues created by a user.
-- --
-- -- > issuesCreatedByUser "user1"
-- --
-- -- returns a (user,projects) tuple, where user is the 'User' found
-- -- for the given searched username, and a list of 'Project's that the
-- -- user has created issues for.
-- issuesCreatedByUser :: Text -> GitLab (Maybe (User, [Project]))
-- issuesCreatedByUser username = do
--   user_maybe <- searchUser username
--   case user_maybe of
--     Nothing -> return Nothing
--     Just usr -> do
--       usersIssues <- userIssues usr
--       projects <- mapM projectOfIssue usersIssues
--       return (Just (usr, projects))

-- -- | searches for all projects with the given name, and returns a list
-- -- of triples of: 1) the found project, 2) the list of issues for the
-- -- found projects, and 3) a list of users who've created issues.
-- issuesOnForks ::
--   -- | name or namespace of the project
--   Text ->
--   GitLab [(Project, [Issue], [User])]
-- issuesOnForks projectName = do
--   projects <- projectsWithName projectName
--   mapM processProject projects
--   where
--     processProject ::
--       Project ->
--       GitLab (Project, [Issue], [User])
--     processProject proj = do
--       (openIssues :: [Issue]) <- projectIssues proj defaultIssueFilters
--       let authors = map (fromMaybe (error "issuesOnForks error") . issue_author) openIssues
--       return (proj, openIssues, authors)

-- -- | returns a (namespace,members) tuple for the given 'Project',
-- -- where namespace is the namespace of the project
-- -- e.g. "user1/project1", and members is a list of (username,name)
-- -- tuples about all members of the project.
-- projectMemebersCount :: Project -> GitLab (Text, [(Text, Text)])
-- projectMemebersCount project = do
--   friends <- count
--   return (namespace_name (fromMaybe (error "projectMemebersCount error") (project_namespace project)), friends)
--   where
--     count = do
--       let addr =
--             "/projects/" <> T.pack (show (project_id project)) <> "/members/all"
--       (res :: [Member]) <- fromRight (error "projectMembersCount error") <$> gitlabGetMany addr []
--       return (map (\x -> (fromMaybe (error "projectMemebersCount error") (member_username x), fromMaybe (error "projectMemebersCount error") (member_name x))) res)

-- | returns 'True' is the last commit for a project passes all
-- continuous integration tests.
projectCISuccess ::
  -- | the name or namespace of the project
  Project ->
  GitLab Bool
projectCISuccess prj = do
  pipes <- pipelines prj
  case pipes of
    Nothing -> return False
    Just [] -> return False
    Just (x : _) -> return (pipeline_status x == "success")

-- -- | searches for a username, and returns a user ID for that user, or
-- -- 'Nothing' if a user cannot be found.
-- namespacePathToUserId ::
--   -- | name or namespace of project
--   Text ->
--   GitLab (Maybe Int)
-- namespacePathToUserId namespacePath = do
--   user_maybe <- searchUser namespacePath
--   case user_maybe of
--     Nothing -> return Nothing
--     Just usr -> return (Just (user_id usr))
