{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Members
-- Description : Queries about and updates to members of projects and groups
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2021
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Members
  ( AccessLevel (..),

    -- * Projects

    -- * Project membership
    memberOfProject,
    membersOfProject,
    memberOfProjectWithInherited,
    membersOfProjectWithInherited,

    -- ** Adding project members
    addMemberToProject,
    addMembersToProject,

    -- ** Editing project members
    editMemberOfProject,

    -- ** Removing project members
    removeUserFromProject,

    -- * Groups

    -- * Group membership
    memberOfGroup,
    membersOfGroup,
    memberOfGroupWithInherited,
    membersOfGroupWithInherited,

    -- ** Adding group members
    addAllUsersToGroup,
    addUserToGroup,
    addUsersToGroup,

    -- ** Editing group members
    editMemberOfGroup,

    -- ** Removing group members
    removeUserFromGroup,

    -- ** Pending members
    approvePendingMember,
    approveAllPendingMembers,
    pendingMembers,
  )
where

import qualified Data.ByteString.Lazy as BSL
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.API.Users
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client
import Network.HTTP.Types.URI

-----------
-- projects
-----------

-- | Gets a list of project members viewable by the authenticated
-- user. Returns only direct members and not inherited members through
-- ancestors groups.
membersOfProject :: Project -> GitLab [Member]
membersOfProject prj =
  fromRight (error "membersOfProject error")
    <$> gitlabGetMany addr []
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/members"

-- | Gets a list of project members viewable by the authenticated
-- user, including inherited members, invited users, and permissions
-- through ancestor groups.
--
-- If a user is a member of this project and also of one or more
-- ancestor groups, only its membership with the highest access_level
-- is returned. This represents the effective permission of the user.
--
-- Members from an invited group are returned if either: the invited
-- group is public, or the requester is also a member of the invited group.
membersOfProjectWithInherited :: Project -> GitLab (Either (Response BSL.ByteString) [Member])
membersOfProjectWithInherited prj =
  gitlabGetMany addr []
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/members"
        <> "/all"

-- | Gets a member of a project. Returns only direct members and not
-- inherited members through ancestor groups.
memberOfProject ::
  -- | The project
  Project ->
  -- | The user ID of the member
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Member))
memberOfProject prj usrId =
  gitlabGetOne addr []
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/members/"
        <> T.pack (show usrId)

-- | Gets a member of a project, including members inherited or
-- invited through ancestor groups.
--
-- If a user is a member of this project and also of one or more
-- ancestor groups, only its membership with the highest access_level
-- is returned. This represents the effective permission of the user.
--
-- Members from an invited group are returned if either: the invited
-- group is public, or the requester is also a member of the invited
-- group.
memberOfProjectWithInherited ::
  -- | The project
  Project ->
  -- | The user ID of the member
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Member))
memberOfProjectWithInherited prj usrId =
  gitlabGetOne addr []
  where
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/members"
        <> "/all/"
        <> T.pack (show usrId)

-- | Adds a member to a project.
addMemberToProject ::
  -- | project ID
  Project ->
  -- | level of access
  AccessLevel ->
  -- | user ID
  User ->
  GitLab (Either (Response BSL.ByteString) (Maybe Member))
addMemberToProject prj access usr =
  gitlabPost addr params
  where
    params :: [GitLabParam]
    params =
      [ ("user_id", Just (T.encodeUtf8 (T.pack (show (user_id usr))))),
        ("access_level", Just (T.encodeUtf8 (T.pack (show access))))
      ]
    addr =
      "/projects/" <> T.pack (show (project_id prj)) <> "/members"

-- | adds a list of users to a project with the given access
-- level. Returns 'Right Member' for each successful action, otherwise
-- it returns 'Left Status'.
addMembersToProject ::
  -- | the project
  Project ->
  -- | level of access
  AccessLevel ->
  -- | users to add to the project
  [User] ->
  GitLab [Either (Response BSL.ByteString) (Maybe Member)]
addMembersToProject project access =
  mapM (addMemberToProject project access)

-- | Updates a member of a project.
editMemberOfProject ::
  -- | the project
  Project ->
  -- | the new level of access
  AccessLevel ->
  -- | user ID
  User ->
  GitLab (Either (Response BSL.ByteString) (Maybe Member))
editMemberOfProject prj access usr =
  gitlabPut addr params
  where
    params :: [GitLabParam]
    params =
      [ ("access_level", Just (T.encodeUtf8 (T.pack (show access))))
      ]
    addr =
      "/projects/"
        <> T.pack (show (project_id prj))
        <> "/members/"
        <> T.pack (show (user_id usr))

---------
-- groups
---------

-- | Gets a list of group members viewable by the authenticated
-- user. Returns only direct members and not inherited members through
-- ancestors groups.
membersOfGroup :: Group -> GitLab (Either (Response BSL.ByteString) [Member])
membersOfGroup grp =
  gitlabGetMany addr []
  where
    addr =
      "/groups/"
        <> T.pack (show (group_id grp))
        <> "/members"

-- | Gets a member of a group. Returns only direct members
-- and not inherited members through ancestor groups.
memberOfGroup ::
  -- | The group
  Group ->
  -- | The user ID of the member
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Member))
memberOfGroup grp usrId =
  gitlabGetOne addr []
  where
    addr =
      "/projects/"
        <> T.pack (show (group_id grp))
        <> "/members/"
        <> T.pack (show usrId)

-- | Gets a member of a group, including members inherited or invited
-- through ancestor groups.
--
-- If a user is a member of this group and also of one or more
-- ancestor groups, only its membership with the highest access_level
-- is returned. This represents the effective permission of the user.
--
-- Members from an invited group are returned if either: the invited
-- group is public, or the requester is also a member of the invited
-- group.
memberOfGroupWithInherited ::
  -- | The group
  Group ->
  -- | The user ID of the member
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Member))
memberOfGroupWithInherited prj usrId =
  gitlabGetOne addr []
  where
    addr =
      "/groups/"
        <> T.pack (show (group_id prj))
        <> "/members"
        <> "/all/"
        <> T.pack (show usrId)

-- | Gets a list of group members viewable by the authenticated
-- user, including inherited members, invited users, and permissions
-- through ancestor groups.
--
-- If a user is a member of this group and also of one or more
-- ancestor groups, only its membership with the highest access_level
-- is returned. This represents the effective permission of the user.
--
-- Members from an invited group are returned if either: the invited
-- group is public, or the requester is also a member of the invited group.
membersOfGroupWithInherited :: Group -> GitLab (Either (Response BSL.ByteString) [Member])
membersOfGroupWithInherited prj =
  gitlabGetMany addr []
  where
    addr =
      "/groups/"
        <> T.pack (show (group_id prj))
        <> "/members"
        <> "/all"

-- | adds all registered users to a group.
addAllUsersToGroup ::
  -- | the group
  Group ->
  -- | level of access granted
  AccessLevel ->
  GitLab [Either (Response BSL.ByteString) (Maybe Member)]
addAllUsersToGroup grp access = do
  allRegisteredUsers <- users
  addUsersToGroup grp access allRegisteredUsers

-- | Adds a member to a group.
addUserToGroup ::
  -- | the group
  Group ->
  -- | level of access granted
  AccessLevel ->
  -- | the user
  User ->
  GitLab (Either (Response BSL.ByteString) (Maybe Member))
addUserToGroup grp access usr = do
  gitlabPost addr params
  where
    params :: [GitLabParam]
    params =
      [ ("user_id", Just (T.encodeUtf8 (T.pack (show (user_id usr))))),
        ("access_level", Just (T.encodeUtf8 (T.pack (show access))))
      ]
    addr =
      "/groups/"
        <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 (T.pack (show (group_id grp)))))
        <> "/members"

-- | adds a list of users to a group.
addUsersToGroup ::
  -- | the group
  Group ->
  -- | level of access granted
  AccessLevel ->
  -- | list of usernames to be added to the group
  [User] ->
  GitLab [Either (Response BSL.ByteString) (Maybe Member)]
addUsersToGroup grp access =
  mapM (addUserToGroup grp access)

-- | Updates a member of a group.
editMemberOfGroup ::
  -- | the group
  Group ->
  -- | the new level of access
  AccessLevel ->
  -- | user ID
  User ->
  GitLab (Either (Response BSL.ByteString) (Maybe Member))
editMemberOfGroup grp access usr =
  gitlabPut addr params
  where
    params :: [GitLabParam]
    params =
      [ ("access_level", Just (T.encodeUtf8 (T.pack (show access))))
      ]
    addr =
      "/groups/"
        <> T.pack (show (group_id grp))
        <> "/members/"
        <> T.pack (show (user_id usr))

-- | Removes a user from a project where the user has been explicitly
-- assigned a role.
--
-- The user needs to be a group member to qualify for removal. For
-- example, if the user was added directly to a project within the
-- group but not this group explicitly, you cannot use this API to
-- remove them.
removeUserFromProject ::
  -- | the project
  Project ->
  -- | user
  User ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
removeUserFromProject prj usr = do
  result <- gitlabDelete addr []
  case result of
    Left err -> return (Left err)
    -- GitLab version 14.2.3 returns Version JSON info when a
    -- member is removed from a group/project. I'm not sure if
    -- this is new behaviour, anyway we catch it here.
    Right (Just (Version {})) -> return (Right (Just ()))
    Right Nothing -> return (Right (Just ()))
  where
    addr =
      "/"
        <> "projects"
        <> "/"
        <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 (T.pack (show (project_id prj)))))
        <> "/members/"
        <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 (T.pack (show (user_id usr)))))

-- | Removes a user from a group where the user has been explicitly
-- assigned a role.
--
-- The user needs to be a group member to qualify for removal. For
-- example, if the user was added directly to a project within the
-- group but not this group explicitly, you cannot use this API to
-- remove them.
removeUserFromGroup ::
  -- | the group
  Group ->
  -- | user
  User ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
removeUserFromGroup grp usr = do
  result <- gitlabDelete addr []
  case result of
    Left err -> return (Left err)
    -- GitLab version 14.2.3 returns Version JSON info when a
    -- member is removed from a group/project. I'm not sure if
    -- this is new behaviour, anyway we catch it here.
    Right (Just (Version {})) -> return (Right (Just ()))
    Right Nothing -> return (Right (Just ()))
  where
    addr =
      "/"
        <> "groups"
        <> "/"
        <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 (T.pack (show (group_id grp)))))
        <> "/members/"
        <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 (T.pack (show (user_id usr)))))

-- | Approves a pending user for a group and its subgroups and
-- projects.
approvePendingMember ::
  -- | the group
  Group ->
  -- | the member
  User ->
  GitLab (Either (Response BSL.ByteString) (Maybe Member))
approvePendingMember grp usr =
  gitlabPut addr []
  where
    addr =
      "/groups/"
        <> T.pack (show (group_id grp))
        <> "/members/"
        <> T.pack (show (user_id usr))
        <> "/approve"

-- | Approves all pending users for a group and its subgroups and
-- projects.
approveAllPendingMembers ::
  -- | the group
  Group ->
  GitLab (Either (Response BSL.ByteString) (Maybe Member))
approveAllPendingMembers grp =
  gitlabPut addr []
  where
    addr =
      "/groups/"
        <> T.pack (show (group_id grp))
        <> "/members/"
        <> "/approve_all"

-- | For a group and its subgroups and projects, get a list of all
-- members in an awaiting state and those who are invited but do not
-- have a GitLab account. This request returns all matching group and
-- project members from all groups and projects in the root group’s
-- hierarchy. When the member is an invited user that has not signed
-- up for a GitLab account yet, the invited email address is
-- returned. This API endpoint works on top-level groups only. It does
-- not work on subgroups. This API endpoint requires permission to
-- administer members for the group.
pendingMembers ::
  -- | the group
  Group ->
  GitLab (Either (Response BSL.ByteString) [Member])
pendingMembers grp =
  gitlabGetMany addr []
  where
    addr =
      "/groups/"
        <> T.pack (show (group_id grp))
        <> "/pending_members"
