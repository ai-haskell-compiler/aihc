{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Users
-- Description : Queries about registered users
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Users
  ( -- * List users
    users,

    -- * Single user
    user,
    searchUser,

    -- * User creation
    createUser,

    -- * User modification
    userAttributes,
    modifyUser,

    -- * Delete authentication identity from user
    deleteAuthIdentity,

    -- * User deletion
    deleteUser,

    -- * List current user
    currentUser,

    -- * User status
    currentUserStatus,

    -- * Get the status of a user
    userStatus,

    -- * Get user preferences
    userPreferences,

    -- * Follow and unfollow users
    followUser,
    unfollowUser,

    -- * User counts
    currentUserCounts,

    -- * List SSH keys
    currentUserSshKeys,

    -- * List SSH keys for user
    userSshKeys,

    -- * Add SSH key
    addSshKeyCurrentUser,

    -- * Add SSH key for user
    addSshKeyUser,

    -- * Delete SSH key for current user
    deleteSshKeyCurrentUser,

    -- * Delete SSH key for given user
    deleteSshKeyUser,

    -- * List emails
    emails,

    -- * List emails for user
    emailsCurrentUser,

    -- * Block or unblock user
    blockUser,
    unblockUser,

    -- * Activate or deactivate user
    activateUser,
    deactivateUser,

    -- * Ban or unban user
    banUser,
    unbanUser,

    -- * Approve or reject user
    approveUser,
    rejectUser,

    -- * Users attributes
    defaultUserFilters,
    UserAttrs (..),
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

-- | all registered users.
users :: GitLab [User]
users = do
  let pathUser = "/users"
  fromRight (error "allUsers error") <$> gitlabGetMany pathUser []

-- | Get a single user.
user ::
  -- | ID of users
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe User))
user usrId =
  gitlabGetOne pathUser []
  where
    pathUser =
      "/users/"
        <> T.pack (show usrId)

-- | Extracts the user attributes for a user. Useful for modifying
-- attrbibutes with 'modifyUser'.
userAttributes ::
  -- | the user
  User ->
  -- | is the user a GitLab server administrator
  Bool ->
  -- | the extracted user attributes
  UserAttrs
userAttributes usr isAdmin =
  UserAttrs
    (Just isAdmin)
    (user_bio usr)
    (user_can_create_group usr)
    (user_email usr)
    (user_extern_uid usr)
    (user_external usr) -- default is false
    (user_force_random_password usr) -- default is false
    (user_group_id_for_saml usr)
    (user_linkedin usr)
    (user_location usr)
    (Just (user_name usr))
    (user_note usr)
    (user_organization usr)
    (user_password usr)
    (user_private_profile usr) -- default is false
    (user_projects_limit usr)
    (user_providor usr)
    (user_reset_password usr)
    (user_skip_confirmation usr)
    (user_skype usr)
    (user_theme_id usr)
    (user_twitter usr)
    (Just (user_username usr))
    (user_view_diffs_file_by_file usr)
    (user_website_url usr)
    (user_pronouns usr)

-- | Creates a new user. Note only administrators can create new
-- users. Either password, reset_password, or force_random_password
-- must be specified. If reset_password and force_random_password are
-- both false, then password is required. force_random_password and
-- reset_password take priority over password. In addition,
-- reset_password and force_random_password can be used together.
createUser ::
  -- | email address
  Text ->
  -- | user's name
  Text ->
  -- | user's username
  Text ->
  -- | optional attributes
  UserAttrs ->
  GitLab (Either (Response BSL.ByteString) (Maybe User))
createUser emailAddr name username attrs =
  gitlabPost
    userAddr
    ( [ ("name", Just (T.encodeUtf8 name)),
        ("username", Just (T.encodeUtf8 username)),
        ("email", Just (T.encodeUtf8 emailAddr))
      ]
        <> userAttrs attrs
    )
  where
    userAddr :: Text
    userAddr =
      "/users"

-- | Modifies an existing user. Only administrators can change
-- attributes of a user.
modifyUser ::
  -- | user ID
  Int ->
  -- | optional attributes
  UserAttrs ->
  GitLab (Either (Response BSL.ByteString) (Maybe User))
modifyUser userId attrs =
  gitlabPut
    userAddr
    (userAttrs attrs)
  where
    userAddr :: Text
    userAddr =
      "/users/"
        <> T.pack (show userId)

-- | Deletes a user’s authentication identity using the provider name
-- associated with that identity. Available only for administrators.
deleteAuthIdentity ::
  -- | user
  User ->
  -- | external providor name
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteAuthIdentity usr providor =
  gitlabDelete userAddr []
  where
    userAddr :: Text
    userAddr =
      "/users/"
        <> T.pack (show (user_id usr))
        <> "/identities/"
        <> providor

-- | Deletes a user. Available only for administrators.
deleteUser ::
  -- | user
  User ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteUser usr =
  gitlabDelete userAddr []
  where
    userAddr :: Text
    userAddr =
      "/users/"
        <> T.pack (show (user_id usr))

-- | Get current user.
currentUser :: GitLab User
currentUser =
  fromMaybe (error "currentUser") . fromRight (error "currentUser error") <$> gitlabGetOne pathUser []
  where
    pathUser =
      "/user"

-- | Get current user status.
currentUserStatus :: GitLab UserStatus
currentUserStatus =
  fromMaybe (error "currentUserStatus") . fromRight (error "currentUserStatus error") <$> gitlabGetOne pathUser []
  where
    pathUser =
      "/user/status"

-- | Get the status of a user.
userStatus ::
  -- | user
  User ->
  GitLab UserStatus
userStatus usr =
  fromMaybe (error "userStatus") . fromRight (error "userStatus error") <$> gitlabGetOne pathUser []
  where
    pathUser =
      "/users/"
        <> T.pack (show (user_id usr))
        <> "/status"

-- | Get the status of the current user.
userPreferences ::
  GitLab UserPrefs
userPreferences =
  fromMaybe (error "userPreferences") . fromRight (error "userPreferences error") <$> gitlabGetOne pathUser []
  where
    pathUser =
      "/user/preferences"

-- | Follow a user.
followUser ::
  -- | user
  User ->
  GitLab (Either (Response BSL.ByteString) (Maybe User))
followUser usr =
  gitlabPost
    userAddr
    []
  where
    userAddr :: Text
    userAddr =
      "/users/"
        <> T.pack (show (user_id usr))
        <> "/follow"

-- | Unfollow a user.
unfollowUser ::
  -- | user
  User ->
  GitLab (Either (Response BSL.ByteString) (Maybe User))
unfollowUser usr =
  gitlabPost
    userAddr
    []
  where
    userAddr :: Text
    userAddr =
      "/users/"
        <> T.pack (show (user_id usr))
        <> "/unfollow"

-- | Get the counts of the currently signed in user.
currentUserCounts :: GitLab UserCount
currentUserCounts =
  fromMaybe (error "currentUserCounts") . fromRight (error "currentUserCounts error") <$> gitlabGetOne pathUser []
  where
    pathUser =
      "/user_counts"

-- | Get a list of currently authenticated user’s SSH keys.
currentUserSshKeys :: GitLab Key
currentUserSshKeys =
  fromMaybe (error "currentUserSshKeys") . fromRight (error "currentUserSshKeys error") <$> gitlabGetOne pathUser []
  where
    pathUser =
      "/user/keys"

-- | Get a list of a specified user’s SSH keys.
userSshKeys ::
  -- | user
  User ->
  GitLab Key
userSshKeys usr =
  fromMaybe (error "userSshKeys") . fromRight (error "userSshKeys error") <$> gitlabGetOne pathUser []
  where
    pathUser =
      "/user/"
        <> T.pack (show (user_id usr))
        <> "/keys"

-- | Creates a new key owned by the currently authenticated user.
addSshKeyCurrentUser ::
  -- | key
  Text ->
  -- | title
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Key))
addSshKeyCurrentUser theKey theTitle =
  gitlabPost
    userAddr
    [ ("key", Just (T.encodeUtf8 theKey)),
      ("title", Just (T.encodeUtf8 theTitle))
    ]
  where
    userAddr :: Text
    userAddr =
      "/user/keys"

-- | Create new key owned by specified user. Available only for
-- administrator.
addSshKeyUser ::
  -- | User
  User ->
  -- | key
  Text ->
  -- | title
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Key))
addSshKeyUser usr theKey theTitle =
  gitlabPost
    userAddr
    [ ("key", Just (T.encodeUtf8 theKey)),
      ("title", Just (T.encodeUtf8 theTitle))
    ]
  where
    userAddr :: Text
    userAddr =
      "/users/"
        <> T.pack (show (user_id usr))
        <> "/keys"

-- | Deletes key owned by currently authenticated user.
deleteSshKeyCurrentUser ::
  -- | key ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteSshKeyCurrentUser keyId =
  gitlabDelete userAddr []
  where
    userAddr :: Text
    userAddr =
      "/users/keys/"
        <> T.pack (show keyId)

-- | Deletes key owned by a specified user. Available only for
-- administrator.
deleteSshKeyUser ::
  -- | user
  User ->
  -- | key ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteSshKeyUser usr keyId =
  gitlabDelete userAddr []
  where
    userAddr :: Text
    userAddr =
      "/users/"
        <> T.pack (show (user_id usr))
        <> "/keys/"
        <> T.pack (show keyId)

-- | Get a list of currently authenticated user’s emails.
emails :: GitLab [Email]
emails = do
  let pathUser = "/user/emails/"
  fromRight (error "emails error") <$> gitlabGetMany pathUser []

-- | Get a list of currently authenticated user’s emails.
emailsCurrentUser ::
  -- | user
  User ->
  GitLab [Email]
emailsCurrentUser usr = do
  let pathUser =
        "/user/"
          <> T.pack (show (user_email usr))
          <> "/emails/"
  fromRight (error "emails error") <$> gitlabGetMany pathUser []

-- | Used internally by the following functions
userAction ::
  -- | user action
  Text ->
  -- | function name for the error
  Text ->
  -- | user
  User ->
  GitLab (Maybe User)
userAction action funcName usr =
  fromRight (error (T.unpack funcName <> " error")) <$> gitlabPost userAddr []
  where
    userAddr :: Text
    userAddr =
      "/users/"
        <> T.pack (show (user_id usr))
        <> action

-- | Blocks the specified user. Available only for administrator.
blockUser ::
  -- | user
  User ->
  GitLab (Maybe User)
blockUser = userAction "/block" "blockUser"

-- | Unblocks the specified user. Available only for administrator.
unblockUser ::
  -- | user
  User ->
  GitLab (Maybe User)
unblockUser = userAction "/unblock" "unblockUser"

-- | Deactivates the specified user. Available only for administrator.
deactivateUser ::
  -- | user
  User ->
  GitLab (Maybe User)
deactivateUser = userAction "/deactivate" "deactivateUser"

-- | Activates the specified user. Available only for administrator.
activateUser ::
  -- | user
  User ->
  GitLab (Maybe User)
activateUser = userAction "/activate" "activateUser"

-- | Bans the specified user. Available only for administrator.
banUser ::
  -- | user
  User ->
  GitLab (Maybe User)
banUser = userAction "/ban" "banUser"

-- | Unbans the specified user. Available only for administrator.
unbanUser ::
  -- | user
  User ->
  GitLab (Maybe User)
unbanUser = userAction "/unban" "unbanUser"

-- | Approves the specified user. Available only for administrator.
approveUser ::
  -- | user
  User ->
  GitLab (Maybe User)
approveUser = userAction "/approve" "approveUser"

-- | Rejects specified user that is pending approval. Available only for administrator.
rejectUser ::
  -- | user
  User ->
  GitLab (Maybe User)
rejectUser = userAction "/reject" "rejectUser"

-- | No group filters applied, thereby returning all groups.
defaultUserFilters :: UserAttrs
defaultUserFilters =
  UserAttrs Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Attributes related to a group
data UserAttrs = UserAttrs
  { -- | User is an administrator - default is false
    userFilter_admin :: Maybe Bool,
    -- | User’s biography
    userFilter_bio :: Maybe Text,
    -- | User can create groups
    userFilter_can_create_group :: Maybe Bool,
    -- | email address
    userFilter_email :: Maybe Text,
    -- | External UID
    userFilter_extern_uid :: Maybe Int,
    -- | Flag the user as external - default is fale
    userFilter_external :: Maybe Bool,
    -- | Set user password to a random value - default is false
    userFilter_force_random_password :: Maybe Bool,
    -- | ID of group where SAML has been configured
    userFilter_group_id_for_saml :: Maybe Int,
    -- | User's LinkedIn account
    userFilter_linkedin :: Maybe Text,
    -- | User's location
    userFilter_location :: Maybe Text,
    -- | User's name
    userFilter_name :: Maybe Text,
    -- | Administrator notes for this user
    userFilter_note :: Maybe Text,
    -- | Organization name
    userFilter_organization :: Maybe Text,
    -- | User's password
    userFilter_password :: Maybe Text,
    -- | User’s profile is private - default is false
    userFilter_private_profile :: Maybe Bool,
    -- | Number of projects user can create
    userFilter_projects_limit :: Maybe Int,
    -- | External provider name
    userFilter_providor :: Maybe Text,
    -- | Send user password reset link - default is false
    userFilter_reset_password :: Maybe Bool,
    -- | Skip confirmation - default is false
    userFilter_skip_confirmation :: Maybe Bool,
    -- | User's Skype ID
    userFilter_skype :: Maybe Text,
    -- | User's theme ID - GitLab theme for the user
    userFilter_theme_id :: Maybe Int,
    -- | User's Twitter account
    userFilter_twitter :: Maybe Text,
    -- | User's username
    userFilter_username :: Maybe Text,
    -- | Flag indicating the user sees only one file diff per page
    userFilter_view_diffs_file_by_file :: Maybe Bool,
    -- | User's website URL
    userFilter_website :: Maybe Text,
    -- | User's pronouns
    userFilter_pronouns :: Maybe Text
  }

userAttrs :: UserAttrs -> [GitLabParam]
userAttrs filters =
  catMaybes
    [ (\t -> Just ("admin", textToBS t)) =<< userFilter_name filters,
      (\t -> Just ("bio", textToBS t)) =<< userFilter_bio filters,
      (\b -> Just ("can_create_group", textToBS (showBool b))) =<< userFilter_can_create_group filters,
      (\t -> Just ("email", textToBS t)) =<< userFilter_email filters,
      (\i -> Just ("extern_uid", textToBS (T.pack (show i)))) =<< userFilter_extern_uid filters,
      (\b -> Just ("external", textToBS (showBool b))) =<< userFilter_external filters,
      (\b -> Just ("force_random_password", textToBS (showBool b))) =<< userFilter_force_random_password filters,
      (\i -> Just ("group_id_for_saml", textToBS (T.pack (show i)))) =<< userFilter_group_id_for_saml filters,
      (\t -> Just ("linkedin", textToBS t)) =<< userFilter_linkedin filters,
      (\t -> Just ("location", textToBS t)) =<< userFilter_location filters,
      (\t -> Just ("name", textToBS t)) =<< userFilter_name filters,
      (\t -> Just ("note", textToBS t)) =<< userFilter_note filters,
      (\t -> Just ("organization", textToBS t)) =<< userFilter_organization filters,
      (\t -> Just ("password", textToBS t)) =<< userFilter_password filters,
      (\b -> Just ("private_profile", textToBS (showBool b))) =<< userFilter_private_profile filters,
      (\i -> Just ("projects_limit", textToBS (T.pack (show i)))) =<< userFilter_projects_limit filters,
      (\t -> Just ("providor", textToBS t)) =<< userFilter_providor filters,
      (\b -> Just ("reset_password", textToBS (showBool b))) =<< userFilter_reset_password filters,
      (\b -> Just ("skip_confirmation", textToBS (showBool b))) =<< userFilter_skip_confirmation filters,
      (\t -> Just ("skype", textToBS t)) =<< userFilter_skype filters,
      (\i -> Just ("theme_id", textToBS (T.pack (show i)))) =<< userFilter_theme_id filters,
      (\t -> Just ("twitter", textToBS t)) =<< userFilter_twitter filters,
      (\t -> Just ("username", textToBS t)) =<< userFilter_username filters,
      (\b -> Just ("view_diffs_file_by_file", textToBS (showBool b))) =<< userFilter_view_diffs_file_by_file filters,
      (\t -> Just ("website", textToBS t)) =<< userFilter_website filters,
      (\t -> Just ("pronouns", textToBS t)) =<< userFilter_pronouns filters
    ]
  where
    textToBS = Just . T.encodeUtf8
    showBool :: Bool -> Text
    showBool True = "true"
    showBool False = "false"

-- | searches for a user given a username.
searchUser ::
  -- | username to search for
  Text ->
  GitLab (Maybe User)
searchUser username = do
  let pathUser = "/users"
      params = [("username", Just (T.encodeUtf8 username))]
  result <- gitlabGetMany pathUser params
  case result of
    Left _err -> return Nothing
    Right [] -> return Nothing
    Right (x : _) -> return (Just x)
