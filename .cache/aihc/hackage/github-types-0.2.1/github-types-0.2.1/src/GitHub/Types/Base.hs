{-# LANGUAGE OverloadedStrings #-}

module GitHub.Types.Base where


import Control.Applicative
import Control.Monad

import Data.Aeson hiding (Error, Success)
import Data.Text



------------------------------------------------------------------------------
-- Owner

data Owner = Owner
    { ownerId        :: Int
    , ownerLogin     :: Text
    , ownerType      :: Text
    } deriving (Eq, Show)


instance FromJSON Owner where
    parseJSON (Object x) = Owner
        <$> x .: "id"
        <*> x .: "login"
        <*> x .: "type"

    parseJSON _ = mzero



------------------------------------------------------------------------------
-- Repository

data Repository = Repository
    { repositoryId        :: Int
    , repositoryName      :: Text
    , repositoryFullName  :: Text
    , repositoryOwner     :: Owner
    } deriving (Eq, Show)


instance FromJSON Repository where
    parseJSON (Object x) = Repository
        <$> x .: "id"
        <*> x .: "name"
        <*> x .: "full_name"
        <*> x .: "owner"

    parseJSON _ = mzero



------------------------------------------------------------------------------
-- State

data State = Pending | Success | Failure | Error
    deriving (Eq, Show)


instance FromJSON State where
    parseJSON (String "pending") = return Pending
    parseJSON (String "success") = return Success
    parseJSON (String "failure") = return Failure
    parseJSON (String "error")   = return Error
    parseJSON _                  = mzero

instance ToJSON State where
    toJSON Pending = String "pending"
    toJSON Success = String "success"
    toJSON Failure = String "failure"
    toJSON Error   = String "error"



------------------------------------------------------------------------------
-- Deployment

data Deployment = Deployment
    { deploymentId          :: Int
    , deploymentSha         :: Text
    , deploymentRef         :: Text
    , deploymentTask        :: Text
    , deploymentEnvironment :: Text
    , deploymentPayload     :: Value
    , deploymentDescription :: Text
    } deriving (Eq, Show)

instance FromJSON Deployment where
    parseJSON (Object x) = Deployment
        <$> x .: "id"
        <*> x .: "sha"
        <*> x .: "ref"
        <*> x .: "task"
        <*> x .: "environment"
        <*> x .: "payload"
        <*> x .: "description"

    parseJSON _ = mzero



------------------------------------------------------------------------------
-- DeploymentStatus

data DeploymentStatus = DeploymentStatus
    { deploymentStatusId             :: Int
    , deploymentStatusState          :: Text
    , deploymentStatusTargetUrl      :: Maybe Text
    , deploymentStatusDescription    :: Maybe Text
    , deploymentStatusDeploymentUrl  :: Maybe Text
    } deriving (Eq, Show)

instance FromJSON DeploymentStatus where
    parseJSON (Object x) = DeploymentStatus
        <$> x .: "id"
        <*> x .: "state"
        <*> x .: "target_url"
        <*> x .: "description"
        <*> x .: "deployment_url"

    parseJSON _ = mzero
