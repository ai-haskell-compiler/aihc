{-# LANGUAGE DeriveGeneric #-}

module System.CloudFoundry.Environment.Internal.Types where

import Control.Exception (Exception)
import Data.Map.Strict (Map)
import GHC.Generics

import qualified Data.Aeson as Aeson

-- | Holds information about the current app running on Cloud Foundry.
--   This is returned from 'current'.
data Application = Application
  { appId           :: String   -- ^ ID of the application
  , applicationUris :: [String] -- ^ Application URI of the app
  , cfApi           :: String   -- ^ URL for the Cloud Foundry API endpoint
  , home            :: String   -- ^ Root folder for the deployed app
  , host            :: String   -- ^ Host of the app
  , instanceId      :: String   -- ^ ID of the instance
  , index           :: Int      -- ^ Index of the app
  , limits          :: Limits   -- ^ Limits imposed on this process
  , memoryLimit     :: String   -- ^ Maximum amount of memory that each instance of the application can consume
  , appName         :: String   -- ^ Name of the app
  , pwd             :: String   -- ^ Present working directory, where the buildpack that processed the application ran
  , port            :: Int      -- ^ Port which the app must listen on to receive traffic
  , services        :: Services -- ^ Services bound to the app
  , spaceId         :: String   -- ^ ID of the space
  , spaceName       :: String   -- ^ Name of the space
  , tmpDir          :: String   -- ^ Directory location where temporary and staging files are stored
  , user            :: String   -- ^ User account under which the app instance runs
  , version         :: String   -- ^ Version of the app
  } deriving (Eq, Show)

data Limits = Limits
  { disk :: Int -- ^ Disk limit
  , fds  :: Int -- ^ File descriptors limit
  , mem  :: Int -- ^ Memory limit
  } deriving (Eq, Show, Generic)

instance Aeson.FromJSON Limits

-- | Description of a bound service. Use `credentialString` to extract the
--   connection details.
data Service = Service
  { name  :: String
  , label :: String
  , tags  :: [String]
  , plan  :: String
  , credentials :: Map String String
  } deriving (Eq, Show, Generic)

instance Aeson.FromJSON Service

-- | A collection of 'Service' instances. 'withTag', 'withName' and 'withLabel' can be
--   used to find the specific services that you want.
newtype Services = Services (Map String [Service]) deriving (Eq, Show)

-- | Exceptions which are raised from this package.
data CfEnvError
    -- | Thrown when a JSON decode failed for either the VCAP_APPLICATION or
    --   VCAP_SERIVCES environment variables.
    = DecodeError String String
    -- | Thrown when an environment variable which is meant to contain an
    --   integer contains invalid characters.
    | NotInteger String String deriving (Eq)

instance Exception CfEnvError

instance Show CfEnvError where
    show (DecodeError envName errorMsg) = envName ++ " " ++ errorMsg
    show (NotInteger envName value) = envName ++ " must be an integer, got '" ++ value ++ "'."
