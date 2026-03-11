{-| The purpose of this library is to assist you in writing Haskell apps that
    run on Cloud Foundry. It provides convenience functions and structures that
    map to Cloud Foundry environment variable primitives.

    This package is a port of https://github.com/cloudfoundry-community/go-cfenv
-}

module System.CloudFoundry.Environment
  ( current
  , isRunningOnCf
  , lookupCurrent
  , module System.CloudFoundry.Environment.Internal.Service
  , module System.CloudFoundry.Environment.Internal.Services
  , module System.CloudFoundry.Environment.Internal.Types
  ) where

import Control.Exception (Exception, throw)
import Control.Monad ((>=>))
import Data.Char (isSpace)
import System.Environment (lookupEnv)

import qualified System.CloudFoundry.Environment.Internal.EnvVars as EnvVars
import System.CloudFoundry.Environment.Internal.Service
import System.CloudFoundry.Environment.Internal.Services
import System.CloudFoundry.Environment.Internal.Types
import qualified System.CloudFoundry.Environment.Internal.VcapApplicationDecoder as VcapApplication
import qualified System.CloudFoundry.Environment.Internal.VcapServicesDecoder as VcapServices

{- | Detect if the application is running as a Cloud Foundry application.

> import System.CloudFoundry.Environment CfEnv
>
> main :: IO ()
> main = do
>     isRunningOnCf <- CfEnv.isRunningOnCf
>
>     if isRunningOnCf
>         then putStrLn "Running on Cloud Foundry"
>         else putStrLn "Not running on Cloud Foundry"
-}
isRunningOnCf :: IO Bool
isRunningOnCf =
    envHasValue "VCAP_APPLICATION"
  where
    envHasValue = lookupEnv >=> return . maybeHasValue
    maybeHasValue = maybe False hasValue
    hasValue = not . isEmptyString . trimLeft
    trimLeft = dropWhile isSpace
    isEmptyString = (==) ""

{-| Get the current Cloud Foundry environment.

    Example using @scotty@:

> import Data.String (fromString)
> import Data.Monoid (mconcat)
>
> import Web.Scotty
>
> import qualified System.CloudFoundry.Environment as CfEnv
>
> main = do
>   app <- CfEnv.current
>
>   scotty (CfEnv.port app) $
>     get "/" $ do
>       html $ mconcat ["<pre>", (fromString (show app)), "</pre>"]
-}
current :: IO Application
current = do
    envVars <- EnvVars.getEnvVars
    vcapApp <- decodeVcapApplication (EnvVars.vcapApplication envVars)
    vcapServices <- decodeVcapServices (EnvVars.vcapServices envVars)

    return $ mkApplication envVars vcapApp vcapServices
  where
    decodeVcapApplication =
      eitherToThrow VcapApplication.decode (DecodeError "VCAP_APPLICATION")

    decodeVcapServices =
      eitherToThrow VcapServices.decode (DecodeError "VCAP_SERVICES")

-- | Get the current Cloud Foundry environment and return the result in a Maybe.
--   See `current`.
lookupCurrent :: IO (Maybe Application)
lookupCurrent = do
  isOnCf <- isRunningOnCf

  if isOnCf
    then fmap Just current
    else return Nothing

eitherToThrow :: (Exception ex)
              => (input -> Either error output)
              -> (error -> ex)
              -> input
              -> IO output
eitherToThrow fn exFn input =
  case fn input of
    Right output  -> return output
    Left errorMsg -> throw $ exFn errorMsg

mkApplication :: EnvVars.EnvVars
              -> VcapApplication.VcapApplication
              -> Services
              -> Application
mkApplication envVars vcapApp vcapServices =
    Application
      { appId = VcapApplication.appId vcapApp
      , appName = VcapApplication.appName vcapApp
      , applicationUris = VcapApplication.applicationUris vcapApp
      , cfApi = VcapApplication.cfApi vcapApp
      , home = EnvVars.home envVars
      , host = VcapApplication.host vcapApp
      , index = VcapApplication.index vcapApp
      , instanceId = VcapApplication.instanceId vcapApp
      , limits = VcapApplication.limits vcapApp
      , memoryLimit = EnvVars.memoryLimit envVars
      , port = EnvVars.port envVars
      , pwd = EnvVars.pwd envVars
      , services = vcapServices
      , spaceId = VcapApplication.spaceId vcapApp
      , spaceName = VcapApplication.spaceName vcapApp
      , tmpDir = EnvVars.tmpDir envVars
      , user = EnvVars.user envVars
      , version = VcapApplication.version vcapApp
      }
