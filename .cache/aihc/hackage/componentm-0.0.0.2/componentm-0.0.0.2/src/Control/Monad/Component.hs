{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : Control.Monad.Component
Description : Sane resource allocation library for complex applications
Copyright   : (c) Roman Gonzalez, 2017-2018
License     : MIT
Maintainer  : open-source@roman-gonzalez.info
Stability   : experimental

= Why use 'ComponentM'?

'ComponentM' values wraps vanilla 'IO' sub-routines whose responsibility is to
allocate resources that your application may need (e.g. database connections,
tcp sockets, etc). Your program will execute these 'ComponentM' sub-routines at
the beginning of it's lifecyle, building an environment that your main
application needs in order to work as intended.

By using 'ComponentM' sub-routines your program will automatically:

* Compose the cleanup sub-routines of all your allocated resources

* Keep track of initialization time for each resource needed in your application

* Keep track of teardown time for each resources needed in your application.

* Isolate the teardown of each resource in your application, ensuring no thrown
  exception will affect the cleanup of resources.

* Initialize resources concurrently when using 'Applicative' notation

* Build a dependency graph of your application resources when using
  'Applicative' or 'Monad' notation; and then guarantees the execution of
  cleanup operations in a topological sorted order

* Make sure that previously allocated resources are cleaned up when a resource
  throws an exception on initialization

* Report all exceptions thrown on each resource teardown

* Document (through types) what is the purpose of some of the 'IO' sub-routines
  in your program

These properties are crucial when applications need to run for long periods of
time and they are reloaded (without a process restart). It also ensures that
resources are cleaned tightly when doing REPL driven development through GHCi.

-}
module Control.Monad.Component
  (

  -- * How to build 'ComponentM' values
  -- $howto_componentm_values
    buildComponent
  , buildComponent_

  -- * Making 'ComponentM' values useful
  -- $monad_and_runners
  , ComponentM
  , runComponentM
  , runComponentM1

  -- * Error Records
  -- $errors
  , ComponentError (..)
  , ComponentBuildError (..)

  -- * 'ComponentM' tracing accessors
  , ComponentEvent (..)
  , Build
  , buildElapsedTime
  , buildFailure
  , BuildResult
  , toBuildList


  -- * Re-exports
  , TeardownResult (..)
  , throwM

  ) where

import           Control.Monad.Catch                    (throwM)
import           Control.Monad.Component.Internal.Core  (buildComponent,
                                                         buildComponent_,
                                                         runComponentM,
                                                         runComponentM1)
import           Control.Monad.Component.Internal.Types (Build (..),
                                                         BuildResult (..),
                                                         ComponentBuildError (..),
                                                         ComponentError (..),
                                                         ComponentEvent (..),
                                                         ComponentM)
import           Control.Teardown                       (TeardownResult (..))

{- $howto_componentm_values

'ComponentM' values are built from vanilla 'IO' sub-routines that allocate resources, the
two functions provided are:

['buildComponent_']: Used when a component in your application does not allocate
  a resource

['buildComponent']: Used when a component in your application allocates a
  resource and requires a cleanup on teardown

Following is an example on how to

@
{-# LANGUAGE PackageImports #-}
import "sqlite-simple" qualified Database.SQLite.Simple as SQLite
import "componentm" Control.Monad.Component (ComponentM, buildComponent, buildComponent_)

-- | App environment record
data AppEnv = AppEnv { appDb :: !SQLite.Connection }

-- | Configuration record
data Config = Config { dbPath :: !String }

readConfig :: IO Config
readConfig =
  -- NOTE: Here we would have a more sophisticated algorithm for fetching
  -- configuration values for our app
  return (Config ":memory:")

configComponent :: ComponentM AppConfig
configComponent = buildComponent_ "Config" $ do
  readConfigFile "./resources/config.yml"

dbComponent :: FilePath -> ComponentM SQLite.Connection
dbComponent dbPath =
  buildComponent "Database" (SQLite.open dbPath) SQLite.close

buildAppEnv :: ComponentM AppEnv
buildAppEnv = do
  config <- configComponent
  AppEnv <$> dbComponent (dbPath config)

@

In the previous example, we use both 'buildComponent_' and 'buildComponent' to
create different components that our application needs.

-}

{- $monad_and_runers

To execute our 'ComponentM' sub-routines, we can use two different functions
'runComponentM' or 'runComponentM1'; following is an example:


@
appMain :: AppEnv -> IO ()
appMain = error "pending"

main :: IO ()
main =
  runComponentM1
    -- Our logging function
    print
    -- The name of our application
    "my-fancy-application"
    -- The 'ComponentM' sub-routine that builds the 'AppEnv'
    buildAppEnv
    -- Our main application runs here
    appMain
@

-}

{- $errors

There are two possible failures that the 'runComponentM' functions can thrown

['ComponentBuildFailed']: This error happens when allocation of some component's
resource fails

['ComponentRuntimeFailed']: This error happens when there is an exception thrown
from our main application callback

-}
