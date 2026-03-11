{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Component.Internal.Core
  ( buildComponent
  , buildComponent_
  , runComponentM
  , runComponentM1
  ) where

import           RIO
import           RIO.Time                               (NominalDiffTime,
                                                         diffUTCTime,
                                                         getCurrentTime)

import qualified RIO.HashMap                            as HashMap
import qualified RIO.Set                                as Set

import           Control.Monad.Component.Internal.Types
import           Control.Teardown                       (emptyTeardown,
                                                         newTeardown,
                                                         runTeardown)

--------------------------------------------------------------------------------

-- | Track duration time of the execution of an IO sub-routine
trackExecutionTime :: IO a -> IO (NominalDiffTime, a)
trackExecutionTime routine = do
  start  <- getCurrentTime
  result <- routine
  end    <- getCurrentTime
  return (diffUTCTime end start, result)

-- | Transforms an `IO` sub-routine into a `ComponentM` sub-routine; the given
-- `IO` sub-routine returns a resource that does not allocate any other
-- resources that would need to be cleaned up on a system shutdown.
--
-- This is similar to using 'liftIO', with the caveat that the library will
-- register the given 'IO' sub-routine as a Component, and it will keep track
-- and report its initialization timespan
--
buildComponent_ :: Text -> IO a -> ComponentM a
buildComponent_ !componentDesc ma = ComponentM $ mask $ \restore -> do
  (buildElapsedTime, result) <- trackExecutionTime (try $ restore ma)
  case result of
    Left err -> do
      let build = Build
            { componentDesc
            , componentTeardown = emptyTeardown componentDesc
            , buildElapsedTime
            , buildFailure      = Just err
            , buildDependencies = Set.empty
            }
          buildTable = HashMap.singleton componentDesc build
      return $ Left ([ComponentAllocationFailed componentDesc err], buildTable)

    Right output -> do
      let build = Build
            { componentDesc
            , componentTeardown = emptyTeardown componentDesc
            , buildElapsedTime
            , buildFailure      = Nothing
            , buildDependencies = Set.empty
            }
          buildTable = HashMap.singleton componentDesc build
      return $ Right (output, buildTable)

-- | Use this function when you want to allocate a new resource (e.g. Database,
-- Socket, etc). It registers the constructed resource in your application
-- component tree and guarantees that its cleanup sub-routine is executed at the
-- end of your program.
--
-- This function is similar to the 'bracket' function with the caveat that it
-- expects a 'Text' argument which identifies the component being allocated.
--
-- NOTE: The name of your component must be unique; otherwise a
-- 'DuplicatedComponentKeyDetected' will be thrown
--
buildComponent
  :: Text         -- ^ Unique name for the component being allocated
  -> IO a         -- ^ Allocation 'IO' sub-routine
  -> (a -> IO ()) -- ^ Cleanup 'IO' sub-routine
  -> ComponentM a
buildComponent !componentDesc construct release =
  ComponentM $ mask $ \restore -> do

    (buildElapsedTime, (result, componentTeardown)) <- trackExecutionTime
      $ startComponent restore

    let buildFailure = either (Just . toException) (const Nothing) result

        build        = Build
          { componentDesc
          , componentTeardown
          , buildElapsedTime
          , buildFailure
          , buildDependencies = Set.empty
          }

        buildTable = HashMap.singleton componentDesc build

    case result of
      Left  err      -> return $ Left ([err], buildTable)
      Right resource -> return $ Right (resource, buildTable)
 where
  startComponent restore = do
    result <- restore (try construct)
    case result of
      Left err -> return
        ( Left $ ComponentAllocationFailed componentDesc err
        , emptyTeardown componentDesc
        )
      Right resource -> do
        resourceTeardown <- newTeardown componentDesc (release resource)
        return (Right resource, resourceTeardown)

-- | Enhances 'runComponentM' with a callback function that emits
-- 'ComponentEvent' records. These events are a great way of tracing the
-- lifecycle and structure of your application.
runComponentM1
  :: (ComponentEvent -> IO ()) -- ^ Callback function to trace 'ComponentEvent' records
  -> Text                      -- ^ Name of your application (used for tracing purposes)
  -> ComponentM a              -- ^ Builder of your application environment
  -> (a -> IO b)               -- ^ Function where your main application will live
  -> IO b
runComponentM1 !logFn !appName (ComponentM buildFn) !appFn =
  mask $ \restore -> do
    result <- restore buildFn
    case result of
      Left (errList, buildTable) -> do
        appTeardown    <- buildTableToTeardown appName buildTable
        teardownResult <- runTeardown appTeardown
        restore $ logFn $ ComponentReleased teardownResult
        throwIO $ ComponentBuildFailed errList teardownResult

      Right (resource, buildTable) -> do
        let buildList = buildTableToOrderedList buildTable
        restore $ logFn $ ComponentBuilt $ BuildResult $ reverse buildList

        appTeardown    <- buildTableToTeardown appName buildTable
        appResult      <- tryAny $ restore $ appFn resource
        teardownResult <- runTeardown appTeardown
        restore $ logFn $ ComponentReleased teardownResult

        case appResult of
          Left appError ->
            throwIO $ ComponentRuntimeFailed appError teardownResult
          Right output -> return output

-- | Constructs the /environment/ of your application by executing the 'IO'
-- sub-routines provided in the 'buildComponent' and 'buildComponent_'
-- functions; it then executes a callback where your main application will run.
--
-- This function:
--
-- * Keeps track of initialization elapsed time for each component of your
--   application
--
-- * Initializes components concurrently as long as they are composed using
--   'Applicative' functions
--
-- * Builds a graph of your dependencies automatically when composing your
--   'ComponentM' values via 'Applicative' or 'Monad' interfaces; it then
--   guarantees the execution of cleanup operations in a topological sorted
--   order
--
-- * Guarantees the proper cleanup of previously allocated resources if the
--   creation of a resource throws an exception on initialization
--
-- * Guarantees best-effort cleanup of resources on application teardown in the
--   scenario where a cleanup sub-routine throws an exception
--
-- * Keeps track of teardown elasped time for each component of your
--   application; and reports what exceptions was thrown in case of failures
--
-- If you want to trace the behavior of your application on initialization and
-- teardown, use 'runComponentM1' instead
runComponentM
  :: Text           -- ^ Name of your application (used for tracing purposes)
  -> ComponentM a   -- ^ Builder of your application environment
  -> (a -> IO b)    -- ^ Function where your main application will live
  -> IO b
runComponentM = runComponentM1 (const $ return ())
