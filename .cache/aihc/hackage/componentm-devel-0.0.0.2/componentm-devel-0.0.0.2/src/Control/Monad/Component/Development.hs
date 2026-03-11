{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Component.Development
  (
  -- * Making 'ComponentM' values useful
    ComponentM
  , runComponentDevel

  -- * Error Records
  , ComponentError (..)
  , ComponentBuildError (..)

  -- * 'ComponentM' tracing accessors
  , ComponentEvent (..)
  , Build
  , buildElapsedTime
  , buildFailure
  , BuildResult
  , toBuildList
  )
  where

import           RIO

import           Control.Monad.Component.Internal.Types
import           Control.Teardown                       (Teardown,
                                                         emptyTeardown,
                                                         newTeardown,
                                                         runTeardown)
import           Foreign.Store

devTeardownStoreNum :: Word32
devTeardownStoreNum = 0

runComponentDevel_
  :: (ComponentEvent -> IO ()) -- ^ Callback function to trace 'ComponentEvent' records
  -> Text                      -- ^ Name of your application (used for tracing purposes)
  -> ComponentM a              -- ^ Builder of your application environment
  -> (a -> IO b)               -- ^ Function where your main application will live
  -> IO Teardown
runComponentDevel_ !logFn !appName (ComponentM buildFn) !appFn =
  mask $ \restore -> do
    result <- restore buildFn
    case result of
      Left (errList, buildTable) -> do
        appTeardown    <- buildTableToTeardown appName buildTable
        teardownResult <- runTeardown appTeardown
        restore $ logFn $ ComponentErrorDetected
          (ComponentBuildFailed errList teardownResult)
        return $ emptyTeardown "development"

      Right (resource, buildTable) -> do
        let buildList = buildTableToOrderedList buildTable
        restore $ logFn $ ComponentBuilt $ BuildResult $ reverse buildList

        appTeardown      <- buildTableToTeardown appName buildTable
        appAsync         <- asyncWithUnmask $ \unmask -> unmask $ appFn resource

        appAsyncTeardown <- newTeardown "application async"
                                        (cancel appAsync :: IO ())
        newTeardown "development" [appTeardown, appAsyncTeardown]

-- | Similar to 'runComponentM1', when running for the first time, it creates an
-- application in the REPL environment, subsequent invocations will teardown the
-- and build up the application again.
--
-- All 'ComponentM' characteristics are driven by this particular use-case given:
--
-- * It will print out the time spent on initialization and teardown
-- * It guarantees that teardown operations are as robust as possible
-- * It documents your application components to pin-point quickly errors in your
--   reloading logic
--
runComponentDevel
  :: (ComponentEvent -> IO ()) -- ^ Callback function to trace 'ComponentEvent' records
  -> Text                      -- ^ Name of your application (used for tracing purposes)
  -> ComponentM a              -- ^ Builder of your application environment
  -> (a -> IO b)               -- ^ Function where your main application will
                               --   live, note this function must block the thread
                               -- as how the normal main would
  -> IO ()
runComponentDevel !logFn !appName !builder !appFn = do
  mdevTeardownStore <- lookupStore devTeardownStoreNum
  case mdevTeardownStore of
    Nothing -> do
      devTeardown <- runComponentDevel_ logFn appName builder appFn
      writeStore (Store devTeardownStoreNum) devTeardown

    Just devTeardownStore -> do
      devTeardown0   <- readStore devTeardownStore
      teardownResult <- runTeardown (devTeardown0 :: Teardown)
      logFn $ ComponentReleased teardownResult

      devTeardown <- runComponentDevel_ logFn appName builder appFn
      writeStore (Store devTeardownStoreNum) devTeardown
