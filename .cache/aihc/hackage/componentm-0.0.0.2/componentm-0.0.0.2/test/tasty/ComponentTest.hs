{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ComponentTest where

import           Control.Exception       (ErrorCall (..), MaskingState (..),
                                          getMaskingState)
import           RIO

import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit

import qualified Control.Monad.Component as SUT
import           Control.Teardown        (toredownCount)


tests :: TestTree
tests = testGroup
  "ComponentM"
  [ testGroup
    "Async Exceptions"
    [ testCase "it releases previously allocated resources" $ do
        callCountRef <- newIORef (0 :: Int)

        let alloc = do
              modifyIORef callCountRef (+ 1)
              return ()
            release         = const $ return ()

            componentOne    = SUT.buildComponent "one" alloc release

            componentTwo    = SUT.buildComponent "two" alloc release

            componentThree  = SUT.buildComponent_ "three" (threadDelay 10100100)

            componentAction = do
              componentOne
              componentTwo
              componentThree

        resultAsync <- async $ SUT.runComponentM "test application"
                                                 componentAction
                                                 (const $ return ())

        threadDelay 500
        cancel resultAsync

        result <- waitCatch resultAsync
        case result of
          Left _err -> do
            callCount <- readIORef callCountRef
            assertEqual "teardown action got called more than once" 2 callCount

          Right _ -> assertFailure "expecting failure; got success instead"
    ]
  , testGroup
    "Sync Exceptions"
    [ testCase "it releases previously allocated resources" $ do
      callCountRef <- newIORef (0 :: Int)

      let
        alloc = do
          modifyIORef callCountRef (+ 1)
          return ()
        release        = const $ return ()

        componentOne   = SUT.buildComponent "one" alloc release

        componentTwo   = SUT.buildComponent "two" alloc release

        componentThree = SUT.buildComponent
          "three"
          (throwIO (ErrorCall "failing three"))
          release

        componentFour =
          SUT.buildComponent_ "four" (throwIO $ ErrorCall "failing four")

        componentAction = do
          componentOne
          void componentTwo
          void componentThree
          componentFour

      result <- try $ SUT.runComponentM "test application"
                                        componentAction
                                        (const $ return ())
      case result of
        Left (SUT.ComponentBuildFailed _appErr teardownResult) -> do
          callCount <- readIORef callCountRef
          assertEqual "got more than one two valid components" 2 callCount

          -- 1. component one
          -- 2. component two
          -- 3. allocated component three (notice, error happens once allocation is done)
          assertEqual "There should be three toredown resources"
                      3
                      (toredownCount teardownResult)

        _ ->
          assertFailure $ "expected error, but did not happen, " <> show result
    , testCase "component construction allows throwM calls" $ do
      callCountRef <- newIORef (0 :: Int)

      let alloc = do
            modifyIORef callCountRef (+ 1)
            return ()
          release         = const $ return ()

          componentOne    = SUT.buildComponent "one" alloc release

          componentTwo    = SUT.buildComponent "two" alloc release

          componentAction = do
            componentOne
            void $ throwM (ErrorCall "failing via MonadThrow")
            componentTwo

      result <- try
        $ SUT.runComponentM "test application" componentAction return
      case result of
        Left (SUT.ComponentBuildFailed [SUT.ComponentErrorThrown err] _teardownResult)
          -> do
            callCount <- readIORef callCountRef
            assertEqual "expected introduced error, got different one"
                        (Just $ ErrorCall "failing via MonadThrow")
                        (fromException err)
            assertEqual "teardown action got called more than once" 1 callCount

        Left err ->
          assertFailure
            $  "expected ComponentStartupFailure exception, got instead: "
            <> show err

        Right _ -> assertFailure "expected error, but did not happen"
    ]
  , testGroup
    "Masking"
    [ testCase "app callback is _always_ on unmasked state" $ do
        maskingVar <- newEmptyMVar
        let alloc = return ()
            release _ = getMaskingState >>= putMVar maskingVar
            componentOne = SUT.buildComponent "one" alloc release

        SUT.runComponentM "app" componentOne return
        masking <- takeMVar maskingVar

        assertEqual "App callback is always in unmasked state"
                    MaskedUninterruptible
                    masking
    ]
  , testGroup
    "Duplicate component names"
    [ testCase "fails with an exception" $ do
        callCountRef <- newIORef (0 :: Int)

        let alloc = do
              modifyIORef callCountRef (+ 1)
              return ()
            release         = const $ return ()

            componentOne    = SUT.buildComponent "one" alloc release

            componentTwo    = SUT.buildComponent "two" alloc release

            componentThree  = SUT.buildComponent_ "three" (return ())


            componentAction = do
              componentOne
              void componentTwo
              void componentThree
              void componentThree

        result <- try $ SUT.runComponentM "test application"
                                          componentAction
                                          (const $ return ())
        case result of
          Left (SUT.ComponentBuildFailed appErr teardownResult) -> do
            callCount <- readIORef callCountRef
            case appErr of
              [SUT.DuplicatedComponentKeyDetected componentName] -> assertEqual
                "should report duplicated component name"
                "three"
                componentName
              _ ->
                assertFailure
                  $  "expecting single component build error error; got: "
                  <> show appErr

            assertEqual "got more than one two valid components" 2 callCount

            -- 1. component one
            -- 2. component two
            -- 3. allocated component three (notice, error happens once allocation is done)
            assertEqual "There should be three toredown resources"
                        3
                        (toredownCount teardownResult)

          _ -> assertFailure "expected error, but did not happen"
    ]
  , testGroup
    "Monad"
    [ testCase "aggregates multiple component teardown values" $ do
      callCountRef <- newIORef (0 :: Int)

      let alloc = do
            modifyIORef callCountRef (+ 1)
            return ()
          release         = const $ return ()

          componentOne    = SUT.buildComponent "one" alloc release

          componentTwo    = SUT.buildComponent "two" alloc release

          componentThree  = SUT.buildComponent_ "three" $ return ()

          componentAction = do
            componentOne
            componentTwo
            componentThree

      _result <- SUT.runComponentM "test application"
                                   componentAction
                                   (const $ return ())

      callCount <- readIORef callCountRef
      assertEqual "teardown action got called more than once" 2 callCount
    , testCase "initializes components sequentially on the same thread" $ do
      let componentOne    = SUT.buildComponent_ "one" myThreadId

          componentTwo    = SUT.buildComponent_ "two" myThreadId

          componentThree  = SUT.buildComponent_ "three" myThreadId

          componentAction = do
            t1 <- componentOne
            t2 <- componentTwo
            t3 <- componentThree
            return (t1, t2, t3)


      (t1, t2, t3) <- SUT.runComponentM "test application"
                                        componentAction
                                        return

      assertBool "expecting t1 to be equal to t2" (t1 == t2)
      assertBool "expecting t2 to be equal to t3" (t2 == t3)
    ]
  , testGroup
    "Applicative"
    [ testCase "reports multiple failures" $ do
      callCountRef <- newIORef (0 :: Int)

      let
        alloc = do
          modifyIORef callCountRef (+ 1)
          return ()
        release      = const $ return ()

        componentOne = SUT.buildComponent "one" alloc release

        componentTwo =
          SUT.buildComponent_ "two" $ throwIO (ErrorCall "failing two")

        componentThree = SUT.buildComponent
          "three"
          (throwIO (ErrorCall "failing three"))
          release

        componentAction = componentOne *> componentTwo *> componentThree

      result <- try
        $ SUT.runComponentM "test application" componentAction return
      case result of
        Left (SUT.ComponentBuildFailed [SUT.ComponentAllocationFailed _desc2 err2, SUT.ComponentAllocationFailed _desc3 err3] _teardownResult)
          -> do

            assertEqual "expected introduced error, got different one"
                        (Just $ ErrorCall "failing two")
                        (fromException err2)

            assertEqual "expected introduced error, got different one"
                        (Just $ ErrorCall "failing three")
                        (fromException err3)

            callCount <- readIORef callCountRef
            assertEqual "teardown action got called more than once" 1 callCount

        Left err ->
          assertFailure
            $ "expected ComponentStartupFailure exception with two errors, got instead: "
            <> show err

        Right _ -> assertFailure "expected error, but did not happen"
    , testCase "initializes components concurrently" $ do
      let
        componentOne   = SUT.buildComponent_ "one" myThreadId

        componentTwo   = SUT.buildComponent_ "two" myThreadId

        componentThree = SUT.buildComponent_ "three" myThreadId

        componentAction =
          (,,) <$> componentOne <*> componentTwo <*> componentThree


      (t1, t2, t3) <- SUT.runComponentM "test application"
                                        componentAction
                                        return

      assertBool "expecting t1 to be different than t2" (t1 /= t2)
      assertBool "expecting t2 to be different than t3" (t2 /= t3)
      assertBool "expecting t1 to be different than t3" (t1 /= t3)
    ]
  , testGroup
    "MonadIO"
    [ testCase "wraps IO exceptions with info" $ do
        callCountRef <- newIORef (0 :: Int)

        let alloc   = return ()
            release = const $ do
              modifyIORef callCountRef (+ 1)
              return ()

            componentOne    = SUT.buildComponent "one" alloc release

            componentTwo    = SUT.buildComponent_ "two" $ return ()

            componentAction = do
              componentOne
              void $ liftIO $ throwIO (ErrorCall "failing on liftIO")
              componentTwo

        result <- try
          $ SUT.runComponentM "test application" componentAction return
        case result of
          Left (SUT.ComponentBuildFailed [SUT.ComponentIOLiftFailed ex] teardownResult)
            -> do
              releaseCount <- readIORef callCountRef
              assertEqual
                "should have called the release of component one, but didn't"
                1
                releaseCount
              assertEqual
                "should have called the release of component one, but didn't"
                1
                (toredownCount teardownResult)
              assertEqual "should have reported the liftIO error, but didn't"
                          (Just $ ErrorCall "failing on liftIO")
                          (fromException ex)
          _ ->
            assertFailure
              $  "expecting to get component build failed error, got instead: "
              <> show result
    ]
  ]
