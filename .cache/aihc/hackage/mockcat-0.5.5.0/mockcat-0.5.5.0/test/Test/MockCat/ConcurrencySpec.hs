{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.MockCat.ConcurrencySpec (spec) where

import Test.Hspec
import Test.MockCat
import Prelude hiding (any)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM, replicateM_)
import Control.Monad.IO.Unlift (withRunInIO, MonadUnliftIO)

class Monad m => ConcurrencyAction m where
  action :: Int -> m Int

makeMock [t|ConcurrencyAction|]

-- -------------------------
-- test target functions
parallelActionSum :: (ConcurrencyAction m, MonadUnliftIO m) => Int -> m Int
parallelActionSum n = do
  values <- withRunInIO \runInIO -> do
    as <- replicateM n (async $ runInIO (action 49))
    mapM wait as
  pure (sum values)

parallelCallActionWithDelay :: (ConcurrencyAction m, MonadUnliftIO m) => Int -> Int -> m ()
parallelCallActionWithDelay threads callsPerThread =
  withRunInIO \runInIO -> do
    as <- replicateM threads (async $ do
      replicateM_ callsPerThread $ do
        _ <- runInIO (action 123)
        threadDelay 100)
    mapM_ wait as

parallelCallActionN :: (ConcurrencyAction m, MonadUnliftIO m) => Int -> m ()
parallelCallActionN n = withRunInIO \runInIO -> do
  as <- replicateM n (async $ runInIO (action 7))
  mapM_ wait as
-- -------------------------

spec :: Spec
spec = do
  describe "Concurrency / applyTimesIs" do
    it "counts calls across parallel async threads" do
      result <- runMockT do
        _action (any |> (1 :: Int)) `applyTimesIs` 10

        parallelActionSum 10
      result `shouldBe` 10

    it "stress concurrent applyTimesIs with nested unlifts" do
      let threads = 50 :: Int
          callsPerThread = 20 :: Int
          total = threads * callsPerThread :: Int
      _ <- (runMockT $ do
        _action (any |> (1 :: Int)) `applyTimesIs` total
        parallelCallActionWithDelay threads callsPerThread
        ) :: IO ()
      pure ()

    it "fails verification when calls are fewer than declared" do
      runMockT (do
        _action (any |> (1 :: Int)) `applyTimesIs` 10
        parallelCallActionN 9
        pure ()
        ) `shouldThrow` anyErrorCall

  describe "Concurrency / neverApply" do
    it "neverApply passes when stub not used in parallel context" do
      r <- runMockT do
        neverApply $ _action (any |> (99 :: Int))

        pure (123 :: Int)
      r `shouldBe` 123
