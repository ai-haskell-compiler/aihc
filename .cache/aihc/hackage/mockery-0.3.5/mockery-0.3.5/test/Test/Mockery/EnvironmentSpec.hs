module Test.Mockery.EnvironmentSpec where

import           System.Environment.Compat
import           Test.Hspec

import           Test.Mockery.Environment

spec :: Spec
spec = do
  describe "withEnvironment" $ do
    it "runs the given action within the specified environment" $ do
      withEnvironment [("foo", "bar")] $ do
        getEnvironment `shouldReturn` [("foo", "bar")]

    it "restores the original environment" $ do
      old <- getEnvironment
      withEnvironment [("foo", "bar")] $ do
        return ()
      new <- getEnvironment
      new `shouldMatchList` old

  describe "withModifiedEnvironment" $ do
    it "runs the given action within a modified environment" $ do
      original <- getEnvironment
      withModifiedEnvironment [("foo", "bar")] $ do
        modified <- getEnvironment
        modified `shouldMatchList` original ++ [("foo", "bar")]

    it "restores the original environment" $ do
      old <- getEnvironment
      withModifiedEnvironment [("foo", "bar")] $ do
        return ()
      new <- getEnvironment
      new `shouldMatchList` old
