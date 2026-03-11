module Test.Mockery.DirectorySpec where

import           Test.Hspec
import           System.Directory
import           System.Environment
import           Control.Concurrent
import           Data.Foldable

import           Test.Mockery.Directory

whenTravis :: IO () -> IO ()
whenTravis action = do
  env <- getEnvironment
  forM_ (lookup "TRAVIS" env) (const action)

spec :: Spec
spec = do
  describe "withFile" $ do
    it "creates a temporary file with the given contents" $ do
      withFile "foo" $ \file ->
        readFile file `shouldReturn` "foo"

  describe "touch" $ do
    around_ inTempDirectory $ do
      let name = "foo"
      it "creates an empty file" $ do
        touch name
        readFile name `shouldReturn` ""

      it "creates any missing directories" $ do
        touch "foo/bar/baz"
        readFile "foo/bar/baz" `shouldReturn` ""

      context "when file already exists" $ do
        before_ (writeFile name "bar") $ do
          it "updates modification time" $ do
            t0 <- getModificationTime name
            threadDelay 10000
            whenTravis (threadDelay 1000000)
            touch name
            t1 <- getModificationTime name
            t0 `shouldSatisfy` (< t1)

          it "does not modify file content" $ do
            touch name
            readFile name `shouldReturn` "bar"
