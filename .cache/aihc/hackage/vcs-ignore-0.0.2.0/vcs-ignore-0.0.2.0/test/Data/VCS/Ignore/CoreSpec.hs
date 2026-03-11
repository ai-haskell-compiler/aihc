{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TypeApplications  #-}

module Data.VCS.Ignore.CoreSpec
  ( spec
  )
where

import           Control.Monad.Catch            ( throwM )
import qualified Data.List                     as L
import           Data.VCS.Ignore.Core

import           Data.VCS.Ignore.Repo           ( Repo(..)
                                                , RepoError(..)
                                                )
import           System.FilePath                ( (</>) )
import           Test.Hspec


spec :: Spec
spec = do

  describe "findRepo" $ do
    it "finds repo for some path inside repo" $ do
      let path     = testRepoRoot </> "a" </> "b"
          expected = TestRepo testRepoRoot
      findRepo path `shouldReturn` Just expected

    it "finds no repo for path outside repo" $ do
      let path = "some" </> "path"
      findRepo path `shouldReturn` Nothing @TestRepo


  describe "listRepo" $ do
    it "lists repository paths, based on the search filter" $ do
      let expected =
            [ ".gitignore"
            , "a"
            , "a" </> ".gitignore"
            , "a" </> "b"
            , "a" </> "b" </> ".gitignore"
            , "a" </> "b" </> "test-b.txt"
            , "a" </> "b" </> "test-b.xml"
            , "a" </> "test-a.txt"
            , "a" </> "test-a.xml"
            ]
      repo   <- scanRepo @TestRepo testRepoRoot
      result <- listRepo repo
      L.sort result `shouldBe` L.sort expected


  describe "walkRepo" $ do
    it "walks repository paths, based on the search filter" $ do
      let fn = \path -> pure ("foo" </> path)
          expected =
            [ "foo" </> ".gitignore"
            , "foo" </> "a"
            , "foo" </> "a" </> ".gitignore"
            , "foo" </> "a" </> "b"
            , "foo" </> "a" </> "b" </> ".gitignore"
            , "foo" </> "a" </> "b" </> "test-b.txt"
            , "foo" </> "a" </> "b" </> "test-b.xml"
            , "foo" </> "a" </> "test-a.txt"
            , "foo" </> "a" </> "test-a.xml"
            ]
      repo   <- scanRepo @TestRepo testRepoRoot
      result <- walkRepo repo fn
      L.sort result `shouldBe` L.sort expected


data TestRepo = TestRepo
  { trPath :: FilePath
  }
  deriving (Eq, Show)

instance Repo TestRepo where
  repoName = const "TestRepo"
  repoRoot TestRepo {..} = trPath
  scanRepo path | path == testRepoRoot = pure TestRepo { trPath = path }
                | otherwise            = throwM $ InvalidRepo path "err"
  isIgnored _ path = pure $ "excluded.txt" `L.isSuffixOf` path


testRepoRoot :: FilePath
testRepoRoot = "test-data" </> "fake-git-repo"
