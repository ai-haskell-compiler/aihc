module Data.VCS.Ignore.FileSystemSpec
  ( spec
  )
where

import qualified Data.List                     as L
import           Data.Maybe                     ( catMaybes )
import           Data.VCS.Ignore.FileSystem
import           System.FilePath                ( (</>) )
import           Test.Hspec


spec :: Spec
spec = do
  describe "findPaths" $ do
    it "recursively finds paths filtered by given predicate" $ do
      let expected =
            ["test-data" </> "list-files" </> "dir1" </> "dir2" </> "d.xml"]
          predicate = pure <$> ("d.xml" `L.isSuffixOf`)
      result <- findPaths ("test-data" </> "list-files") predicate
      result `shouldBe` expected


  describe "listPaths" $ do
    it "returns empty list if path doesn't exist" $ do
      filePaths <- listPaths "non-existing-path"
      filePaths `shouldBe` []

    it "recursively finds all paths in directory" $ do
      result <- listPaths $ "test-data" </> "list-files"
      let expected =
            [ "test-data" </> "list-files"
            , "test-data" </> "list-files" </> "a.txt"
            , "test-data" </> "list-files" </> "dir1"
            , "test-data" </> "list-files" </> "dir1" </> "b.txt"
            , "test-data" </> "list-files" </> "dir1" </> "dir2"
            , "test-data" </> "list-files" </> "dir1" </> "dir2" </> "c.txt"
            , "test-data" </> "list-files" </> "dir1" </> "dir2" </> "d.xml"
            ]
      L.sort result `shouldBe` L.sort expected


  describe "walkPaths" $ do
    it "recursively traverses and processes paths in directory" $ do
      let
        fn path =
          if ".txt" `L.isSuffixOf` path then pure $ Just path else pure Nothing
        expected =
          [ "test-data" </> "list-files" </> "a.txt"
          , "test-data" </> "list-files" </> "dir1" </> "b.txt"
          , "test-data" </> "list-files" </> "dir1" </> "dir2" </> "c.txt"
          ]
      actual <- catMaybes <$> walkPaths ("test-data" </> "list-files") fn
      L.sort actual `shouldBe` L.sort expected


  describe "toPosixPath" $ do
    it "replaces any backward slashes with forward ones" $ do
      let sample   = "foo\\bar\\x.txt"
          expected = "foo/bar/x.txt"
      toPosixPath sample `shouldBe` expected

    it "keep forwars slashes as is" $ do
      let sample = "foo/bar/x.txt"
      toPosixPath sample `shouldBe` sample
