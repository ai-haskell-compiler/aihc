module Lentil.FileSpec where

import Test.Hspec

import Lentil.File

-- File tests

main :: IO ()
main = hspec spec

fld :: String
fld = "test/test-files/test-proj/"

bas :: String
bas = fld ++ "base-case/"

spec :: Spec
spec = do

  describe "findIssues" $ do
    it "scans directories to find issues" $
      findIssues [] [] [bas ++ ""] [] >>= \fiss ->
      length fiss `shouldBe` 5
    it "allows to add specific folders" $
      findIssues [] [] [bas ++ ""] [] >>= \fiss ->
      findIssues [] [] [bas ++ "fold-a/", bas ++ "fold-b",
                        bas ++ "fold-c"] [] >>= \fiss2 ->
      length fiss `shouldBe` length fiss2
    it "accepts filenames instead of dirs, too" $
      findIssues [] [] [bas ++ "fold-a/foo1.hs"] [] >>= \fiss ->
      length fiss `shouldBe` 1

-- TODO do not allow duplicate issues [bug]
--     it "doesn't duplicate issues" $
--       findIssues [bas ++ "", bas ++ ""] [] >>= \fiss ->
--       length fiss `shouldBe` 5

    -- TODO: excluding files fails on MS Windows! Check why! [u:3] [bug]
    it "excludes some folders" $
      findIssues [] [] [bas ++ ""] [bas ++ "fold-c/"] >>= \fiss ->
      length fiss `shouldBe` 2
    it "excludes files" $
      findIssues [] [] [bas ++ ""] [bas ++ "fold-a/foo1.hs"] >>= \fiss ->
      length fiss `shouldBe` 4
    it "excludes subfolders" $
      findIssues [] [] [bas ++ "fold-c/"]
                 [bas ++ "fold-c/sub-fold/"] >>= \fiss ->
      length fiss `shouldBe` 1
    it "exludes . and _ folders" $
      findIssues [] [] [fld ++ "dot-folders/"] [] >>= \fiss ->
      length fiss `shouldBe` 1
    it "doesn't crash on wrong file" $
      findIssues [] [] [bas ++ "zxczd"] [] >>= \fiss ->
      length fiss `shouldBe` 0
    it "does search outside $PWD" $
      findIssues [] [] [bas] []                    >>= \fiss  ->
      findIssues [] [] [bas ++ "../base-case/"] [] >>= \fiss' ->
      length fiss `shouldBe` length fiss'
    it "does recognised capitalised ext names" $
      findIssues [] [] [fld ++ "upper/"] [] >>= \fiss ->
      length fiss `shouldBe` 3
    it "does recognised capitalised ext names in aliases" $
      findIssues [(".f", ".c")] [] [fld ++ "upper/"] [] >>= \fiss ->
      length fiss `shouldBe` 4

