{-# LANGUAGE OverloadedStrings #-}

-- |
module Main (main) where

import Test.Hspec
import Web.RedHatBugzilla.Search

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "unit tests" $ do
  describe "search" $ do
    it "eval isNotEmpty" $ do
      evalSearchExpr (isNotEmpty (CustomField "my_field"))
        `shouldBe` [ ("f1", Just "my_field"),
                     ("o1", Just "isnotempty"),
                     ("v1", Just "")
                   ]
    it "eval changedSince" $ do
      evalSearchExpr (changedSince (read "2021-04-01 00:00:00 UTC"))
        `shouldBe` [("chfieldfrom", Just "2021-04-01T00:00:00Z")]

    it "eval changedRange" $ do
      evalSearchExpr
        ( changedRange
            (read "2021-04-01 00:00:00 UTC")
            (read "2021-04-13 00:00:00 UTC")
        )
        `shouldBe` [ -- TODO: fix unused f1 and f4 query args
                     ("f1", Just "OP"),
                     ("f4", Just "CP"),
                     ("chfieldfrom", Just "2021-04-01T00:00:00Z"),
                     ("chfieldto", Just "2021-04-13T00:00:00Z")
                   ]
