{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module API.BranchesTests (branchesTests) where

import API.Common
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import Test.Tasty

-- | https://docs.gitlab.com/ee/api/branches.html
branchesTests :: [TestTree]
branchesTests =
  concat
    [ let fname = "data/api/branches/list-repository-branches.json"
       in gitlabJsonParserTests
            "list-repository-branches"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Branch])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Branch]
                parseOne (encode decodedFile) :: IO [Branch]
            ),
      let fname = "data/api/branches/get-single-repository-branch.json"
       in gitlabJsonParserTests
            "get-single-repository-branch"
            fname
            (parseOne =<< BSL.readFile fname :: IO Branch)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Branch
                parseOne (encode decodedFile) :: IO Branch
            ),
      let fname = "data/api/branches/create-repository-branch.json"
       in gitlabJsonParserTests
            "create-repository-branch"
            fname
            (parseOne =<< BSL.readFile fname :: IO Branch)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Branch
                parseOne (encode decodedFile) :: IO Branch
            )
    ]
