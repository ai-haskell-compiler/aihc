{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module API.CommitsTests (commitsTests) where

import API.Common
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import Test.Tasty

-- | https://docs.gitlab.com/ee/api/commits.html
commitsTests :: [TestTree]
commitsTests =
  concat
    [ let fname = "data/api/commits/list-repository-commits.json"
       in gitlabJsonParserTests
            "list-repository-commits"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Commit])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Commit]
                parseOne (encode decodedFile) :: IO [Commit]
            ),
      let fname = "data/api/commits/create-commit-multiple-files-commits.json"
       in gitlabJsonParserTests
            "create-commit-multiple-files-commits"
            fname
            (parseOne =<< BSL.readFile fname :: IO Commit)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Commit
                parseOne (encode decodedFile) :: IO Commit
            ),
      let fname = "data/api/commits/cherry-pick-commit.json"
       in gitlabJsonParserTests
            "cherry-pick-commit"
            fname
            (parseOne =<< BSL.readFile fname :: IO Commit)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Commit
                parseOne (encode decodedFile) :: IO Commit
            ),
      let fname = "data/api/commits/revert-commit.json"
       in gitlabJsonParserTests
            "revert-commit"
            fname
            (parseOne =<< BSL.readFile fname :: IO Commit)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Commit
                parseOne (encode decodedFile) :: IO Commit
            ),
      let fname = "data/api/commits/get-diff-of-commit.json"
       in gitlabJsonParserTests
            "get-diff-of-commit"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Diff])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Diff]
                parseOne (encode decodedFile) :: IO [Diff]
            ),
      let fname = "data/api/commits/get-comments-of-commit.json"
       in gitlabJsonParserTests
            "get-comments-of-commit"
            fname
            (parseOne =<< BSL.readFile fname :: IO [CommitNote])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [CommitNote]
                parseOne (encode decodedFile) :: IO [CommitNote]
            )
    ]
