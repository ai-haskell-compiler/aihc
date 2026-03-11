{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module API.DiscussionsTests (discussionsTests) where

import API.Common
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import Test.Tasty

-- | https://docs.gitlab.com/ee/api/discussions.html
discussionsTests :: [TestTree]
discussionsTests =
  concat
    [ let fname = "data/api/discussions/list-group-epic-discussion-items.json"
       in gitlabJsonParserTests
            "list-group-epic-discussion-items"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Discussion])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Discussion]
                parseOne (encode decodedFile) :: IO [Discussion]
            ),
      let fname = "data/api/discussions/list-project-merge-request-discussion-items.json"
       in gitlabJsonParserTests
            "list-project-merge-request-discussion-items"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Discussion])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Discussion]
                parseOne (encode decodedFile) :: IO [Discussion]
            ),
      let fname = "data/api/discussions/list-project-commit-discussion-items.json"
       in gitlabJsonParserTests
            "list-project-commit-discussion-items"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Discussion])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Discussion]
                parseOne (encode decodedFile) :: IO [Discussion]
            ),
      let fname = "data/api/discussions/list-project-snippet-discussion-items.json"
       in gitlabJsonParserTests
            "list-project-snippet-discussion-items"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Discussion])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Discussion]
                parseOne (encode decodedFile) :: IO [Discussion]
            ),
      let fname = "data/api/discussions/list-project-discussion-items.json"
       in gitlabJsonParserTests
            "list-project-discussion-items"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Discussion])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Discussion]
                parseOne (encode decodedFile) :: IO [Discussion]
            )
    ]
