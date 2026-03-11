{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module API.MergeRequestsTests (mergeRequestsTests) where

import API.Common
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import Test.Tasty

-- | https://docs.gitlab.com/ee/api/merge_requests.html
mergeRequestsTests :: [TestTree]
mergeRequestsTests =
  concat
    [ let fname = "data/api/merge-requests/accept-merge-request.json"
       in gitlabJsonParserTests
            "accept-merge-request"
            fname
            (parseOne =<< BSL.readFile fname :: IO MergeRequest)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO MergeRequest
                parseOne (encode decodedFile) :: IO MergeRequest
            ),
      let fname = "data/api/merge-requests/add-spent-time-merge-request.json"
       in gitlabJsonParserTests
            "add-spent-time-merge-request"
            fname
            (parseOne =<< BSL.readFile fname :: IO TimeEstimate)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO TimeEstimate
                parseOne (encode decodedFile) :: IO TimeEstimate
            ),
      let fname = "data/api/merge-requests/cancel-merge-request-when-pipeline-succeeds.json"
       in gitlabJsonParserTests
            "cancel-merge-request-when-pipeline-succeeds"
            fname
            (parseOne =<< BSL.readFile fname :: IO MergeRequest)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO MergeRequest
                parseOne (encode decodedFile) :: IO MergeRequest
            ),
      let fname = "data/api/merge-requests/create-merge-request.json"
       in gitlabJsonParserTests
            "create-merge-request"
            fname
            (parseOne =<< BSL.readFile fname :: IO MergeRequest)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO MergeRequest
                parseOne (encode decodedFile) :: IO MergeRequest
            ),
      let fname = "data/api/merge-requests/create-merge-request-pipeline.json"
       in gitlabJsonParserTests
            "create-merge-request-pipeline"
            fname
            (parseOne =<< BSL.readFile fname :: IO Pipeline)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Pipeline
                parseOne (encode decodedFile) :: IO Pipeline
            ),
      let fname = "data/api/merge-requests/create-todo-item.json"
       in gitlabJsonParserTests
            "create-todo-item"
            fname
            (parseOne =<< BSL.readFile fname :: IO Todo)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Todo
                parseOne (encode decodedFile) :: IO Todo
            ),
      let fname = "data/api/merge-requests/list-group-merge-requests.json"
       in gitlabJsonParserTests
            "list-group-merge-requests"
            fname
            (parseOne =<< BSL.readFile fname :: IO [MergeRequest])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [MergeRequest]
                parseOne (encode decodedFile) :: IO [MergeRequest]
            ),
      let fname = "data/api/merge-requests/list-merge-request-pipelines.json"
       in gitlabJsonParserTests
            "list-merge-request-pipelines"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Pipeline])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Pipeline]
                parseOne (encode decodedFile) :: IO [Pipeline]
            ),
      let fname = "data/api/merge-requests/list-merge-requests.json"
       in gitlabJsonParserTests
            "list-merge-requests"
            fname
            (parseOne =<< BSL.readFile fname :: IO [MergeRequest])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [MergeRequest]
                parseOne (encode decodedFile) :: IO [MergeRequest]
            ),
      let fname = "data/api/merge-requests/list-project-merge-requests.json"
       in gitlabJsonParserTests
            "list-project-merge-requests"
            fname
            (parseOne =<< BSL.readFile fname :: IO [MergeRequest])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [MergeRequest]
                parseOne (encode decodedFile) :: IO [MergeRequest]
            ),
      let fname = "data/api/merge-requests/list-project-merge-requests.json"
       in gitlabJsonParserTests
            "list-project-merge-requests"
            fname
            (parseOne =<< BSL.readFile fname :: IO [MergeRequest])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [MergeRequest]
                parseOne (encode decodedFile) :: IO [MergeRequest]
            ),
      let fname = "data/api/merge-requests/reset-time-estimate-merge-request.json"
       in gitlabJsonParserTests
            "reset-time-estimate-merge-request"
            fname
            (parseOne =<< BSL.readFile fname :: IO TimeEstimate)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO TimeEstimate
                parseOne (encode decodedFile) :: IO TimeEstimate
            ),
      let fname = "data/api/merge-requests/reset-time-merge-request.json"
       in gitlabJsonParserTests
            "reset-time-merge-request"
            fname
            (parseOne =<< BSL.readFile fname :: IO TimeEstimate)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO TimeEstimate
                parseOne (encode decodedFile) :: IO TimeEstimate
            ),
      let fname = "data/api/merge-requests/single-merge-request-changes.json"
       in gitlabJsonParserTests
            "single-merge-request-changes"
            fname
            (parseOne =<< BSL.readFile fname :: IO MergeRequest)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO MergeRequest
                parseOne (encode decodedFile) :: IO MergeRequest
            ),
      let fname = "data/api/merge-requests/single-merge-request-participants.json"
       in gitlabJsonParserTests
            "single-merge-request-participants"
            fname
            (parseOne =<< BSL.readFile fname :: IO [User])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [User]
                parseOne (encode decodedFile) :: IO [User]
            ),
      let fname = "data/api/merge-requests/subscribe-merge-request.json"
       in gitlabJsonParserTests
            "subscribe-merge-request"
            fname
            (parseOne =<< BSL.readFile fname :: IO MergeRequest)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO MergeRequest
                parseOne (encode decodedFile) :: IO MergeRequest
            ),
      let fname = "data/api/merge-requests/time-estimate-merge-request.json"
       in gitlabJsonParserTests
            "time-estimate-merge-request"
            fname
            (parseOne =<< BSL.readFile fname :: IO TimeEstimate)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO TimeEstimate
                parseOne (encode decodedFile) :: IO TimeEstimate
            ),
      let fname = "data/api/merge-requests/time-tracking-stats.json"
       in gitlabJsonParserTests
            "time-tracking-stats"
            fname
            (parseOne =<< BSL.readFile fname :: IO TimeEstimate)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO TimeEstimate
                parseOne (encode decodedFile) :: IO TimeEstimate
            ),
      let fname = "data/api/merge-requests/unsubscribe-merge-request.json"
       in gitlabJsonParserTests
            "unsubscribe-merge-request"
            fname
            (parseOne =<< BSL.readFile fname :: IO MergeRequest)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO MergeRequest
                parseOne (encode decodedFile) :: IO MergeRequest
            ),
      let fname = "data/api/merge-requests/update-merge-request.json"
       in gitlabJsonParserTests
            "update-merge-request"
            fname
            (parseOne =<< BSL.readFile fname :: IO MergeRequest)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO MergeRequest
                parseOne (encode decodedFile) :: IO MergeRequest
            )
            -- let fname = "data/api/merge-requests/single-merge-request.json"
            --  in gitlabJsonParserTests
            --       "single-merge-request"
            --       fname
            --       (parseOne =<< BSL.readFile fname :: IO MergeRequest)
            --       ( do
            --           decodedFile <- parseOne =<< BSL.readFile fname :: IO MergeRequest
            --           parseOne (encode decodedFile) :: IO MergeRequest
            --       )
    ]

{- untested -}

-- -- testCase
-- --   "merge-request-comments-on-merge-requests"
-- --   ( gitlabParseTestMany
-- --       undefined
-- --       "data/api/merge-requests/comments-on-merge-requests.json"
-- --   ),

-- -- testCase
-- --   "merge-request-get-merge-request-diff-versions"
-- --   ( gitlabParseTestMany
-- --       undefined
-- --       "data/api/merge-requests/get-merge-request-diff-versions.json"
-- --   ),

-- -- testCase
-- --   "merge-request-merge-to-default-merge-path"
-- --   ( gitlabParseTestMany
-- --       undefined
-- --       "data/api/merge-requests/merge-to-default-merge-path.json"
-- --   ),

-- -- testCase
-- --   "merge-request-rebase-merge-request"
-- --   ( gitlabParseTestMany
-- --       undefined
-- --       "data/api/merge-requests/rebase-merge-request.json"
-- --   ),

-- -- testCase
-- --   "merge-request-single-merge-request-commits"
-- --   ( gitlabParseTestMany
-- --       undefined
-- --       "data/api/merge-requests/single-merge-request-commits.json"
-- --   ),

-- -- testCase
-- --   "merge-request-single-merge-request-diff-version"
-- --   ( gitlabParseTestMany
-- --       undefined
-- --       "data/api/merge-requests/single-merge-request-diff-version.json"
-- --   ),
