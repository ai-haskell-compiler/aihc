{-# LANGUAGE FlexibleInstances #-}

module API.IssuesTests (issuesTests) where

import API.Common
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import Test.Tasty

-- | https://docs.gitlab.com/ee/api/issues.html
issuesTests :: [TestTree]
issuesTests =
  concat
    [ let fname = "data/api/issues/list-project-issues.json"
       in gitlabJsonParserTests
            "list-project-issues"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Issue])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Issue]
                parseOne (encode decodedFile) :: IO [Issue]
            ),
      let fname = "data/api/issues/add-time-spent-for-issue.json"
       in gitlabJsonParserTests
            "add-time-spent-for-issue"
            fname
            (parseOne =<< BSL.readFile fname :: IO TimeStats)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO TimeStats
                parseOne (encode decodedFile) :: IO TimeStats
            ),
      let fname = "data/api/issues/reset-time-spent-for-issue.json"
       in gitlabJsonParserTests
            "reset-time-spent-for-issue"
            fname
            (parseOne =<< BSL.readFile fname :: IO TimeStats)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO TimeStats
                parseOne (encode decodedFile) :: IO TimeStats
            ),
      let fname = "data/api/issues/clone-issue.json"
       in gitlabJsonParserTests
            "clone-issue"
            fname
            (parseOne =<< BSL.readFile fname :: IO Issue)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Issue
                parseOne (encode decodedFile) :: IO Issue
            ),
      let fname = "data/api/issues/merge-requests-close-issue-on-merge.json"
       in gitlabJsonParserTests
            "merge-requests-close-issue-on-merge"
            fname
            (parseOne =<< BSL.readFile fname :: IO [MergeRequest])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [MergeRequest]
                parseOne (encode decodedFile) :: IO [MergeRequest]
            ),
      let fname = "data/api/issues/set-time-estimate-for-issue.json"
       in gitlabJsonParserTests
            "set-time-estimate-for-issue"
            fname
            (parseOne =<< BSL.readFile fname :: IO TimeEstimate)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO TimeEstimate
                parseOne (encode decodedFile) :: IO TimeEstimate
            ),
      let fname = "data/api/issues/reset-time-estimate-for-issue.json"
       in gitlabJsonParserTests
            "reset-time-estimate-for-issue"
            fname
            (parseOne =<< BSL.readFile fname :: IO TimeEstimate)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO TimeEstimate
                parseOne (encode decodedFile) :: IO TimeEstimate
            ),
      let fname = "data/api/issues/create-todo-item.json"
       in gitlabJsonParserTests
            "create-todo-item"
            fname
            (parseOne =<< BSL.readFile fname :: IO Todo)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Todo
                parseOne (encode decodedFile) :: IO Todo
            ),
      let fname = "data/api/issues/merge-requests-related-to-issue.json"
       in gitlabJsonParserTests
            "merge-requests-related-to-issue"
            fname
            (parseOne =<< BSL.readFile fname :: IO [MergeRequest])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [MergeRequest]
                parseOne (encode decodedFile) :: IO [MergeRequest]
            ),
      let fname = "data/api/issues/single-issue.json"
       in gitlabJsonParserTests
            "single-issue"
            fname
            (parseOne =<< BSL.readFile fname :: IO Issue)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Issue
                parseOne (encode decodedFile) :: IO Issue
            ),
      let fname = "data/api/issues/edit-issue.json"
       in gitlabJsonParserTests
            "edit-issue"
            fname
            (parseOne =<< BSL.readFile fname :: IO Issue)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Issue
                parseOne (encode decodedFile) :: IO Issue
            ),
      let fname = "data/api/issues/move-issue.json"
       in gitlabJsonParserTests
            "move-issue"
            fname
            (parseOne =<< BSL.readFile fname :: IO Issue)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Issue
                parseOne (encode decodedFile) :: IO Issue
            ),
      let fname = "data/api/issues/single-project-issue.json"
       in gitlabJsonParserTests
            "single-project-issue"
            fname
            (parseOne =<< BSL.readFile fname :: IO Issue)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Issue
                parseOne (encode decodedFile) :: IO Issue
            ),
      let fname = "data/api/issues/new-issue.json"
       in gitlabJsonParserTests
            "new-issue"
            fname
            (parseOne =<< BSL.readFile fname :: IO Issue)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Issue
                parseOne (encode decodedFile) :: IO Issue
            ),
      let fname = "data/api/issues/subscribe-to-issue.json"
       in gitlabJsonParserTests
            "subscribe-to-issue"
            fname
            (parseOne =<< BSL.readFile fname :: IO Issue)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Issue
                parseOne (encode decodedFile) :: IO Issue
            ),
      let fname = "data/api/issues/list-group-issues.json"
       in gitlabJsonParserTests
            "list-group-issues"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Issue])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Issue]
                parseOne (encode decodedFile) :: IO [Issue]
            ),
      let fname = "data/api/issues/participants-on-issues.json"
       in gitlabJsonParserTests
            "participants-on-issues"
            fname
            (parseOne =<< BSL.readFile fname :: IO [User])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [User]
                parseOne (encode decodedFile) :: IO [User]
            ),
      let fname = "data/api/issues/unsubscribe-from-issue.json"
       in gitlabJsonParserTests
            "unsubscribe-from-issue"
            fname
            (parseOne =<< BSL.readFile fname :: IO Issue)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Issue
                parseOne (encode decodedFile) :: IO Issue
            ),
      let fname = "data/api/issues/list-issues.json"
       in gitlabJsonParserTests
            "list-issues"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Issue])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Issue]
                parseOne (encode decodedFile) :: IO [Issue]
            ),
      let fname = "data/api/issues/promote-issue-to-epic.json"
       in gitlabJsonParserTests
            "promote-issue-to-epic"
            fname
            (parseOne =<< BSL.readFile fname :: IO Note)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Note
                parseOne (encode decodedFile) :: IO Note
            ),
      let fname = "data/api/issues/get-time-tracking-stats.json"
       in gitlabJsonParserTests
            "get-time-tracking-stats"
            fname
            (parseOne =<< BSL.readFile fname :: IO TimeEstimate)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO TimeEstimate
                parseOne (encode decodedFile) :: IO TimeEstimate
            )
    ]

{-

upload-metric-image.json
list-metric-images.json
user-agent-details.json

-}
