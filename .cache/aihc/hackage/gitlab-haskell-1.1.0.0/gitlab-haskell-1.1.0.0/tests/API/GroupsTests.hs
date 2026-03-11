{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module API.GroupsTests (groupsTests) where

import API.Common
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import Test.Tasty

-- | https://docs.gitlab.com/ee/api/groups.html
groupsTests :: [TestTree]
groupsTests =
  concat
    [ let fname = "data/api/groups/list-groups.json"
       in gitlabJsonParserTests
            "list-groups"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Group])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Group]
                parseOne (encode decodedFile) :: IO [Group]
            ),
      let fname = "data/api/groups/list-group-shared-projects.json"
       in gitlabJsonParserTests
            "list-group-shared-projects"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Project])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Project]
                parseOne (encode decodedFile) :: IO [Project]
            ),
      let fname = "data/api/groups/update-group.json"
       in gitlabJsonParserTests
            "update-group"
            fname
            (parseOne =<< BSL.readFile fname :: IO Group)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Group
                parseOne (encode decodedFile) :: IO Group
            ),
      let fname = "data/api/groups/list-group-subgroups.json"
       in gitlabJsonParserTests
            "list-group-subgroups"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Group])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Group]
                parseOne (encode decodedFile) :: IO [Group]
            ),
      let fname = "data/api/groups/list-group-projects.json"
       in gitlabJsonParserTests
            "list-group-projects"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Project])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Project]
                parseOne (encode decodedFile) :: IO [Project]
            ),
      let fname = "data/api/groups/list-groups-with-stats.json"
       in gitlabJsonParserTests
            "list-groups-with-stats"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Group])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Group]
                parseOne (encode decodedFile) :: IO [Group]
            ),
      let fname = "data/api/groups/list-group-descendant-groups.json"
       in gitlabJsonParserTests
            "list-group-descendant-groups"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Group])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Group]
                parseOne (encode decodedFile) :: IO [Group]
            ),
      let fname = "data/api/groups/details-of-group.json"
       in gitlabJsonParserTests
            "details-of-group"
            fname
            (parseOne =<< BSL.readFile fname :: IO Group)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Group
                parseOne (encode decodedFile) :: IO Group
            ),
      let fname = "data/api/groups/search-for-group.json"
       in gitlabJsonParserTests
            "search-for-group"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Group])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Group]
                parseOne (encode decodedFile) :: IO [Group]
            ),
      let fname = "data/api/groups/details-of-group-with-projects-false.json"
       in gitlabJsonParserTests
            "details-of-group-with-projects-false"
            fname
            (parseOne =<< BSL.readFile fname :: IO Group)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Group
                parseOne (encode decodedFile) :: IO Group
            )
    ]

{-
Not tested yet:

add-group-push-rule.json
get-group-hook.json
get-group-push-rules.json
edit-group-push-rule.json

-}
