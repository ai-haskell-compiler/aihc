{-# LANGUAGE FlexibleInstances #-}

module API.MembersTests (membersTests) where

import API.Common
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import Test.Tasty

-- | https://docs.gitlab.com/ee/api/members.html
membersTests :: [TestTree]
membersTests =
  concat
    [ let fname = "data/api/members/list-pending-members.json"
       in gitlabJsonParserTests
            "list-project-repository-tags"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Member])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Member]
                parseOne (encode decodedFile) :: IO [Member]
            ),
      let fname = "data/api/members/billable-members-of-group.json"
       in gitlabJsonParserTests
            "billable-members-of-group"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Member])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Member]
                parseOne (encode decodedFile) :: IO [Member]
            ),
      let fname = "data/api/members/removed-override-flag-member-group.json"
       in gitlabJsonParserTests
            "removed-override-flag-member-group"
            fname
            (parseOne =<< BSL.readFile fname :: IO Member)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Member
                parseOne (encode decodedFile) :: IO Member
            ),
      let fname = "data/api/members/member-group-or-project-including-inherited-invited.json"
       in gitlabJsonParserTests
            "member-group-or-project-including-inherited-invited"
            fname
            (parseOne =<< BSL.readFile fname :: IO Member)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Member
                parseOne (encode decodedFile) :: IO Member
            ),
      let fname = "data/api/members/override-flag-member-group.json"
       in gitlabJsonParserTests
            "override-flag-member-group"
            fname
            (parseOne =<< BSL.readFile fname :: IO Member)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Member
                parseOne (encode decodedFile) :: IO Member
            ),
      let fname = "data/api/members/member-group-or-project.json"
       in gitlabJsonParserTests
            "member-group-or-project"
            fname
            (parseOne =<< BSL.readFile fname :: IO Member)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Member
                parseOne (encode decodedFile) :: IO Member
            ),
      let fname = "data/api/members/edit-member-group-project.json"
       in gitlabJsonParserTests
            "edit-member-group-project"
            fname
            (parseOne =<< BSL.readFile fname :: IO Member)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Member
                parseOne (encode decodedFile) :: IO Member
            ),
      let fname = "data/api/members/list-all-memembers-group-or-project-including-inherited-invited.json"
       in gitlabJsonParserTests
            "list-all-memembers-group-or-project-including-inherited-invited"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Member])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Member]
                parseOne (encode decodedFile) :: IO [Member]
            ),
      let fname = "data/api/members/add-member-group-project.json"
       in gitlabJsonParserTests
            "add-member-group-project"
            fname
            (parseOne =<< BSL.readFile fname :: IO Member)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Member
                parseOne (encode decodedFile) :: IO Member
            ),
      let fname = "data/api/members/list-all-memembers-group-or-project.json"
       in gitlabJsonParserTests
            "list-all-memembers-group-or-project"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Member])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Member]
                parseOne (encode decodedFile) :: IO [Member]
            )
            -- let fname = "data/api/members/membership-billable-member-of-group.json"
            --  in gitlabJsonParserTests
            --       "membership-billable-member-of-group"
            --       fname
            --       (parseOne =<< BSL.readFile fname :: IO Member)
            --       ( do
            --           decodedFile <- parseOne =<< BSL.readFile fname :: IO Member
            --           parseOne (encode decodedFile) :: IO Member
            --       )
    ]
