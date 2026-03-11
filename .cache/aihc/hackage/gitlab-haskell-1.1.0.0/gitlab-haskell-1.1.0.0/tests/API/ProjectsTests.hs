{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module API.ProjectsTests (projectsTests) where

import API.Common
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import Test.Tasty

-- | https://docs.gitlab.com/ee/api/projects.html
projectsTests :: [TestTree]
projectsTests =
  concat
    [ let fname = "data/api/projects/list-all-projects.json"
       in gitlabJsonParserTests
            "list-all-projects"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Project])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Project]
                parseOne (encode decodedFile) :: IO [Project]
            ),
      let fname = "data/api/projects/archive-project.json"
       in gitlabJsonParserTests
            "archive-project"
            fname
            (parseOne =<< BSL.readFile fname :: IO Project)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Project
                parseOne (encode decodedFile) :: IO Project
            ),
      let fname = "data/api/projects/get-single-project.json"
       in gitlabJsonParserTests
            "get-single-project"
            fname
            (parseOne =<< BSL.readFile fname :: IO Project)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Project
                parseOne (encode decodedFile) :: IO Project
            ),
      let fname = "data/api/projects/list-projects-starred-by-user.json"
       in gitlabJsonParserTests
            "list-projects-starred-by-user"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Project])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Project]
                parseOne (encode decodedFile) :: IO [Project]
            ),
      let fname = "data/api/projects/unarchive-project.json"
       in gitlabJsonParserTests
            "unarchive-project"
            fname
            (parseOne =<< BSL.readFile fname :: IO Project)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Project
                parseOne (encode decodedFile) :: IO Project
            ),
      let fname = "data/api/projects/get-path-to-repository-storage.json"
       in gitlabJsonParserTests
            "get-path-to-repository-storage"
            fname
            (parseOne =<< BSL.readFile fname :: IO [RepositoryStorage])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [RepositoryStorage]
                parseOne (encode decodedFile) :: IO [RepositoryStorage]
            ),
      let fname = "data/api/projects/list-user-projects.json"
       in gitlabJsonParserTests
            "list-user-projects"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Project])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Project]
                parseOne (encode decodedFile) :: IO [Project]
            ),
      let fname = "data/api/projects/unstar-project.json"
       in gitlabJsonParserTests
            "unstar-project"
            fname
            (parseOne =<< BSL.readFile fname :: IO Project)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Project
                parseOne (encode decodedFile) :: IO Project
            ),
      let fname = "data/api/projects/star-project.json"
       in gitlabJsonParserTests
            "star-project"
            fname
            (parseOne =<< BSL.readFile fname :: IO Project)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Project
                parseOne (encode decodedFile) :: IO Project
            ),
      let fname = "data/api/projects/star-project.json"
       in gitlabJsonParserTests
            "star-project"
            fname
            (parseOne =<< BSL.readFile fname :: IO Project)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Project
                parseOne (encode decodedFile) :: IO Project
            ),
      let fname = "data/api/projects/list-forks-of-project.json"
       in gitlabJsonParserTests
            "list-forks-of-project"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Project])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Project]
                parseOne (encode decodedFile) :: IO [Project]
            ),
      let fname = "data/api/projects/starrers-of-project.json"
       in gitlabJsonParserTests
            "starrers-of-project"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Starrer])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Starrer]
                parseOne (encode decodedFile) :: IO [Starrer]
            ),
      let fname = "data/api/projects/upload-project-avatar.json"
       in gitlabJsonParserTests
            "upload-project-avatar"
            fname
            (parseOne =<< BSL.readFile fname :: IO ProjectAvatar)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO ProjectAvatar
                parseOne (encode decodedFile) :: IO ProjectAvatar
            ),
      let fname = "data/api/projects/get-project-users.json"
       in gitlabJsonParserTests
            "get-project-users"
            fname
            (parseOne =<< BSL.readFile fname :: IO [User])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [User]
                parseOne (encode decodedFile) :: IO [User]
            ),
      let fname = "data/api/projects/list-project-groups.json"
       in gitlabJsonParserTests
            "list-project-groups"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Group])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Group]
                parseOne (encode decodedFile) :: IO [Group]
            ),
      let fname = "data/api/projects/transfer-project-new-namespace.json"
       in gitlabJsonParserTests
            "transfer-project-new-namespace"
            fname
            (parseOne =<< BSL.readFile fname :: IO Project)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Project
                parseOne (encode decodedFile) :: IO Project
            )
    ]

{-

Untested data files:

languages.json
get-project-hook.json
upload-file.json
get-project-push-rules.json

-}
