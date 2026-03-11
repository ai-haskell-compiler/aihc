{-# LANGUAGE FlexibleInstances #-}

module API.UsersTests (usersTests) where

import API.Common
import Data.Aeson hiding (Key)
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import Test.Tasty

-- | https://docs.gitlab.com/ee/api/users.html
usersTests :: [TestTree]
usersTests =
  concat
    [ let fname = "data/api/users/emails.json"
       in gitlabJsonParserTests
            "emails"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Email])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Email]
                parseOne (encode decodedFile) :: IO [Email]
            ),
      let fname = "data/api/users/add-ssh-key.json"
       in gitlabJsonParserTests
            "add-ssh-key"
            fname
            (parseOne =<< BSL.readFile fname :: IO Key)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Key
                parseOne (encode decodedFile) :: IO Key
            ),
      let fname = "data/api/users/ssh-keys.json"
       in gitlabJsonParserTests
            "ssh-keys"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Key])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Key]
                parseOne (encode decodedFile) :: IO [Key]
            ),
      let fname = "data/api/users/ssh-key.json"
       in gitlabJsonParserTests
            "ssh-key"
            fname
            (parseOne =<< BSL.readFile fname :: IO Key)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Key
                parseOne (encode decodedFile) :: IO Key
            ),
      let fname = "data/api/users/followers.json"
       in gitlabJsonParserTests
            "followers"
            fname
            (parseOne =<< BSL.readFile fname :: IO [User])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [User]
                parseOne (encode decodedFile) :: IO [User]
            ),
      let fname = "data/api/users/follow-user.json"
       in gitlabJsonParserTests
            "follow-user"
            fname
            (parseOne =<< BSL.readFile fname :: IO User)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO User
                parseOne (encode decodedFile) :: IO User
            ),
      let fname = "data/api/users/user-preferences.json"
       in gitlabJsonParserTests
            "user-preferences"
            fname
            (parseOne =<< BSL.readFile fname :: IO UserPrefs)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO UserPrefs
                parseOne (encode decodedFile) :: IO UserPrefs
            ),
      let fname = "data/api/users/user-status.json"
       in gitlabJsonParserTests
            "user-status"
            fname
            (parseOne =<< BSL.readFile fname :: IO UserStatus)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO UserStatus
                parseOne (encode decodedFile) :: IO UserStatus
            ),
      let fname = "data/api/users/set-user-status.json"
       in gitlabJsonParserTests
            "set-user-status"
            fname
            (parseOne =<< BSL.readFile fname :: IO UserStatus)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO UserStatus
                parseOne (encode decodedFile) :: IO UserStatus
            ),
      let fname = "data/api/users/current-user.json"
       in gitlabJsonParserTests
            "current-user"
            fname
            (parseOne =<< BSL.readFile fname :: IO User)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO User
                parseOne (encode decodedFile) :: IO User
            ),
      let fname = "data/api/users/list-user.json"
       in gitlabJsonParserTests
            "list-user"
            fname
            (parseOne =<< BSL.readFile fname :: IO User)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO User
                parseOne (encode decodedFile) :: IO User
            ),
      let fname = "data/api/users/list-users.json"
       in gitlabJsonParserTests
            "list-users"
            fname
            (parseOne =<< BSL.readFile fname :: IO [User])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [User]
                parseOne (encode decodedFile) :: IO [User]
            ),
      let fname = "data/api/users/gpg-keys.json"
       in gitlabJsonParserTests
            "gpg-keys"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Key])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Key]
                parseOne (encode decodedFile) :: IO [Key]
            ),
      let fname = "data/api/users/user-counts.json"
       in gitlabJsonParserTests
            "user-counts"
            fname
            (parseOne =<< BSL.readFile fname :: IO UserCount)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO UserCount
                parseOne (encode decodedFile) :: IO UserCount
            )
    ]
