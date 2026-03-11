{-# LANGUAGE FlexibleInstances #-}

module API.EventsTests (eventsTests) where

import API.Common
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import Test.Tasty

-- | https://docs.gitlab.com/ee/api/events.html
eventsTests :: [TestTree]
eventsTests =
  concat
    [ let fname = "data/api/events/get-user-contributions-events.json"
       in gitlabJsonParserTests
            "get-user-contributions-events"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Event])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Event]
                parseOne (encode decodedFile) :: IO [Event]
            ),
      let fname = "data/api/events/list-current-authenticated-users-events.json"
       in gitlabJsonParserTests
            "list-current-authenticated-users-events"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Event])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Event]
                parseOne (encode decodedFile) :: IO [Event]
            ),
      let fname = "data/api/events/list-projects-visible-events.json"
       in gitlabJsonParserTests
            "list-projects-visible-events"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Event])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Event]
                parseOne (encode decodedFile) :: IO [Event]
            )
    ]
