{-# LANGUAGE FlexibleInstances #-}

module API.NotesTests (notesTests) where

import API.Common
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import Test.Tasty

-- | https://docs.gitlab.com/ee/api/notes.html
notesTests :: [TestTree]
notesTests =
  concat
    [ let fname = "data/api/notes/project-issue-notes.json"
       in gitlabJsonParserTests
            "project-issue-notes"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Note])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Note]
                parseOne (encode decodedFile) :: IO [Note]
            ),
      let fname = "data/api/notes/single-merge-request-note.json"
       in gitlabJsonParserTests
            "single-merge-request-note"
            fname
            (parseOne =<< BSL.readFile fname :: IO Note)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Note
                parseOne (encode decodedFile) :: IO Note
            ),
      let fname = "data/api/notes/single-snippet-note.json"
       in gitlabJsonParserTests
            "single-snippet-note"
            fname
            (parseOne =<< BSL.readFile fname :: IO Note)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Note
                parseOne (encode decodedFile) :: IO Note
            )
    ]
