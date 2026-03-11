{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module API.BoardsTests (boardsTests) where

import API.Common
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import Test.Tasty

boardsTests :: [TestTree]
boardsTests =
  concat
    [ let fname = "data/api/boards/list-project.json"
       in gitlabJsonParserTests
            "list-project"
            fname
            (parseOne =<< BSL.readFile fname :: IO [IssueBoard])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [IssueBoard]
                parseOne (encode decodedFile) :: IO [IssueBoard]
            ),
      let fname = "data/api/boards/create-board.json"
       in gitlabJsonParserTests
            "create-board"
            fname
            (parseOne =<< BSL.readFile fname :: IO IssueBoard)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO IssueBoard
                parseOne (encode decodedFile) :: IO IssueBoard
            ),
      let fname = "data/api/boards/update-board.json"
       in gitlabJsonParserTests
            "update-board"
            fname
            (parseOne =<< BSL.readFile fname :: IO IssueBoard)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO IssueBoard
                parseOne (encode decodedFile) :: IO IssueBoard
            )
    ]
