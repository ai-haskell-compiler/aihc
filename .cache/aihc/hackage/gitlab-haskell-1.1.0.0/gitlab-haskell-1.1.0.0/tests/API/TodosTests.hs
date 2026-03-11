{-# LANGUAGE FlexibleInstances #-}

module API.TodosTests (todosTests) where

import API.Common
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import Test.Tasty

-- | https://docs.gitlab.com/ee/api/todos.html
todosTests :: [TestTree]
todosTests =
  ( let fname = "data/api/todos/todo-items.json"
     in gitlabJsonParserTests
          "todo-items"
          fname
          (parseOne =<< BSL.readFile fname :: IO [Todo])
          ( do
              decodedFile <- parseOne =<< BSL.readFile fname :: IO [Todo]
              parseOne (encode decodedFile) :: IO [Todo]
          )
  )
    ++ ( let fname = "data/api/todos/mark-todo-item-done.json"
          in gitlabJsonParserTests
               "mark-todo-item-done"
               fname
               (parseOne =<< BSL.readFile fname :: IO Todo)
               ( do
                   decodedFile <- parseOne =<< BSL.readFile fname :: IO Todo
                   parseOne (encode decodedFile) :: IO Todo
               )
       )
