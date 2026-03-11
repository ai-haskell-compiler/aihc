{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (unpack)
import Ollama (Version (..), getVersion)
import Test.Ollama.Blob qualified as Blob
import Test.Ollama.Chat qualified as Chat
import Test.Ollama.Common qualified as Common
import Test.Ollama.Copy qualified as Copy
import Test.Ollama.Create qualified as Create
import Test.Ollama.Delete qualified as Delete
import Test.Ollama.Embedding qualified as Embeddings
import Test.Ollama.Generate qualified as Generate
import Test.Ollama.List qualified as List
import Test.Ollama.Load qualified as Load
import Test.Ollama.Show qualified as Show
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ -- Core functionality tests
      Generate.tests
    , Chat.tests
    , Embeddings.tests
    , -- Model management tests
      Show.tests
    , List.tests
    , Copy.tests
    , Create.tests
    , Delete.tests
    , Load.tests
    , -- Utility and blob tests
      Blob.tests
    , Common.tests
    ]

main :: IO ()
main = do
  eRes <- getVersion
  case eRes of
    Left err -> print err
    Right (Version r) -> do
      putStrLn $ "Ollama client version: " <> unpack r
      defaultMain tests
