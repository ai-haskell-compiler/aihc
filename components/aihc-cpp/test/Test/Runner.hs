{-# LANGUAGE OverloadedStrings #-}

module Test.Runner
  ( runPreprocessFromFile,
    runPreprocessFromText,
  )
where

import Aihc.Cpp
  ( Config (..),
    IncludeRequest (..),
    Result,
    Step (..),
    preprocess,
  )
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (</>))

runPreprocessFromFile :: Config -> FilePath -> IO Result
runPreprocessFromFile cfg actualInputPath = do
  source <- BS.readFile actualInputPath
  drive initialSources (preprocess cfg source)
  where
    initialSources = M.singleton (configInputFile cfg) actualInputPath

runPreprocessFromText :: Config -> Text -> IO Result
runPreprocessFromText cfg sourceText = do
  let source = T.encodeUtf8 sourceText
      displayInputPath =
        if null (configInputFile cfg)
          then "<input>"
          else configInputFile cfg
  drive (M.singleton displayInputPath displayInputPath) (preprocess cfg source)

drive :: M.Map FilePath FilePath -> Step -> IO Result
drive _ (Done result) = pure result
drive actualPaths (NeedInclude req k) = do
  actualFrom <-
    case M.lookup (includeFrom req) actualPaths of
      Just path -> pure path
      Nothing -> fail ("Unknown include source path: " <> includeFrom req)
  let actualIncludePath = takeDirectory actualFrom </> includePath req
      displayIncludePath = takeDirectory (includeFrom req) </> includePath req
  exists <- doesFileExist actualIncludePath
  content <- if exists then Just <$> BS.readFile actualIncludePath else pure Nothing
  let actualPaths' =
        if exists
          then M.insert displayIncludePath actualIncludePath actualPaths
          else actualPaths
  drive actualPaths' (k content)
