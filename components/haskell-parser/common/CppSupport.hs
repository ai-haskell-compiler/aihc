{-# LANGUAGE OverloadedStrings #-}

module CppSupport
  ( preprocessForParser,
    preprocessForParserWithoutIncludes,
  )
where

import Cpp
  ( Config (..),
    IncludeRequest,
    Result (..),
    Step (..),
    preprocess,
  )
import Data.Text (Text)
import qualified Data.Text as T

preprocessForParser :: FilePath -> (IncludeRequest -> IO (Maybe Text)) -> Text -> IO Result
preprocessForParser inputFile resolveInclude source = do
  result <- drive (preprocess Config {configInputFile = inputFile} source)
  pure result {resultOutput = stripLinePragmas (resultOutput result)}
  where
    drive (Done result) = pure result
    drive (NeedInclude req k) = resolveInclude req >>= drive . k

preprocessForParserWithoutIncludes :: FilePath -> Text -> IO Result
preprocessForParserWithoutIncludes inputFile =
  preprocessForParser inputFile (\_ -> pure Nothing)

stripLinePragmas :: Text -> Text
stripLinePragmas =
  T.unlines
    . filter (not . isLinePragma)
    . T.lines
  where
    isLinePragma line = "#line " `T.isPrefixOf` T.stripStart line
