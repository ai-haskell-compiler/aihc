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
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Text (Text)
import qualified Data.Text as T

preprocessForParser :: (Monad m) => FilePath -> (IncludeRequest -> m (Maybe Text)) -> Text -> m Result
preprocessForParser inputFile resolveInclude source = do
  result <- drive (preprocess Config {configInputFile = inputFile} source)
  pure result {resultOutput = stripLanguagePragmas (stripLinePragmas (resultOutput result))}
  where
    drive (Done result) = pure result
    drive (NeedInclude req k) = resolveInclude req >>= drive . k

preprocessForParserWithoutIncludes :: FilePath -> Text -> Result
preprocessForParserWithoutIncludes inputFile source =
  runIdentity (preprocessForParser inputFile (\_ -> Identity Nothing) source)

stripLinePragmas :: Text -> Text
stripLinePragmas =
  T.unlines
    . filter (not . isLinePragma)
    . T.lines
  where
    isLinePragma line = "#line " `T.isPrefixOf` T.stripStart line

stripLanguagePragmas :: Text -> Text
stripLanguagePragmas =
  T.unlines
    . filter (not . isLanguagePragma)
    . T.lines
  where
    isLanguagePragma line =
      let trimmed = T.strip line
       in "{-# LANGUAGE " `T.isPrefixOf` trimmed && "#-}" `T.isSuffixOf` trimmed
