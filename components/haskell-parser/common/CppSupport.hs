{-# LANGUAGE OverloadedStrings #-}

module CppSupport
  ( preprocessForParser,
    preprocessForParserIfEnabled,
    preprocessForParserWithoutIncludes,
    preprocessForParserWithoutIncludesIfEnabled,
    moduleHeaderExtensionSettings,
    cppEnabledInSource,
  )
where

import Cpp
  ( Config (..),
    IncludeRequest,
    Result (..),
    Step (..),
    defaultConfig,
    preprocess,
  )
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Parser as P
import Parser.Ast (Extension (CPP), ExtensionSetting (..))

preprocessForParser :: (Monad m) => FilePath -> (IncludeRequest -> m (Maybe Text)) -> Text -> m Result
preprocessForParser inputFile resolveInclude source = do
  let cfg =
        defaultConfig
          { configInputFile = inputFile
          }
  result <- drive (preprocess cfg source)
  pure result {resultOutput = stripLinePragmas (resultOutput result)}
  where
    drive (Done result) = pure result
    drive (NeedInclude req k) = resolveInclude req >>= drive . k

preprocessForParserWithoutIncludes :: FilePath -> Text -> Result
preprocessForParserWithoutIncludes inputFile source =
  runIdentity (preprocessForParser inputFile (\_ -> Identity Nothing) source)

preprocessForParserIfEnabled :: (Monad m) => FilePath -> (IncludeRequest -> m (Maybe Text)) -> Text -> m Result
preprocessForParserIfEnabled inputFile resolveInclude source =
  if cppEnabledInSource source
    then preprocessForParser inputFile resolveInclude source
    else pure Result {resultOutput = source, resultDiagnostics = []}

preprocessForParserWithoutIncludesIfEnabled :: FilePath -> Text -> Result
preprocessForParserWithoutIncludesIfEnabled inputFile source =
  runIdentity (preprocessForParserIfEnabled inputFile (\_ -> Identity Nothing) source)

moduleHeaderExtensionSettings :: Text -> [ExtensionSetting]
moduleHeaderExtensionSettings = P.readModuleHeaderExtensions

cppEnabledInSource :: Text -> Bool
cppEnabledInSource = foldl apply False . moduleHeaderExtensionSettings
  where
    apply enabled setting =
      case setting of
        EnableExtension CPP -> True
        DisableExtension CPP -> False
        _ -> enabled

stripLinePragmas :: Text -> Text
stripLinePragmas =
  T.unlines
    . filter (not . isLinePragma)
    . T.lines
  where
    isLinePragma line = "#line " `T.isPrefixOf` T.stripStart line
