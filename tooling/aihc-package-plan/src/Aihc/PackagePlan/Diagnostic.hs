{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Aihc.PackagePlan.Diagnostic
-- Description : Diagnostics of the parser and the preprocessor
--
-- Keeps the diagnostics that parsing and preprocessing a package produce,
-- and renders them as GHC-style messages.
module Aihc.PackagePlan.Diagnostic
  ( Diagnostic (..),
    isCppWarning,
    renderHumanDiagnostic,
    DiagnosticSourceMap,
    diagnosticSourceMap,
  )
where

import Aihc.Cpp qualified as Cpp
import Aihc.Parser.Syntax (SourceSpan, pattern SourceSpan)
import Control.DeepSeq (NFData (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.MonoidMap (MonoidMap)
import Data.MonoidMap qualified as MonoidMap
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

-- | A diagnostic from the parser or the preprocessor.
data Diagnostic
  = -- | A parse error in the file at the path, at the span.
    ParseDiagnostic !FilePath !SourceSpan !Text
  | CppDiagnostic !Cpp.Diagnostic
  deriving (Eq, Show)

instance NFData Diagnostic where
  rnf diagnostic =
    case diagnostic of
      ParseDiagnostic path span' message -> rnf path `seq` rnf span' `seq` rnf message
      CppDiagnostic cppDiagnostic -> rnf cppDiagnostic

isCppWarning :: Diagnostic -> Bool
isCppWarning diagnostic =
  case diagnostic of
    CppDiagnostic cppDiagnostic -> Cpp.diagSeverity cppDiagnostic == Cpp.Warning
    ParseDiagnostic {} -> False

-- | One line, as @FILE:LINE[:COLUMN]: SEVERITY: MESSAGE@.
renderHumanDiagnostic :: Diagnostic -> String
renderHumanDiagnostic diagnostic =
  case diagnostic of
    ParseDiagnostic path (SourceSpan _ line column _ _ _ _) message ->
      path <> ":" <> show line <> ":" <> show column <> ": error: " <> T.unpack message <> "\n"
    CppDiagnostic cppDiagnostic ->
      Cpp.diagFile cppDiagnostic
        <> ":"
        <> show (Cpp.diagLine cppDiagnostic)
        <> ": "
        <> show (Cpp.diagSeverity cppDiagnostic)
        <> ": "
        <> T.unpack (Cpp.diagMessage cppDiagnostic)
        <> "\n"

type DiagnosticSourceMap = MonoidMap FilePath (Map.Map Int Text)

diagnosticSourceMap :: FilePath -> Text -> DiagnosticSourceMap
diagnosticSourceMap initialFile =
  third . foldl' step (initialFile, 1, MonoidMap.empty) . T.lines
  where
    third (_, _, value) = value
    step (currentFile, currentLine, sourceMap) line =
      case parseLineDirective line of
        Just (nextLine, nextFile) -> (fromMaybe currentFile nextFile, nextLine, sourceMap)
        Nothing ->
          ( currentFile,
            currentLine + 1,
            MonoidMap.adjust (Map.insert currentLine line) currentFile sourceMap
          )

parseLineDirective :: Text -> Maybe (Int, Maybe FilePath)
parseLineDirective line = do
  afterHash <- T.stripPrefix "#" line
  let directive = T.stripStart afterHash
      afterLine = fromMaybe directive (T.stripPrefix "line" directive)
      (lineNumberText, rest) = T.span (`elem` ['0' .. '9']) (T.stripStart afterLine)
  lineNumber <- readMaybe (T.unpack lineNumberText)
  pure (lineNumber, directiveFile rest)
  where
    directiveFile rest =
      case T.breakOn "\"" rest of
        (_, quoted)
          | Just afterQuote <- T.stripPrefix "\"" quoted,
            let (file, closingQuote) = T.breakOn "\"" afterQuote,
            not (T.null closingQuote) ->
              Just (T.unpack file)
        _ -> Nothing
