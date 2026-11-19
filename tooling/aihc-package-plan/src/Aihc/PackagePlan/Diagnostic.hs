{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Aihc.PackagePlan.Diagnostic
-- Description : Human-readable rendering of parse and preprocessor diagnostics
--
-- Renders the diagnostics produced while parsing and preprocessing a package
-- as GHC-style messages.
module Aihc.PackagePlan.Diagnostic
  ( Diagnostic,
    isCppWarning,
    renderHumanDiagnostic,
    DiagnosticSourceMap,
    diagnosticSourceMap,
    parseDiagnosticValue,
    cppDiagnosticValue,
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

-- | A diagnostic from the parser or preprocessor.
data Diagnostic = Diagnostic
  { diagnosticFile :: !FilePath,
    -- | The line and, if known, the column.
    diagnosticPosition :: !(Int, Maybe Int),
    diagnosticSeverity :: !Text,
    diagnosticMessage :: !Text
  }
  deriving (Eq, Show)

instance NFData Diagnostic where
  rnf (Diagnostic file position severity message) =
    rnf file `seq` rnf position `seq` rnf severity `seq` rnf message

isCppWarning :: Diagnostic -> Bool
isCppWarning diagnostic = diagnosticSeverity diagnostic == "Warning"

renderHumanDiagnostic :: Diagnostic -> String
renderHumanDiagnostic diagnostic =
  unlines
    [ diagnosticFile diagnostic
        <> ":"
        <> show line
        <> maybe "" ((":" <>) . show) column
        <> ": "
        <> T.unpack (diagnosticSeverity diagnostic)
        <> ": "
        <> T.unpack (diagnosticMessage diagnostic)
    ]
  where
    (line, column) = diagnosticPosition diagnostic

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

parseDiagnosticValue :: FilePath -> (SourceSpan, Text) -> Diagnostic
parseDiagnosticValue path (SourceSpan _ startLine startColumn _ _ _ _, message) =
  Diagnostic
    { diagnosticFile = path,
      diagnosticPosition = (startLine, Just startColumn),
      diagnosticSeverity = "error",
      diagnosticMessage = message
    }

cppDiagnosticValue :: Cpp.Diagnostic -> Diagnostic
cppDiagnosticValue diag =
  Diagnostic
    { diagnosticFile = Cpp.diagFile diag,
      diagnosticPosition = (Cpp.diagLine diag, Nothing),
      diagnosticSeverity = T.pack (show (Cpp.diagSeverity diag)),
      diagnosticMessage = Cpp.diagMessage diag
    }
