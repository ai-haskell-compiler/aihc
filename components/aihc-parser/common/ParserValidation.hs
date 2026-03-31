{-# LANGUAGE OverloadedStrings #-}

module ParserValidation
  ( ValidationErrorKind (..),
    ValidationError (..),
    validateParser,
    validateParserWithExtensions,
    validateParserDetailed,
    validateParserDetailedWithExtensions,
    validateParserDetailedWithExtensionNames,
    validateParserDetailedWithParserExtensions,
  )
where

import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, errorBundlePretty, parseModule)
import qualified Aihc.Parser.Syntax as Syntax
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.LanguageExtensions.Type (Extension)
import qualified GhcOracle
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

data ValidationErrorKind
  = ValidationParseError
  | ValidationRoundtripError
  deriving (Eq, Show)

data ValidationError = ValidationError
  { validationErrorKind :: ValidationErrorKind,
    validationErrorMessage :: String
  }
  deriving (Eq, Show)

validateParser :: Text -> Maybe String
validateParser = fmap validationErrorMessage . validateParserDetailed

validateParserWithExtensions :: [Extension] -> Text -> Maybe String
validateParserWithExtensions exts = fmap validationErrorMessage . validateParserDetailedWithExtensions exts

validateParserDetailed :: Text -> Maybe ValidationError
validateParserDetailed = validateParserDetailedWithExtensions []

-- | Validate parser with GHC extensions.
validateParserDetailedWithExtensions :: [Extension] -> Text -> Maybe ValidationError
validateParserDetailedWithExtensions = validateParserDetailedCore

-- | Validate parser with extension names (as strings) and optional language.
-- This is a convenience function for use with cabal file metadata.
validateParserDetailedWithExtensionNames :: [String] -> Maybe String -> Text -> Maybe ValidationError
validateParserDetailedWithExtensionNames extNames langName =
  validateParserDetailedWithExtensions (GhcOracle.extensionNamesToGhcExtensions extNames langName)

-- | Validate parser with parser extensions directly.
-- This is the preferred way to validate when using unified extension handling.
validateParserDetailedWithParserExtensions :: [Syntax.Extension] -> Text -> Maybe ValidationError
validateParserDetailedWithParserExtensions parserExts =
  let ghcExts = mapMaybe GhcOracle.toGhcExtension parserExts
   in validateParserDetailedCoreWithParserExts parserExts ghcExts

-- | Core validation that accepts both parser extensions and GHC extensions.
-- This ensures both parsers use exactly the same extensions.
validateParserDetailedCoreWithParserExts :: [Syntax.Extension] -> [Extension] -> Text -> Maybe ValidationError
validateParserDetailedCoreWithParserExts parserExts ghcExts source =
  case parseModule parserConfig source of
    ParseErr err ->
      Just
        ValidationError
          { validationErrorKind = ValidationParseError,
            validationErrorMessage = "Parse failed:\n" <> errorBundlePretty (Just source) err
          }
    ParseOk parsed ->
      let rendered = renderStrict (layoutPretty defaultLayoutOptions (pretty parsed))
          sourceAst = GhcOracle.oracleModuleAstFingerprintWithExtensionsAt "parser-validation" ghcExts source
          renderedAst = GhcOracle.oracleModuleAstFingerprintWithExtensionsAt "parser-validation" ghcExts rendered
       in case (sourceAst, renderedAst) of
            (Right sourceFp, Right renderedFp)
              | sourceFp == renderedFp -> Nothing
              | otherwise ->
                  Just
                    ValidationError
                      { validationErrorKind = ValidationRoundtripError,
                        validationErrorMessage = formatFingerprintMismatch sourceFp renderedFp
                      }
            (Left sourceErr, Left renderedErr) ->
              Just
                ValidationError
                  { validationErrorKind = ValidationRoundtripError,
                    validationErrorMessage =
                      unlines
                        [ "Roundtrip check failed: GHC rejected both module versions.",
                          "Original error:",
                          T.unpack sourceErr,
                          "Roundtripped error:",
                          T.unpack renderedErr
                        ]
                  }
            (Left sourceErr, Right _) ->
              Just
                ValidationError
                  { validationErrorKind = ValidationRoundtripError,
                    validationErrorMessage =
                      unlines
                        [ "Roundtrip check failed: GHC rejected the original module.",
                          T.unpack sourceErr
                        ]
                  }
            (Right _, Left renderedErr) ->
              Just
                ValidationError
                  { validationErrorKind = ValidationRoundtripError,
                    validationErrorMessage =
                      unlines
                        [ "Roundtrip check failed: GHC rejected the roundtripped module.",
                          T.unpack renderedErr
                        ]
                  }
  where
    parserConfig =
      defaultConfig
        { parserSourceName = "parser-validation",
          parserExtensions = parserExts
        }

validateParserDetailedCore :: [Extension] -> Text -> Maybe ValidationError
validateParserDetailedCore exts =
  validateParserDetailedCoreWithParserExts (mapMaybe GhcOracle.fromGhcExtension exts) exts

formatFingerprintMismatch :: Text -> Text -> String
formatFingerprintMismatch sourceFp renderedFp =
  let header = "Roundtrip mismatch: GHC fingerprint changed after pretty-print(parse(module))."
      diffText = formatDiff sourceFp renderedFp
   in case diffText of
        Nothing -> header
        Just diffChunk ->
          unlines
            [ header,
              "Changed section in GHC pretty-printed output:",
              T.unpack diffChunk
            ]

formatDiff :: Text -> Text -> Maybe Text
formatDiff before after =
  let beforeLines = T.lines before
      afterLines = T.lines after
      prefixLen = commonPrefixLen beforeLines afterLines
      beforeRest = drop prefixLen beforeLines
      afterRest = drop prefixLen afterLines
      suffixLen = commonSuffixLen beforeRest afterRest
      changedBefore = take (length beforeRest - suffixLen) beforeRest
      changedAfter = take (length afterRest - suffixLen) afterRest
      removed = map ("- " <>) (take 30 changedBefore)
      added = map ("+ " <>) (take 30 changedAfter)
   in if null changedBefore && null changedAfter
        then Nothing
        else
          Just
            ( T.unlines
                ( ["@@ line " <> T.pack (show (prefixLen + 1)) <> " @@"]
                    <> removed
                    <> added
                    <> [truncationNote (length changedBefore) (length changedAfter)]
                )
            )

truncationNote :: Int -> Int -> Text
truncationNote removedN addedN
  | removedN <= 30 && addedN <= 30 = ""
  | otherwise = "...diff truncated..."

commonPrefixLen :: (Eq a) => [a] -> [a] -> Int
commonPrefixLen = go 0
  where
    go n (a : as) (b : bs)
      | a == b = go (n + 1) as bs
      | otherwise = n
    go n _ _ = n

commonSuffixLen :: (Eq a) => [a] -> [a] -> Int
commonSuffixLen xs ys =
  let lenXs = length xs
      lenYs = length ys
      minLen = min lenXs lenYs
      alignedXs = drop (lenXs - minLen) xs
      alignedYs = drop (lenYs - minLen) ys
      suffixEqs = zipWith (==) (reverse alignedXs) (reverse alignedYs)
   in length (takeWhile id suffixEqs)
