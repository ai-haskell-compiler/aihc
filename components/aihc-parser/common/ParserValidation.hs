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
import Data.Text (Text)
import qualified Data.Text as T
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

validateParserWithExtensions :: [Syntax.ExtensionSetting] -> Text -> Maybe String
validateParserWithExtensions exts = fmap validationErrorMessage . validateParserDetailedWithExtensions exts

validateParserDetailed :: Text -> Maybe ValidationError
validateParserDetailed = validateParserDetailedWithExtensions []

-- | Validate parser with GHC extensions.
validateParserDetailedWithExtensions :: [Syntax.ExtensionSetting] -> Text -> Maybe ValidationError
validateParserDetailedWithExtensions = validateParserDetailedCore Syntax.Haskell2010Edition

-- | Validate parser with extension names (as strings) and optional language.
-- This is a convenience function for use with cabal file metadata.
-- In-file pragmas are read from the source to determine the full GHC extension set,
-- matching the behaviour of the GHC pre-check in the hackage-tester.
validateParserDetailedWithExtensionNames :: [Syntax.ExtensionSetting] -> Syntax.LanguageEdition -> Text -> Maybe ValidationError
validateParserDetailedWithExtensionNames extensionSettings edition source =
  validateParserDetailedCoreWithFingerprint "parser-validation" edition extensionSettings source

-- | Validate parser with parser extensions directly.
-- This is the preferred way to validate when using unified extension handling.
validateParserDetailedWithParserExtensions :: [Syntax.ExtensionSetting] -> Text -> Maybe ValidationError
validateParserDetailedWithParserExtensions =
  validateParserDetailedCoreWithFingerprint "parser-validation" Syntax.Haskell2010Edition

-- | Core validation with a caller-supplied fingerprint function.
-- This allows the caller to choose how GHC extensions are determined (e.g. from
-- pre-computed extension lists or by reading in-file pragmas).
validateParserDetailedCoreWithFingerprint :: String -> Syntax.LanguageEdition -> [Syntax.ExtensionSetting] -> Text -> Maybe ValidationError
validateParserDetailedCoreWithFingerprint sourceTag edition extensionSettings source =
  case parseModule parserConfig source of
    ParseErr err ->
      Just
        ValidationError
          { validationErrorKind = ValidationParseError,
            validationErrorMessage = "Parse failed:\n" <> errorBundlePretty (Just source) err
          }
    ParseOk parsed ->
      let rendered = renderStrict (layoutPretty defaultLayoutOptions (pretty parsed))
          sourceAst = fingerprint source
          renderedAst = fingerprint rendered
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
    finalExts = Syntax.effectiveExtensions edition extensionSettings
    fingerprint = GhcOracle.oracleModuleAstFingerprint sourceTag edition extensionSettings
    parserConfig =
      defaultConfig
        { parserSourceName = "parser-validation",
          parserExtensions = finalExts
        }

validateParserDetailedCore :: Syntax.LanguageEdition -> [Syntax.ExtensionSetting] -> Text -> Maybe ValidationError
validateParserDetailedCore =
  validateParserDetailedCoreWithFingerprint "parser-validation"

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
