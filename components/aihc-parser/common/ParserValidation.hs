{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module ParserValidation
  ( ValidationErrorKind (..),
    ValidationError (..),
    formatDiff,
    stripParens,
    validateParser,
  )
where

import Aihc.Parser (ParserConfig (..), defaultConfig, formatParseErrors, parseModule)
import Aihc.Parser.Syntax qualified as Syntax
import Control.DeepSeq (NFData)
import Data.Algorithm.Diff (PolyDiff (..), getDiff)
import Data.Data (Data (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable, cast)
import GHC.Generics (Generic)
import GhcOracle qualified
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

data ValidationErrorKind
  = ValidationParseError
  | ValidationRoundtripError
  deriving (Eq, Show, NFData, Generic)

data ValidationError = ValidationError
  { validationErrorKind :: ValidationErrorKind,
    validationErrorMessage :: String
  }
  deriving (Eq, NFData, Generic)

instance Show ValidationError where
  show ValidationError {validationErrorKind = kind, validationErrorMessage = message} =
    show kind <> ":\n" <> message

-- | Core validation with a caller-supplied fingerprint function.
-- This allows the caller to choose how GHC extensions are determined (e.g. from
-- pre-computed extension lists or by reading in-file pragmas).
validateParser :: String -> Syntax.LanguageEdition -> [Syntax.ExtensionSetting] -> Text -> Maybe ValidationError
validateParser sourceTag edition extensionSettings source =
  let (errs, parsed) = parseModule parserConfig source
   in case errs of
        _ : _ ->
          Just
            ValidationError
              { validationErrorKind = ValidationParseError,
                validationErrorMessage = formatParseErrors sourceTag (Just source) errs
              }
        [] ->
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
        { parserSourceName = sourceTag,
          parserExtensions = finalExts
        }

stripParens :: (Data a) => a -> a
stripParens x = applyStrip (gmapT stripParens x)
  where
    applyStrip :: (Data c) => c -> c
    applyStrip =
      id
        `extT` stripExprParens
        `extT` stripTypeParens
        `extT` stripPatternParens

    stripExprParens :: Syntax.Expr -> Syntax.Expr
    stripExprParens (Syntax.EParen expr) = expr
    stripExprParens expr = expr

    stripTypeParens :: Syntax.Type -> Syntax.Type
    stripTypeParens (Syntax.TParen typ) = typ
    stripTypeParens typ = typ

    stripPatternParens :: Syntax.Pattern -> Syntax.Pattern
    stripPatternParens (Syntax.PParen pat) = pat
    stripPatternParens pat = pat

    extT :: (Typeable c, Typeable d) => (c -> c) -> (d -> d) -> c -> c
    extT f g y = fromMaybe (f y) (cast . g =<< cast y)

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
      diffLines = concatMap renderDiffLine (getDiff changedBefore changedAfter)
   in if null diffLines
        then Nothing
        else
          Just
            ( T.unlines
                ( ["@@ line " <> T.pack (show (prefixLen + 1)) <> " @@"]
                    <> diffLines
                )
            )

renderDiffLine :: PolyDiff Text Text -> [Text]
renderDiffLine diffLine =
  case diffLine of
    First lineText -> ["- " <> lineText]
    Second lineText -> ["+ " <> lineText]
    Both _ _ -> []

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
