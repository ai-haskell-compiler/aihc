{-# LANGUAGE OverloadedStrings #-}

-- | Module roundtrip validation utilities.
--
-- This module provides functions to validate that a parsed module can be
-- pretty-printed and re-parsed to produce the same AST (as verified against
-- the GHC oracle).
module RoundtripValidation
  ( -- * Roundtrip checking
    moduleRoundtripsViaGhc,
    moduleRoundtripsViaGhcWithExtensions,

    -- * GHC fingerprint helpers
    compareGhcFingerprints,
    FingerprintComparison (..),
  )
where

import Aihc.Parser (ParseResult (..))
import Aihc.Parser.Ast (Module)
import Data.Text (Text)
import GHC.LanguageExtensions.Type (Extension)
import GhcOracle (oracleModuleAstFingerprintWithExtensionsAt)
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

-- | Result of comparing GHC AST fingerprints.
data FingerprintComparison
  = -- | Fingerprints match
    FingerprintsMatch
  | -- | Fingerprints differ
    FingerprintsDiffer Text Text
  | -- | GHC rejected the original source
    SourceRejected Text
  | -- | GHC rejected the roundtripped source
    RoundtripRejected Text
  | -- | GHC rejected both sources
    BothRejected Text Text
  deriving (Eq, Show)

-- | Check if a parsed module roundtrips correctly via GHC.
--
-- This pretty-prints the parsed module and checks that GHC produces the
-- same AST fingerprint for both the original source and the pretty-printed
-- version.
moduleRoundtripsViaGhc :: Text -> ParseResult Module -> Bool
moduleRoundtripsViaGhc = moduleRoundtripsViaGhcWithExtensions []

-- | Check if a parsed module roundtrips correctly via GHC with extensions.
moduleRoundtripsViaGhcWithExtensions :: [Extension] -> Text -> ParseResult Module -> Bool
moduleRoundtripsViaGhcWithExtensions exts source oursResult =
  case oursResult of
    ParseErr _ -> False
    ParseOk parsed ->
      let rendered = renderStrict (layoutPretty defaultLayoutOptions (pretty parsed))
       in case compareGhcFingerprints "roundtrip-validation" exts source rendered of
            FingerprintsMatch -> True
            _ -> False

-- | Compare GHC AST fingerprints between original and pretty-printed source.
compareGhcFingerprints ::
  -- | Source file name for GHC
  String ->
  -- | Extensions to enable
  [Extension] ->
  -- | Original source
  Text ->
  -- | Pretty-printed source
  Text ->
  FingerprintComparison
compareGhcFingerprints sourceName exts original rendered =
  case ( oracleModuleAstFingerprintWithExtensionsAt sourceName exts original,
         oracleModuleAstFingerprintWithExtensionsAt sourceName exts rendered
       ) of
    (Right sourceFp, Right renderedFp)
      | sourceFp == renderedFp -> FingerprintsMatch
      | otherwise -> FingerprintsDiffer sourceFp renderedFp
    (Left sourceErr, Left renderedErr) -> BothRejected sourceErr renderedErr
    (Left sourceErr, Right _) -> SourceRejected sourceErr
    (Right _, Left renderedErr) -> RoundtripRejected renderedErr
