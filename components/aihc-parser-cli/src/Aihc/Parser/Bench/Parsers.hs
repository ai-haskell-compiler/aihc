-- | Parser wrappers for benchmarking.
-- Provides uniform interface for aihc-parser, haskell-src-exts, and ghc-lib-parser.
-- All parsers use the same extension detection: first reading LANGUAGE pragmas from the
-- module header using readModuleHeaderExtensions, then applying cabal-file extensions on top.
module Aihc.Parser.Bench.Parsers
  ( ParseResult (..),

    -- * Parsing with explicit extensions (from cabal files)
    parseWithAihcExts,
    parseWithHseExts,
    parseWithGhcExts,
    lexWithAihcExts,

    -- * Legacy: parsing without cabal extensions (uses defaults)
    parseWithAihc,
    parseWithHse,
    parseWithGhc,
    lexWithAihc,
  )
where

import Aihc.Parser qualified as Aihc
import Aihc.Parser.Lex qualified as AihcLex
import Aihc.Parser.Syntax qualified as Syntax
import Control.DeepSeq (deepseq)
import Data.List qualified as List
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Session qualified as DynFlags
import GHC.LanguageExtensions.Type qualified as GHC
import GHC.Parser (parseModule)
import GHC.Parser.Lexer
  ( getPsErrorMessages,
    initParserState,
    mkParserOpts,
    unP,
  )
import GHC.Parser.Lexer qualified as Lexer (ParseResult (..))
import GHC.Types.Error (NoDiagnosticOpts (NoDiagnosticOpts))
import GHC.Types.SrcLoc (mkRealSrcLoc)
import GHC.Utils.Error (emptyDiagOpts, pprMessages)
import GHC.Utils.Outputable (showSDocUnsafe)
import Language.Haskell.Exts qualified as HSE
import Text.Read (readMaybe)

-- | Result of attempting to parse a file.
data ParseResult
  = ParseSuccess
  | ParseFailure !String
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Parsing with explicit extensions from cabal files
--------------------------------------------------------------------------------

-- | Parse with aihc-parser using extensions from cabal file.
-- First reads LANGUAGE pragmas from source, then applies cabal extensions.
parseWithAihcExts :: [String] -> Maybe String -> Text -> ParseResult
parseWithAihcExts cabalExts langName source =
  let -- Get extensions from LANGUAGE pragmas in the source
      headerSettings = AihcLex.readModuleHeaderExtensions source
      -- Convert cabal extension names to settings
      cabalSettings = extensionNamesToSettings cabalExts
      -- Combine: cabal first, then header pragmas override
      allSettings = cabalSettings <> headerSettings
      -- Convert to aihc Extension list
      extensions = applyExtensionSettings allSettings (languageBaseExtensions langName)
      config = Aihc.defaultConfig {Aihc.parserExtensions = extensions}
   in case Aihc.parseModule config source of
        Aihc.ParseOk m -> m `deepseq` ParseSuccess
        Aihc.ParseErr err -> ParseFailure (Aihc.errorBundlePretty (Just source) err)

-- | Lex with aihc-parser using extensions from cabal file (lexer-only mode).
lexWithAihcExts :: [String] -> Maybe String -> Text -> ParseResult
lexWithAihcExts cabalExts langName source =
  let headerSettings = AihcLex.readModuleHeaderExtensions source
      cabalSettings = extensionNamesToSettings cabalExts
      allSettings = cabalSettings <> headerSettings
      extensions = applyExtensionSettings allSettings (languageBaseExtensions langName)
      tokens = AihcLex.lexModuleTokensWithExtensions extensions source
   in tokens `deepseq` ParseSuccess

-- | Parse with haskell-src-exts using extensions from cabal file.
parseWithHseExts :: [String] -> Maybe String -> Text -> ParseResult
parseWithHseExts cabalExts langName source =
  let -- Get extensions from LANGUAGE pragmas in the source
      headerSettings = AihcLex.readModuleHeaderExtensions source
      -- Convert cabal extension names to settings
      cabalSettings = extensionNamesToSettings cabalExts
      -- Combine: cabal first, then header pragmas override
      allSettings = cabalSettings <> headerSettings
      -- Convert to HSE extensions
      hseExts = toHseExtensionSettings allSettings
      langExts = languageToHseExtensions langName
      baseLang = languageToHseLanguage langName
      mode =
        HSE.defaultParseMode
          { HSE.baseLanguage = baseLang,
            HSE.extensions = langExts <> hseExts
          }
   in case HSE.parseModuleWithMode mode (T.unpack source) of
        HSE.ParseOk m -> m `seq` ParseSuccess
        HSE.ParseFailed loc msg -> ParseFailure (HSE.prettyPrint loc ++ ": " ++ msg)

-- | Parse with ghc-lib-parser using extensions from cabal file.
parseWithGhcExts :: [String] -> Maybe String -> Text -> ParseResult
parseWithGhcExts cabalExts langName source =
  let -- Get extensions from LANGUAGE pragmas in the source
      headerSettings = AihcLex.readModuleHeaderExtensions source
      -- Convert cabal extension names to settings
      cabalSettings = extensionNamesToSettings cabalExts
      -- Combine: cabal first, then header pragmas override
      allSettings = cabalSettings <> headerSettings
      -- Start with language base extensions
      langExts = languageToGhcExtensions langName
      baseExtSet = EnumSet.fromList langExts :: EnumSet.EnumSet GHC.Extension
      -- Apply all settings
      finalExtSet = List.foldl' applyGhcExtensionSetting baseExtSet allSettings
      -- Apply implied extensions
      extSet = applyImpliedExtensions finalExtSet
      opts = mkParserOpts extSet emptyDiagOpts False False False True
      buffer = stringToStringBuffer (T.unpack source)
      start = mkRealSrcLoc (mkFastString "<bench>") 1 1
   in case unP parseModule (initParserState opts buffer start) of
        Lexer.POk _ m -> m `seq` ParseSuccess
        Lexer.PFailed pState ->
          let msgs = getPsErrorMessages pState
              errText = showSDocUnsafe (pprMessages NoDiagnosticOpts msgs)
           in ParseFailure errText

--------------------------------------------------------------------------------
-- Legacy functions (no cabal extensions)
--------------------------------------------------------------------------------

-- | Parse with aihc-parser using only LANGUAGE pragmas from source.
parseWithAihc :: Text -> ParseResult
parseWithAihc = parseWithAihcExts [] Nothing

-- | Lex with aihc-parser (lexer-only mode).
lexWithAihc :: Text -> ParseResult
lexWithAihc = lexWithAihcExts [] Nothing

-- | Parse with haskell-src-exts using only LANGUAGE pragmas from source.
parseWithHse :: Text -> ParseResult
parseWithHse = parseWithHseExts [] Nothing

-- | Parse with ghc-lib-parser using only LANGUAGE pragmas from source.
parseWithGhc :: Text -> ParseResult
parseWithGhc = parseWithGhcExts [] Nothing

--------------------------------------------------------------------------------
-- Extension conversion utilities
--------------------------------------------------------------------------------

-- | Convert extension names (from cabal files) to ExtensionSettings.
extensionNamesToSettings :: [String] -> [Syntax.ExtensionSetting]
extensionNamesToSettings = mapMaybe (Syntax.parseExtensionSettingName . T.pack)

-- | Apply extension settings to a base list of extensions.
applyExtensionSettings :: [Syntax.ExtensionSetting] -> [Syntax.Extension] -> [Syntax.Extension]
applyExtensionSettings settings baseExts = List.foldl' applySetting baseExts settings
  where
    applySetting exts (Syntax.EnableExtension ext) = ext : filter (/= ext) exts
    applySetting exts (Syntax.DisableExtension ext) = filter (/= ext) exts

-- | Get base extensions for a language.
languageBaseExtensions :: Maybe String -> [Syntax.Extension]
languageBaseExtensions langName =
  case langName of
    Just "GHC2024" -> ghc2024Extensions
    Just "GHC2021" -> ghc2021Extensions
    Just "Haskell2010" -> []
    Just "Haskell98" -> []
    _ -> [] -- Default to no extensions

-- | GHC2021 extensions in Syntax.Extension form
ghc2021Extensions :: [Syntax.Extension]
ghc2021Extensions =
  [ Syntax.BangPatterns,
    Syntax.BinaryLiterals,
    Syntax.ConstrainedClassMethods,
    Syntax.ConstraintKinds,
    Syntax.DeriveDataTypeable,
    Syntax.DeriveFoldable,
    Syntax.DeriveFunctor,
    Syntax.DeriveGeneric,
    Syntax.DeriveLift,
    Syntax.DeriveTraversable,
    Syntax.DoAndIfThenElse,
    Syntax.EmptyCase,
    Syntax.EmptyDataDecls,
    Syntax.EmptyDataDeriving,
    Syntax.ExistentialQuantification,
    Syntax.ExplicitForAll,
    Syntax.FlexibleContexts,
    Syntax.FlexibleInstances,
    Syntax.ForeignFunctionInterface,
    Syntax.GADTSyntax,
    Syntax.GeneralizedNewtypeDeriving,
    Syntax.HexFloatLiterals,
    Syntax.ImportQualifiedPost,
    Syntax.InstanceSigs,
    Syntax.KindSignatures,
    Syntax.MultiParamTypeClasses,
    Syntax.NamedFieldPuns,
    Syntax.NamedWildCards,
    Syntax.NumericUnderscores,
    Syntax.PolyKinds,
    Syntax.PostfixOperators,
    Syntax.RankNTypes,
    Syntax.ScopedTypeVariables,
    Syntax.StandaloneDeriving,
    Syntax.StandaloneKindSignatures,
    Syntax.TupleSections,
    Syntax.TypeApplications,
    Syntax.TypeOperators,
    Syntax.TypeSynonymInstances
  ]

-- | GHC2024 extensions (GHC2021 + extras)
ghc2024Extensions :: [Syntax.Extension]
ghc2024Extensions =
  ghc2021Extensions
    <> [ Syntax.DataKinds,
         Syntax.DerivingStrategies,
         Syntax.DisambiguateRecordFields,
         Syntax.ExplicitNamespaces,
         Syntax.GADTs,
         Syntax.MonoLocalBinds,
         Syntax.LambdaCase,
         Syntax.RoleAnnotations
       ]

--------------------------------------------------------------------------------
-- HSE extension conversion
--------------------------------------------------------------------------------

-- | Convert ExtensionSettings to HSE extensions.
toHseExtensionSettings :: [Syntax.ExtensionSetting] -> [HSE.Extension]
toHseExtensionSettings = mapMaybe toHseExtensionSetting

toHseExtensionSetting :: Syntax.ExtensionSetting -> Maybe HSE.Extension
toHseExtensionSetting setting =
  case setting of
    Syntax.EnableExtension ext -> toHseExtension ext
    Syntax.DisableExtension ext -> HSE.DisableExtension <$> toHseKnownExtension ext

toHseExtension :: Syntax.Extension -> Maybe HSE.Extension
toHseExtension ext = HSE.EnableExtension <$> toHseKnownExtension ext

toHseKnownExtension :: Syntax.Extension -> Maybe HSE.KnownExtension
toHseKnownExtension ext =
  case ext of
    Syntax.CPP -> Just HSE.CPP
    Syntax.GeneralizedNewtypeDeriving -> Just HSE.GeneralizedNewtypeDeriving
    _ -> readMaybe (T.unpack (Syntax.extensionName ext))

-- | Get HSE Language for a language name.
-- HSE has native support for Haskell98 and Haskell2010.
-- For GHC2021/2024, we fall back to Haskell2010 as the base and add extensions separately.
languageToHseLanguage :: Maybe String -> HSE.Language
languageToHseLanguage langName =
  case langName of
    Just "Haskell98" -> HSE.Haskell98
    Just "Haskell2010" -> HSE.Haskell2010
    Just "GHC2021" -> HSE.Haskell2010 -- Base for GHC2021, extensions added separately
    Just "GHC2024" -> HSE.Haskell2010 -- Base for GHC2024, extensions added separately
    _ -> HSE.Haskell2010 -- Default

-- | Get HSE extensions for a language.
-- For Haskell98/Haskell2010, no extra extensions needed (handled by baseLanguage).
-- For GHC2021/2024, we add the extensions that HSE supports.
languageToHseExtensions :: Maybe String -> [HSE.Extension]
languageToHseExtensions langName =
  case langName of
    Just "GHC2024" -> map HSE.EnableExtension hseGhc2024Exts
    Just "GHC2021" -> map HSE.EnableExtension hseGhc2021Exts
    _ -> [] -- Haskell98/Haskell2010 handled by baseLanguage
  where
    -- HSE doesn't have GHC2021/2024, so we approximate with individual extensions
    -- Note: Some GHC2021 extensions are not available in HSE (e.g., GADTSyntax, HexFloatLiterals)
    hseGhc2021Exts =
      [ HSE.BangPatterns,
        HSE.BinaryLiterals,
        HSE.ConstraintKinds,
        HSE.DeriveDataTypeable,
        HSE.DeriveFoldable,
        HSE.DeriveFunctor,
        HSE.DeriveGeneric,
        HSE.DeriveTraversable,
        HSE.DoAndIfThenElse,
        HSE.EmptyCase,
        HSE.EmptyDataDecls,
        HSE.ExistentialQuantification,
        HSE.ExplicitForAll,
        HSE.FlexibleContexts,
        HSE.FlexibleInstances,
        HSE.ForeignFunctionInterface,
        HSE.GeneralizedNewtypeDeriving,
        HSE.InstanceSigs,
        HSE.KindSignatures,
        HSE.MultiParamTypeClasses,
        HSE.NamedFieldPuns,
        HSE.NamedWildCards,
        HSE.PolyKinds,
        HSE.PostfixOperators,
        HSE.RankNTypes,
        HSE.ScopedTypeVariables,
        HSE.StandaloneDeriving,
        HSE.TupleSections,
        HSE.TypeApplications,
        HSE.TypeOperators,
        HSE.TypeSynonymInstances
      ]
    hseGhc2024Exts =
      hseGhc2021Exts
        <> [ HSE.DataKinds,
             HSE.DerivingStrategies,
             HSE.DisambiguateRecordFields,
             HSE.ExplicitNamespaces,
             HSE.GADTs,
             HSE.LambdaCase,
             HSE.RoleAnnotations
           ]

--------------------------------------------------------------------------------
-- GHC extension conversion
--------------------------------------------------------------------------------

-- | Apply an ExtensionSetting to a GHC extension set.
applyGhcExtensionSetting :: EnumSet.EnumSet GHC.Extension -> Syntax.ExtensionSetting -> EnumSet.EnumSet GHC.Extension
applyGhcExtensionSetting exts setting =
  case setting of
    Syntax.EnableExtension ext ->
      maybe exts (`EnumSet.insert` exts) (toGhcExtension ext)
    Syntax.DisableExtension ext ->
      maybe exts (`EnumSet.delete` exts) (toGhcExtension ext)

-- | Convert an aihc Extension to a GHC Extension.
toGhcExtension :: Syntax.Extension -> Maybe GHC.Extension
toGhcExtension ext =
  case ext of
    Syntax.NondecreasingIndentation ->
      lookupAny ["NondecreasingIndentation", "AlternativeLayoutRule", "AlternativeLayoutRuleTransitional", "RelaxedLayout"]
    _ ->
      lookupAny [toGhcExtensionName ext]
  where
    ghcExtensions = [(show ghcExt, ghcExt) | ghcExt <- [minBound .. maxBound]]
    lookupAny [] = Nothing
    lookupAny (name : names) =
      case lookup name ghcExtensions of
        Just ghcExt -> Just ghcExt
        Nothing -> lookupAny names

    toGhcExtensionName Syntax.CPP = "Cpp"
    toGhcExtensionName Syntax.GeneralizedNewtypeDeriving = "GeneralisedNewtypeDeriving"
    toGhcExtensionName Syntax.SafeHaskell = "Safe"
    toGhcExtensionName Syntax.Trustworthy = "Trustworthy"
    toGhcExtensionName Syntax.UnsafeHaskell = "Unsafe"
    toGhcExtensionName Syntax.Rank2Types = "RankNTypes"
    toGhcExtensionName Syntax.PolymorphicComponents = "RankNTypes"
    toGhcExtensionName other = T.unpack (Syntax.extensionName other)

-- | Get GHC extensions for a language.
languageToGhcExtensions :: Maybe String -> [GHC.Extension]
languageToGhcExtensions langName =
  case langName >>= parseLanguage of
    Just lang -> DynFlags.languageExtensions (Just lang)
    Nothing -> []
  where
    parseLanguage lang =
      case lang of
        "Haskell98" -> Just DynFlags.Haskell98
        "Haskell2010" -> Just DynFlags.Haskell2010
        "GHC2021" -> Just DynFlags.GHC2021
        "GHC2024" -> Just DynFlags.GHC2024
        _ -> Nothing

-- | Apply implied extensions (like GHC does).
applyImpliedExtensions :: EnumSet.EnumSet GHC.Extension -> EnumSet.EnumSet GHC.Extension
applyImpliedExtensions = go
  where
    go exts =
      let next = List.foldl' apply exts DynFlags.impliedXFlags
       in if EnumSet.toList next == EnumSet.toList exts then exts else go next

    apply exts (enabled, implied)
      | EnumSet.member enabled exts =
          case implied of
            DynFlags.On ext -> EnumSet.insert ext exts
            DynFlags.Off ext -> EnumSet.delete ext exts
      | otherwise = exts
