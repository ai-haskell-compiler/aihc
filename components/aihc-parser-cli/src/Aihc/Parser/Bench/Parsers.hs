-- | Parser wrappers for benchmarking.
-- Provides uniform interface for aihc-parser, haskell-src-exts, and ghc-lib-parser.
-- All parsers use the same extension detection and CPP preprocessing:
-- 1. First scan for CPP extension in cabal extensions + LANGUAGE pragmas
-- 2. If CPP is enabled, preprocess the source
-- 3. Re-scan the preprocessed source for LANGUAGE pragmas
-- 4. Parse with the final extension set
module Aihc.Parser.Bench.Parsers
  ( ParseResult (..),

    -- * CPP include collection
    collectCppIncludes,

    -- * Parsing with explicit extensions (from cabal files)
    parseWithAihcExts,
    parseWithHseExts,
    parseWithGhcExts,
    lexWithAihcExts,
  )
where

import Aihc.Cpp qualified as Cpp
import Aihc.Hackage.Util qualified as HU
import Aihc.Parser qualified as Aihc
import Aihc.Parser.Lex qualified as AihcLex
import Aihc.Parser.Syntax qualified as Syntax
import Control.DeepSeq (NFData (..), deepseq)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Session qualified as DynFlags
import GHC.Generics (Generic)
import GHC.LanguageExtensions.Type qualified as GHC
import GHC.Parser (parseModule)
import GHC.Parser.Lexer
  ( getPsErrorMessages,
    initParserState,
    mkParserOpts,
    unP,
  )
import GHC.Parser.Lexer qualified as Lexer (ParseResult (..))
import GHC.Types.Error (NoDiagnosticOpts (NoDiagnosticOpts), errorsFound)
import GHC.Types.SrcLoc (mkRealSrcLoc)
import GHC.Utils.Error (emptyDiagOpts, pprMessages)
import GHC.Utils.Outputable (showSDocUnsafe)
import Language.Haskell.Exts qualified as HSE
import System.Directory (doesFileExist)
import System.FilePath (normalise, takeDirectory, (</>))
import Text.Read (readMaybe)

-- | Result of attempting to parse a file.
data ParseResult
  = ParseSuccess
  | ParseFailure !String
  deriving (Eq, Show, Generic)

instance NFData ParseResult where
  rnf ParseSuccess = ()
  rnf (ParseFailure s) = rnf s

--------------------------------------------------------------------------------
-- Parsing with explicit extensions from cabal files
--------------------------------------------------------------------------------

-- | Parse with aihc-parser using extensions from cabal file.
-- Handles CPP preprocessing if CPP extension is enabled.
parseWithAihcExts :: Map FilePath Text -> FilePath -> [String] -> Maybe String -> Text -> ParseResult
parseWithAihcExts includeMap filePath cabalExts langName source =
  let (preprocessedSource, extensions) = prepareSourceAndExtensions includeMap filePath cabalExts langName source
      config = Aihc.defaultConfig {Aihc.parserExtensions = extensions}
      (errs, m) = Aihc.parseModule config preprocessedSource
   in if null errs
        then m `deepseq` ParseSuccess
        else ParseFailure (Aihc.formatParseErrors "<bench>" (Just preprocessedSource) errs)

-- | Lex with aihc-parser using extensions from cabal file (lexer-only mode).
-- Handles CPP preprocessing if CPP extension is enabled.
lexWithAihcExts :: Map FilePath Text -> FilePath -> [String] -> Maybe String -> Text -> ParseResult
lexWithAihcExts includeMap filePath cabalExts langName source =
  let (preprocessedSource, extensions) = prepareSourceAndExtensions includeMap filePath cabalExts langName source
      tokens = AihcLex.lexModuleTokensWithExtensions extensions preprocessedSource
   in tokens `deepseq` ParseSuccess

-- | Parse with haskell-src-exts using extensions from cabal file.
-- Handles CPP preprocessing if CPP extension is enabled.
parseWithHseExts :: Map FilePath Text -> FilePath -> [String] -> Maybe String -> Text -> ParseResult
parseWithHseExts includeMap filePath cabalExts langName source =
  let (preprocessedSource, extensions) = prepareSourceAndExtensions includeMap filePath cabalExts langName source
      hseExts = toHseExtensions extensions
      langExts = languageToHseExtensions langName
      baseLang = languageToHseLanguage langName
      mode =
        HSE.defaultParseMode
          { HSE.baseLanguage = baseLang,
            HSE.extensions = langExts <> hseExts
          }
   in case HSE.parseModuleWithMode mode (T.unpack preprocessedSource) of
        HSE.ParseOk m -> m `seq` ParseSuccess
        HSE.ParseFailed loc msg -> ParseFailure (HSE.prettyPrint loc ++ ": " ++ msg)

-- | Parse with ghc-lib-parser using extensions from cabal file.
-- Handles CPP preprocessing if CPP extension is enabled.
-- Note: GHC can return POk with errors, so we check for errors even on success.
parseWithGhcExts :: Map FilePath Text -> FilePath -> [String] -> Maybe String -> Text -> ParseResult
parseWithGhcExts includeMap filePath cabalExts langName source =
  let (preprocessedSource, extensions) = prepareSourceAndExtensions includeMap filePath cabalExts langName source
      -- Start with language base extensions
      langExts = languageToGhcExtensions langName
      baseExtSet = EnumSet.fromList langExts :: EnumSet.EnumSet GHC.Extension
      -- Convert aihc extensions to GHC extensions and apply
      ghcSettings = map toGhcExtensionSetting extensions
      finalExtSet = List.foldl' applyGhcExtensionSetting baseExtSet ghcSettings
      -- Apply implied extensions
      extSet = applyImpliedExtensions finalExtSet
      opts = mkParserOpts extSet emptyDiagOpts False False False True
      buffer = stringToStringBuffer (T.unpack preprocessedSource)
      start = mkRealSrcLoc (mkFastString "<bench>") 1 1
   in case unP parseModule (initParserState opts buffer start) of
        Lexer.POk pState m ->
          -- GHC can return POk with errors, so we must check
          if errorsFound (getPsErrorMessages pState)
            then
              let msgs = getPsErrorMessages pState
                  errText = showSDocUnsafe (pprMessages NoDiagnosticOpts msgs)
               in ParseFailure errText
            else m `seq` ParseSuccess
        Lexer.PFailed pState ->
          let msgs = getPsErrorMessages pState
              errText = showSDocUnsafe (pprMessages NoDiagnosticOpts msgs)
           in ParseFailure errText

--------------------------------------------------------------------------------
-- Source preparation with CPP preprocessing
--------------------------------------------------------------------------------

-- | Prepare source for parsing: check for CPP, preprocess if needed, collect extensions.
-- This implements the two-step extension scanning:
-- 1. Check if CPP is enabled (from cabal extensions or initial LANGUAGE pragmas)
-- 2. If CPP is enabled, preprocess the source
-- 3. Re-scan the (possibly preprocessed) source for final LANGUAGE pragmas
prepareSourceAndExtensions :: Map FilePath Text -> FilePath -> [String] -> Maybe String -> Text -> (Text, [Syntax.Extension])
prepareSourceAndExtensions includeMap filePath cabalExts langName source =
  let -- Convert cabal extension names to settings
      cabalSettings = extensionNamesToSettings cabalExts
      -- Get initial extensions from LANGUAGE pragmas (before CPP)
      initialHeaderSettings = AihcLex.readModuleHeaderExtensions source
      -- Combine to check if CPP is enabled
      initialSettings = cabalSettings <> initialHeaderSettings
      baseExts = languageBaseExtensions langName
      initialExtensions = applyExtensionSettings initialSettings baseExts
      cppEnabled = Syntax.CPP `elem` initialExtensions
      -- Preprocess if CPP is enabled
      preprocessedSource =
        if cppEnabled
          then runCppWithIncludes includeMap filePath source
          else source
      -- Re-scan for extensions after preprocessing (CPP can affect LANGUAGE pragmas)
      finalHeaderSettings = AihcLex.readModuleHeaderExtensions preprocessedSource
      finalSettings = cabalSettings <> finalHeaderSettings
      finalExtensions = applyExtensionSettings finalSettings baseExts
   in (preprocessedSource, finalExtensions)

-- | Run CPP preprocessor on source, resolving includes from the provided map.
-- Include paths not found in the map are treated as missing.
runCppWithIncludes :: Map FilePath Text -> FilePath -> Text -> Text
runCppWithIncludes includeMap filePath source =
  let cfg = Cpp.defaultConfig {Cpp.configInputFile = filePath}
   in Cpp.resultOutput (go (Cpp.preprocess cfg (TE.encodeUtf8 source)))
  where
    go (Cpp.Done result) = result
    go (Cpp.NeedInclude req k) =
      let resolved = normalise (takeDirectory (Cpp.includeFrom req) </> Cpp.includePath req)
          mContents = fmap TE.encodeUtf8 (Map.lookup resolved includeMap)
       in go (k mContents)

-- | Collect all CPP include files needed by a Haskell source file.
-- Only runs CPP if the CPP extension is enabled for this file.
-- Returns (absolutePath, content) for all transitively included files.
collectCppIncludes :: FilePath -> [String] -> Maybe String -> Text -> IO [(FilePath, Text)]
collectCppIncludes absFile cabalExts langName source = do
  let cabalSettings = extensionNamesToSettings cabalExts
      initialHeaderSettings = AihcLex.readModuleHeaderExtensions source
      initialSettings = cabalSettings <> initialHeaderSettings
      baseExts = languageBaseExtensions langName
      initialExtensions = applyExtensionSettings initialSettings baseExts
      cppEnabled = Syntax.CPP `elem` initialExtensions
  if not cppEnabled
    then pure []
    else
      let cfg = Cpp.defaultConfig {Cpp.configInputFile = absFile}
       in Map.toList <$> go Map.empty (Cpp.preprocess cfg (TE.encodeUtf8 source))
  where
    go acc (Cpp.Done _) = pure acc
    go acc (Cpp.NeedInclude req k) = do
      let resolved = normalise (takeDirectory (Cpp.includeFrom req) </> Cpp.includePath req)
      case Map.lookup resolved acc of
        Just contents -> go acc (k (Just (TE.encodeUtf8 contents)))
        Nothing -> do
          exists <- doesFileExist resolved
          if not exists
            then go acc (k Nothing)
            else do
              contents <- HU.readTextFileLenient resolved
              go (Map.insert resolved contents acc) (k (Just (TE.encodeUtf8 contents)))

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

-- | Convert a list of aihc Extensions to HSE extensions.
toHseExtensions :: [Syntax.Extension] -> [HSE.Extension]
toHseExtensions = mapMaybe toHseExtension

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

-- | Convert an aihc Extension to an ExtensionSetting (always enabled).
toGhcExtensionSetting :: Syntax.Extension -> Syntax.ExtensionSetting
toGhcExtensionSetting = Syntax.EnableExtension

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
    lookupAny [] = Nothing
    lookupAny (name : names) =
      case lookup name ghcExtensionsByName of
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

-- | Lookup table mapping extension names to GHC extensions.
-- Lifted to top-level to avoid recomputing on every call.
ghcExtensionsByName :: [(String, GHC.Extension)]
ghcExtensionsByName = [(show ghcExt, ghcExt) | ghcExt <- [minBound .. maxBound]]

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
