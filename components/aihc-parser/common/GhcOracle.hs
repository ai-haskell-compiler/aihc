{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module GhcOracle
  ( oracleParsesModuleWithExtensions,
    oracleParsesModuleWithExtensionsAt,
    oracleModuleAstFingerprintWithExtensions,
    oracleModuleAstFingerprintWithExtensionsAt,
    oracleModuleParseErrorWithNames,
    oracleModuleParseErrorWithNamesAt,
    oracleParsesModuleWithNames,
    oracleParsesModuleWithNamesAt,
    oracleDetailedParsesModuleWithNames,
    oracleDetailedParsesModuleWithNamesAt,
    toGhcExtension,
    fromGhcExtension,
    extensionNamesToGhcExtensions,
    extensionNamesToParserExtensions,
  )
where

import Aihc.Cpp (resultOutput)
import qualified Aihc.Parser.Ast as Ast
import Control.Exception (catch, displayException, evaluate)
import CppSupport (moduleHeaderExtensionSettings, preprocessForParserWithoutIncludes)
import Data.Bifunctor (first)
import Data.List (nub)
import qualified Data.List as List
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import qualified GHC.Driver.DynFlags as DynFlags
import GHC.Driver.Session (impliedXFlags)
import GHC.Hs (GhcPs, HsModule)
import qualified GHC.LanguageExtensions.Type as GHC
import GHC.Parser (parseModule)
import GHC.Parser.Header (getOptions)
import GHC.Parser.Lexer
  ( PState,
    ParseResult (..),
    Token (..),
    getPsErrorMessages,
    initParserState,
    lexer,
    mkParserOpts,
    unP,
  )
import GHC.Types.Error (NoDiagnosticOpts (NoDiagnosticOpts))
import GHC.Types.SourceError (SourceError)
import GHC.Types.SrcLoc (GenLocated, Located, mkRealSrcLoc, unLoc)
import GHC.Utils.Error (emptyDiagOpts, pprMessages)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (foldl')

oracleParsesModuleWithExtensions :: [GHC.Extension] -> Text -> Bool
oracleParsesModuleWithExtensions = oracleParsesModuleWithExtensionsAt "oracle"

oracleParsesModuleWithExtensionsAt :: String -> [GHC.Extension] -> Text -> Bool
oracleParsesModuleWithExtensionsAt sourceTag exts input =
  case parseWithGhcWithExtensions sourceTag exts input of
    Left _ -> False
    Right _ -> True

oracleModuleAstFingerprintWithExtensions :: [GHC.Extension] -> Text -> Either Text Text
oracleModuleAstFingerprintWithExtensions = oracleModuleAstFingerprintWithExtensionsAt "oracle"

oracleModuleAstFingerprintWithExtensionsAt :: String -> [GHC.Extension] -> Text -> Either Text Text
oracleModuleAstFingerprintWithExtensionsAt sourceTag exts input = do
  (pragmas, parsed) <- parseWithGhcWithExtensions sourceTag exts input
  let pragmaFingerprint =
        if null pragmas
          then ""
          else "LANGUAGE " <> T.intercalate "," (map Ast.extensionSettingName pragmas) <> "\n"
  pure (pragmaFingerprint <> T.pack (showSDocUnsafe (ppr parsed)))

parseWithGhcWithExtensions :: String -> [GHC.Extension] -> Text -> Either Text ([Ast.ExtensionSetting], HsModule GhcPs)
parseWithGhcWithExtensions sourceTag extraExts input =
  first fst (parseWithGhcWithExtensionsDetailed sourceTag extraExts input)

parseWithGhcWithExtensionsDetailed :: String -> [GHC.Extension] -> Text -> Either (Text, EnumSet.EnumSet GHC.Extension) ([Ast.ExtensionSetting], HsModule GhcPs)
parseWithGhcWithExtensionsDetailed sourceTag extraExts input =
  let baseExts = nub extraExts
      baseExtSet = EnumSet.fromList baseExts :: EnumSet.EnumSet GHC.Extension
      baseParseExts = applyImpliedExtensions baseExtSet
   in do
        initialLanguagePragmas <- first (,baseParseExts) (extractLanguagePragmas sourceTag baseExts input)
        let initialParseExts =
              applyImpliedExtensions
                (List.foldl' applyExtensionSetting baseExtSet initialLanguagePragmas)
            inputForParse =
              if EnumSet.member GHC.Cpp initialParseExts
                then resultOutput (preprocessForParserWithoutIncludes sourceTag input)
                else input
            languagePragmas =
              if EnumSet.member GHC.Cpp initialParseExts
                then nub (initialLanguagePragmas <> moduleHeaderExtensionSettings inputForParse)
                else initialLanguagePragmas
            parseExts =
              applyImpliedExtensions
                (List.foldl' applyExtensionSetting baseExtSet languagePragmas)
            opts = mkParserOpts parseExts emptyDiagOpts False False False True
            buffer = stringToStringBuffer (T.unpack inputForParse)
            start = mkRealSrcLoc (mkFastString sourceTag) 1 1
        case catchPureExceptionText $ case unP parseModule (initParserState opts buffer start) of
          POk st modu ->
            case firstSignificantTokenAfterModule st of
              Right tok ->
                case unLoc tok of
                  ITeof -> Right (languagePragmas, unLoc modu)
                  _ ->
                    Left
                      ( "GHC parser accepted module prefix but left trailing token: "
                          <> T.pack (show tok),
                        parseExts
                      )
              Left lexErr ->
                Left
                  ( "GHC lexer failed while checking for trailing tokens: "
                      <> lexErr,
                    parseExts
                  )
          PFailed st ->
            let rendered = showSDocUnsafe (pprMessages NoDiagnosticOpts (getPsErrorMessages st))
             in Left (T.pack rendered, parseExts) of
          Left err -> Left ("GHC parser exception: " <> err, parseExts)
          Right result -> result

applyExtensionSetting :: EnumSet.EnumSet GHC.Extension -> Ast.ExtensionSetting -> EnumSet.EnumSet GHC.Extension
applyExtensionSetting exts setting =
  case setting of
    Ast.EnableExtension ext ->
      maybe exts (`EnumSet.insert` exts) (toGhcExtension ext)
    Ast.DisableExtension ext ->
      maybe exts (`EnumSet.delete` exts) (toGhcExtension ext)

extractLanguagePragmas :: String -> [GHC.Extension] -> Text -> Either Text [Ast.ExtensionSetting]
extractLanguagePragmas sourceTag baseExts input =
  let headerPragmas = moduleHeaderExtensionSettings input
      headerExts =
        applyImpliedExtensions
          (List.foldl' applyExtensionSetting (EnumSet.fromList baseExts :: EnumSet.EnumSet GHC.Extension) headerPragmas)
      inputForOptions =
        if EnumSet.member GHC.Cpp headerExts
          then resultOutput (preprocessForParserWithoutIncludes sourceTag input)
          else input
      buffer = stringToStringBuffer (T.unpack inputForOptions)
      baseOpts =
        mkParserOpts
          (EnumSet.fromList baseExts :: EnumSet.EnumSet GHC.Extension)
          emptyDiagOpts
          False
          False
          False
          True
   in case catchPureExceptionText
        ( let (_warns, rawOptions) = getOptions baseOpts supportedLanguagePragmas buffer sourceTag
              optionPragmas = mapMaybe optionToLanguagePragma rawOptions
              pragmas = nub (headerPragmas <> optionPragmas)
           in length pragmas `seq` pragmas
        ) of
        Left err -> Left ("GHC option parsing exception: " <> err)
        Right pragmas -> Right pragmas

firstSignificantTokenAfterModule :: PState -> Either Text (Located Token)
firstSignificantTokenAfterModule st =
  case unP (lexer False pure) st of
    POk st' tok
      | isIgnorableToken (unLoc tok) -> firstSignificantTokenAfterModule st'
      | otherwise -> Right tok
    PFailed st' ->
      Left (T.pack (showSDocUnsafe (pprMessages NoDiagnosticOpts (getPsErrorMessages st'))))

isIgnorableToken :: Token -> Bool
isIgnorableToken tok =
  case tok of
    ITdocComment {} -> True
    ITdocOptions {} -> True
    ITlineComment {} -> True
    ITblockComment {} -> True
    _ -> False

optionToLanguagePragma :: GenLocated l String -> Maybe Ast.ExtensionSetting
optionToLanguagePragma locatedOpt =
  let opt = T.pack (unLoc locatedOpt)
   in case T.stripPrefix "-X" opt of
        Just pragmaName | not (T.null pragmaName) -> Ast.parseExtensionSettingName pragmaName
        _ -> Nothing

applyImpliedExtensions :: EnumSet.EnumSet GHC.Extension -> EnumSet.EnumSet GHC.Extension
applyImpliedExtensions = go
  where
    go exts =
      let next = List.foldl' apply exts impliedXFlags
       in if EnumSet.toList next == EnumSet.toList exts then exts else go next

    apply exts (enabled, implied)
      | EnumSet.member enabled exts =
          case implied of
            DynFlags.On ext -> EnumSet.insert ext exts
            DynFlags.Off ext -> EnumSet.delete ext exts
      | otherwise = exts

supportedLanguagePragmas :: [String]
supportedLanguagePragmas =
  [ "CPP",
    "Haskell98",
    "Haskell2010",
    "GHC2021",
    "GHC2024",
    "Safe",
    "Trustworthy",
    "Unsafe",
    "Rank2Types",
    "PolymorphicComponents",
    "GeneralisedNewtypeDeriving",
    "NoGeneralisedNewtypeDeriving"
  ]
    <> concatMap (includeNegative . show) ([minBound .. maxBound] :: [GHC.Extension])
  where
    includeNegative extName =
      case extName of
        "Cpp" -> [extName, "NoCPP", "NoCpp"]
        _ -> [extName, "No" <> extName]

catchPureExceptionText :: a -> Either Text a
catchPureExceptionText value =
  unsafePerformIO $ do
    (Right <$> evaluate value)
      `catch` \(err :: SourceError) ->
        pure (Left (T.pack (displayException err)))
{-# NOINLINE catchPureExceptionText #-}

oracleParsesModuleWithNames :: [String] -> Maybe String -> Text -> Bool
oracleParsesModuleWithNames = oracleParsesModuleWithNamesAt "oracle"

oracleParsesModuleWithNamesAt :: String -> [String] -> Maybe String -> Text -> Bool
oracleParsesModuleWithNamesAt sourceTag extNames langName input =
  case oracleDetailedParsesModuleWithNamesAt sourceTag extNames langName input of
    Left _ -> False
    Right _ -> True

oracleDetailedParsesModuleWithNames :: [String] -> Maybe String -> Text -> Either Text ()
oracleDetailedParsesModuleWithNames = oracleDetailedParsesModuleWithNamesAt "oracle"

oracleDetailedParsesModuleWithNamesAt :: String -> [String] -> Maybe String -> Text -> Either Text ()
oracleDetailedParsesModuleWithNamesAt sourceTag extNames langName input =
  let extSettings = mapMaybe (Ast.parseExtensionSettingName . T.pack) extNames
      langExts = maybe [] languageExtensions langName
      allExts = EnumSet.toList (List.foldl' applyExtensionSetting (EnumSet.fromList langExts) extSettings)
   in case parseWithGhcWithExtensionsDetailed sourceTag allExts input of
        Left (err, parseExts) ->
          let extList = T.pack (show extNames)
              parseExtList = T.pack (show (EnumSet.toList parseExts))
              langInfo = maybe "" (\l -> " Language: " <> T.pack l) langName
           in Left (err <> "\n(Extensions: " <> extList <> langInfo <> " Effective parse extensions: " <> parseExtList <> ")")
        Right _ -> Right ()

oracleModuleParseErrorWithNames :: [String] -> Maybe String -> Text -> Either Text Text
oracleModuleParseErrorWithNames = oracleModuleParseErrorWithNamesAt "oracle"

oracleModuleParseErrorWithNamesAt :: String -> [String] -> Maybe String -> Text -> Either Text Text
oracleModuleParseErrorWithNamesAt sourceTag extNames langName input =
  let extSettings = mapMaybe (Ast.parseExtensionSettingName . T.pack) extNames
      langExts = maybe [] languageExtensions langName
      allExts = EnumSet.toList (List.foldl' applyExtensionSetting (EnumSet.fromList langExts) extSettings)
   in case parseWithGhcWithExtensionsDetailed sourceTag allExts input of
        Left (err, _parseExts) -> Right err
        Right _ -> Left "GHC parser accepted the input"

toGhcExtension :: Ast.Extension -> Maybe GHC.Extension
toGhcExtension ext =
  case ext of
    Ast.NondecreasingIndentation ->
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

    toGhcExtensionName Ast.CPP = "Cpp"
    toGhcExtensionName Ast.GeneralizedNewtypeDeriving = "GeneralisedNewtypeDeriving"
    toGhcExtensionName Ast.SafeHaskell = "Safe"
    toGhcExtensionName Ast.Trustworthy = "Trustworthy"
    toGhcExtensionName Ast.UnsafeHaskell = "Unsafe"
    toGhcExtensionName Ast.Rank2Types = "RankNTypes"
    toGhcExtensionName Ast.PolymorphicComponents = "RankNTypes"
    toGhcExtensionName other = T.unpack (Ast.extensionName other)

fromGhcExtension :: GHC.Extension -> Maybe Ast.Extension
fromGhcExtension ghcExt = Ast.parseExtensionName (T.pack (show ghcExt))

-- | Convert a list of extension names (from cabal files) to GHC extensions.
-- Handles both positive (e.g., "UnicodeSyntax") and negative (e.g., "NoUnicodeSyntax") extension names.
-- The language parameter can be used to include base language extensions (e.g., "Haskell2010").
extensionNamesToGhcExtensions :: [String] -> Maybe String -> [GHC.Extension]
extensionNamesToGhcExtensions extNames langName =
  let extSettings = mapMaybe (Ast.parseExtensionSettingName . T.pack) extNames
      langExts = maybe [] languageExtensions langName
   in EnumSet.toList (List.foldl' applyExtensionSetting (EnumSet.fromList langExts) extSettings)

-- | Convert a list of extension names (from cabal files) directly to AIHC parser extensions.
-- This extracts enabled extensions from ExtensionSettings, applying enable/disable semantics.
extensionNamesToParserExtensions :: [String] -> [Ast.Extension]
extensionNamesToParserExtensions extNames =
  let extSettings = mapMaybe (Ast.parseExtensionSettingName . T.pack) extNames
   in applyExtensionSettings extSettings []
  where
    applyExtensionSettings [] acc = acc
    applyExtensionSettings (setting : rest) acc =
      case setting of
        Ast.EnableExtension ext -> applyExtensionSettings rest (ext : filter (/= ext) acc)
        Ast.DisableExtension ext -> applyExtensionSettings rest (filter (/= ext) acc)

languageExtensions :: String -> [GHC.Extension]
languageExtensions lang =
  case parseLanguage lang of
    Just ghcLanguage -> DynFlags.languageExtensions (Just ghcLanguage)
    Nothing -> []

parseLanguage :: String -> Maybe DynFlags.Language
parseLanguage lang =
  case lang of
    "Haskell98" -> Just DynFlags.Haskell98
    "Haskell2010" -> Just DynFlags.Haskell2010
    "GHC2021" -> Just DynFlags.GHC2021
    "GHC2024" -> Just DynFlags.GHC2024
    _ -> Nothing
